# Star Streaks: data prep. Every actor's run of consecutive lead roles in Hollywood films since 1960,
# with each film's IMDb rating.
#
# Sources (data/raw, not committed; fetch with get_data.sh):
#   IMDb non-commercial datasets  title.basics, title.ratings, title.principals, name.basics
#   Wikidata (via QLever)          release dates with precision and place, country of origin
#
# Definitions
#   Film:   a Hollywood film released 1960 or later: IMDb titleType "movie", not adult, rated on IMDb, 40+ minutes
#           (the feature-film line; or unknown), not a Documentary, not Animation (voice roles), and a US production
#           (Wikidata country of origin includes the United States). Everything else neither counts nor breaks a run.
#   Lead:   one of the first three names billed. Wikipedia's "Starring" list (poster billing) where the film has
#           an article and its names resolve; otherwise the first three distinct people in IMDb's acting credits.
#   Order:  release date: Wikidata (US release when sane, else earliest), then Freebase, TMDB, Wikipedia;
#           films with only a year sort mid-year and are flagged.
#
# Output: data/streaks.rds  list(films, leads, people, built)

suppressPackageStartupMessages(library(data.table))
setwd("data")
dir.create("build", showWarnings = FALSE)
raw <- function(f) file.path("raw", f)

# ---- films ------------------------------------------------------------------------------------
if (!file.exists("build/movies.tsv"))
  system("gzip -cd raw/title.basics.tsv.gz | awk -F'\\t' 'NR==1 || ($2==\"movie\" && $5==\"0\")' > build/movies.tsv")
mv <- fread("build/movies.tsv", quote = "", na.strings = "\\N",
            select = c("tconst", "primaryTitle", "startYear", "runtimeMinutes", "genres"))
rt <- fread(raw("title.ratings.tsv.gz"), quote = "", na.strings = "\\N")
films <- rt[mv, on = "tconst", nomatch = 0]
films <- films[!grepl("Documentary|Animation", genres) & (is.na(runtimeMinutes) | runtimeMinutes >= 40) & !is.na(startYear)]   # 40 min: silent features like Sherlock Jr. ran 45
# Hand exclusions: not new acting roles (each checked on Wikipedia); they neither count nor break a run
EXCLUDE <- c(tt0050243 = "The Chaplin Revue (1959): a compilation of Chaplin shorts from 1918-22")
films <- films[!tconst %in% names(EXCLUDE)]
setnames(films, c("averageRating", "numVotes", "primaryTitle", "startYear", "runtimeMinutes"), c("rating", "votes", "title", "year", "runtime"))
cat(sprintf("films: %s rated feature films\n", format(nrow(films), big.mark = ",")))

# ---- leads: first three distinct people in the acting credits -----------------------------------------
if (!file.exists("build/acting.tsv")) {
  writeLines(mv$tconst, "build/movie_ids.txt")
  system(paste("gzip -cd raw/title.principals.tsv.gz | awk -F'\\t' 'NR==FNR { m[$1]; next }",
               "FNR==1 || (($4==\"actor\" || $4==\"actress\") && ($1 in m) && $2 <= 12) { print $1 \"\\t\" $2 \"\\t\" $3 \"\\t\" $4 }'",
               "build/movie_ids.txt - > build/acting.tsv"))
}
ac <- fread("build/acting.tsv", quote = "")[tconst %in% films$tconst]
ac <- ac[, .(ordering = min(ordering)), by = .(tconst, nconst)]                  # one row per person per film
setorder(ac, tconst, ordering)
ac[, bill := seq_len(.N), by = tconst]
leads <- ac[bill <= 3, .(nconst, tconst, bill, src = "imdb")]

# Wikipedia billing (wiki_infobox.py): the first three names in "Starring", resolved to IMDb people through
# Wikidata (linked names) or by name among the film's credited cast (unlinked names).
if (file.exists("reference/wiki_infobox.tsv")) {
  ib <- fread("reference/wiki_infobox.tsv", quote = "", colClasses = "character", encoding = "UTF-8")[starring != "" & tconst %in% films$tconst]
  st <- ib[, .(item = strsplit(starring, " \u00a6 ", fixed = FALSE)[[1]]), by = tconst][, pos := seq_len(.N), by = tconst]
  st[, `:=`(target = sub("\u2016.*$", "", item), text = sub("^[^\u2016]*\u2016", "", item))]    # split on the first "‖" only
  # Leads: the first three names, unless the poster bills an ensemble alphabetically (five or more names in
  # surname order at the top), in which case nobody is billed above anyone else and the whole block leads
  surname <- function(x) { x <- sub(",? +(Jr\\.?|Sr\\.?|II|III|IV)$", "", trimws(x)); tolower(iconv(sub(".* ", "", x), "UTF-8", "ASCII//TRANSLIT", sub = "")) }
  st[, sn := surname(fifelse(is.na(text) | text == "", target, text))]
  st[, alpha := { ok <- c(TRUE, sn[-1] >= sn[-.N]); ok[is.na(ok)] <- FALSE; p <- if (all(ok)) .N else which(!ok)[1] - 1L; if (p >= 5) p else 0L }, by = tconst]
  st <- st[pos <= pmax(3L, alpha)]
  norm_title <- function(x) { x <- gsub("_", " ", sub("#.*$", "", x)); x <- trimws(x); paste0(toupper(substr(x, 1, 1)), substring(x, 2)) }
  pe <- fread("raw/wd_people_enwiki.tsv", quote = "", col.names = c("nconst", "article"), encoding = "UTF-8")
  pe[, `:=`(nconst = gsub('"', "", nconst), target = norm_title(URLdecode(sub("^<https://en.wikipedia.org/wiki/(.*)>$", "\\1", article))))]
  pe <- pe[, .(nconst = nconst[1]), by = target]
  st[, target := norm_title(target)]
  # follow Wikipedia redirects (wiki_redirects.py) for names that point at one: "Beyonce Knowles" -> "Beyonce"
  rd <- if (file.exists("reference/wiki_redirects.tsv")) fread("reference/wiki_redirects.tsv", quote = "", encoding = "UTF-8")[to != ""] else data.table(from = character(), to = character())
  st[rd, on = .(target = from), target := i.to]
  st <- pe[st, on = "target"]                                                       # 1. linked names
  st[, shown := norm_title(fifelse(is.na(text) | text == "", target, text))]
  st[, shown_rd := shown][rd, on = .(shown_rd = from), shown_rd := i.to]
  credited <- paste(ac$tconst, ac$nconst)
  # 2. unlinked names, looked up as article titles: a director-star is linked once, under "Directed by",
  #    and left unlinked in "Starring". Accepted only if that person is credited in the film.
  u <- pe[st[is.na(nconst), .(tconst, pos, shown_rd)], on = .(target = shown_rd), .(tconst, pos, cand = x.nconst)][!is.na(cand)]
  u <- u[paste(tconst, cand) %in% credited]
  st[u, on = .(tconst, pos), nconst := i.cand]
  # 3. the shown name against the film's credited cast; 4. same surname and first initial, if unique there
  #    (Wikipedia's "Charlie Chaplin" is IMDb's "Charles Chaplin")
  if (!file.exists("build/cast_names.tsv")) {
    writeLines(unique(ac$nconst), "build/cast_ids.txt")
    system("gzip -cd raw/name.basics.tsv.gz | awk -F'\\t' 'NR==FNR { m[$1]; next } ($1 in m) { print $1 \"\\t\" $2 }' build/cast_ids.txt - > build/cast_names.tsv")
  }
  cn <- fread("build/cast_names.tsv", header = FALSE, quote = "", col.names = c("nconst", "name"), encoding = "UTF-8")
  ascii <- function(x) tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = ""))
  key <- function(x) gsub("[^a-z]", "", ascii(x))
  key2 <- function(x) { x <- ascii(sub(",? +(Jr\\.?|Sr\\.?|II|III|IV)$", "", trimws(x))); w <- strsplit(gsub("[^a-z ]", "", x), " +")
                        vapply(w, function(v) { v <- v[nzchar(v)]; if (length(v) < 2) "" else paste(substr(v[1], 1, 1), v[length(v)]) }, "") }
  cast <- cn[ac[tconst %in% st[is.na(nconst), tconst], .(tconst, nconst)], on = "nconst", nomatch = 0]
  cast[, `:=`(k = key(name), k2 = key2(name))]
  st[, `:=`(k = key(shown), k2 = key2(shown))]
  st[is.na(nconst), nconst := cast[st[is.na(nconst)], on = .(tconst, k), mult = "first", x.nconst]]
  c2 <- cast[k2 != "", .(cand = if (uniqueN(nconst) == 1) nconst[1] else NA_character_), by = .(tconst, k2)]
  st[is.na(nconst) & k2 != "", nconst := c2[st[is.na(nconst) & k2 != ""], on = .(tconst, k2), x.cand]]
  cat(sprintf("Starring names resolved: %.1f%% of the first three (%s left)\n", 100 * st[pos <= 3, mean(!is.na(nconst))], format(st[pos <= 3, sum(is.na(nconst))], big.mark = ",")))
  fwrite(st[pos <= 3 & is.na(nconst), .(tconst, pos, target, shown)], "build/unresolved.tsv", sep = "\t")
  ok <- st[pos <= 3, .(n = .N, resolved = sum(!is.na(nconst))), by = tconst][resolved >= pmin(2, n)]
  wl <- st[tconst %in% ok$tconst & !is.na(nconst), .(nconst, tconst, bill = pos, src = fifelse(alpha > 0, "wikipedia-alpha", "wikipedia"))][, .SD[1], by = .(tconst, nconst)]
  cat(sprintf("alphabetical ensembles: %s films\n", format(uniqueN(st[alpha > 0, tconst]), big.mark = ",")))
  leads <- rbind(leads[!tconst %in% ok$tconst], wl)
  cat(sprintf("billing: Wikipedia for %s films, IMDb for %s\n", format(uniqueN(wl$tconst), big.mark = ","),
              format(uniqueN(leads[src == "imdb", tconst]), big.mark = ",")))
}

# ---- release dates (Wikidata) ---------------------------------------------------------------------
wd <- fread(raw("wd_dates_full.tsv"), quote = "", col.names = c("tconst", "d", "prec", "rank", "place"))
wd[, tconst := gsub('"', "", tconst)]
wd <- wd[!grepl("Deprecated", rank) & prec >= 10]                                  # day or month precision only
wd[, date := as.IDate(substr(d, 1, 10))][prec == 10, date := date + 14L]          # month only: mid-month
first <- wd[, .(first = min(date)), by = tconst]
us <- wd[grepl("/Q30>$", place), .(us = min(date)), by = tconst]
dates <- us[first, on = "tconst"]
dates[, date := fifelse(!is.na(us) & us >= first & us <= first + 548L, us, first)]  # US release, if sane
dates[, src := "wikidata"]
# Fallbacks for films Wikidata only dates to the year: Freebase (via the CMU Movie Summary Corpus,
# linked through Wikidata's Freebase ID) and the 2016 TMDB dump. 1 January is a placeholder there.
fb <- fread(raw("wd_freebase.tsv"), quote = "", col.names = c("tconst", "fb"))[, lapply(.SD, function(x) gsub('"', "", x))]
cmu <- fread("~/Documents/R/podcast/MovieSummaries/movie.metadata.tsv", header = FALSE, quote = "", select = c(2, 4), col.names = c("fb", "d"))
cmu <- cmu[nchar(d) %in% c(7, 10)][, date := as.IDate(fifelse(nchar(d) == 7, paste0(d, "-15"), d))][!is.na(date) & format(date, "%m-%d") != "01-01"]
cmu <- fb[cmu, on = "fb", nomatch = 0][, .(date = min(date)), by = tconst][, src := "freebase"]
tm <- fread(raw("tmdb-movies.csv"), select = c("imdb_id", "release_date", "release_year"))[imdb_id != ""]
tm[, date := as.IDate(as.Date(release_date, "%m/%d/%y"))]
tm[!is.na(date) & year(date) > release_year, date := as.IDate(sprintf("%d-%s", year(date) - 100L, format(date, "%m-%d")))]   # "1/1/65" is 1965
tm <- tm[!is.na(date) & format(date, "%m-%d") != "01-01", .(date = min(date)), by = .(tconst = imdb_id)][, src := "tmdb"]
# Wikipedia infobox dates (wiki_infobox.py)
wk <- if (file.exists("reference/wiki_infobox.tsv")) fread("reference/wiki_infobox.tsv", quote = "", colClasses = "character", encoding = "UTF-8") else data.table(tconst = character(), date = character())
wk <- wk[date != "", .(tconst, date = as.IDate(date), src = "wikipedia")][!is.na(date)]
dates <- rbindlist(list(dates[!is.na(date), .(tconst, date, src)], cmu, tm, wk))
films <- films[, .(tconst, year)][dates, on = "tconst", nomatch = 0][abs(year(date) - year) <= 1][   # far from IMDb's year: a re-release
  , .SD[1], by = tconst, .SDcols = c("date", "src")][films, on = "tconst"]          # first source in priority order
films[, dated := !is.na(date)][is.na(src), src := "year"]
films[is.na(date), date := as.IDate(sprintf("%d-07-01", year))]

# ---- Hollywood since 1960: US productions (Wikidata country of origin includes the United States) -------------
cty <- fread(raw("wd_countries.tsv"), quote = "", col.names = c("tconst", "c"))[, tconst := gsub('"', "", tconst)]
films <- films[year >= 1960 & tconst %in% cty[grepl("/Q30>$", c), tconst]]
leads <- leads[tconst %in% films$tconst]
cat(sprintf("Hollywood since 1960: %s films\n", format(nrow(films), big.mark = ",")))

# ---- keep actors who could hold a streak: 3+ lead roles in films with 1,000+ votes -------------------------
leads <- films[, .(tconst, votes)][leads, on = "tconst"]
keep <- leads[votes >= 1000, .N, by = nconst][N >= 3, nconst]
leads <- leads[nconst %in% keep, .(nconst, tconst, bill, src)]
films <- films[tconst %in% leads$tconst]

# ---- names ------------------------------------------------------------------------------------------
writeLines(keep, "build/people_ids.txt")
system("gzip -cd raw/name.basics.tsv.gz | awk -F'\\t' 'NR==FNR { m[$1]; next } ($1 in m) { print $1 \"\\t\" $2 \"\\t\" $3 \"\\t\" $4 }' build/people_ids.txt - > build/people.tsv")
people <- fread("build/people.tsv", header = FALSE, quote = "", na.strings = "\\N", col.names = c("nconst", "name", "born", "died"))

# Wikidata can point at IMDb person ids that have since been merged away; drop those credits
leads <- leads[nconst %in% people$nconst]
films <- films[tconst %in% leads$tconst]

# compact ids for the app
films[, fid := .I]; people[, pid := .I]
leads <- people[, .(nconst, pid)][films[, .(tconst, fid)][leads, on = "tconst"], on = "nconst"][, .(pid, fid, bill, bill_src = src)]
films <- films[, .(fid, tconst, title, year, date, dated, date_src = src, rating, votes, genres, runtime)]
people <- people[, .(pid, nconst, name, born, died)]
saveRDS(list(films = films, leads = leads, people = people, built = Sys.Date()), "streaks.rds", compress = "xz")
cat(sprintf("saved: %s films, %s lead roles, %s actors | %.0f%% of films with 1k+ votes have a real release date\n",
            format(nrow(films), big.mark = ","), format(nrow(leads), big.mark = ","), format(nrow(people), big.mark = ","),
            100 * mean(films[votes >= 1000]$dated)))
print(films[votes >= 1000, .N, by = date_src])

# ---- articles for wiki_infobox.py: every film in the app with an English Wikipedia article ---------------------
w <- fread("raw/wd_enwiki.tsv", quote = "", col.names = c("tconst", "article"))[, tconst := gsub('"', "", tconst)]
need <- w[tconst %in% films$tconst][, .(title = URLdecode(sub("^<https://en.wikipedia.org/wiki/(.*)>$", "\\1", article[1]))), by = tconst]
need <- films[, .(tconst, votes)][need, on = "tconst"][order(-votes), .(tconst, title)]
fwrite(need, "build/wiki_need.tsv", sep = "\t")
done <- if (file.exists("reference/wiki_infobox.tsv")) nrow(fread("reference/wiki_infobox.tsv", select = 1)) else 0
cat(sprintf("%s films have a Wikipedia article; %s read so far (run wiki_infobox.py, then prep.R again)\n",
            format(nrow(need), big.mark = ","), format(done, big.mark = ",")))
