# The Shape of Sport — builds data/athletes.rds from the ESPN roster snapshot.
# Input: data/snapshot-2026-10-17/ (athletes_height_weight.csv, coverage.csv, manifest.json), collected by fetch_data.py.
# Rows are listed roster measurements: nothing is estimated, jittered or dropped beyond the snapshot's own screens.

suppressPackageStartupMessages({ library(data.table); library(stringi); library(jsonlite) })

SNAP <- "data/snapshot-2026-10-17"
a <- fread(file.path(SNAP, "athletes_height_weight.csv"), encoding = "UTF-8", colClasses = list(character = "athlete_id"))
coverage <- fread(file.path(SNAP, "coverage.csv"))
manifest <- fromJSON(file.path(SNAP, "manifest.json"))

stopifnot(nrow(a) == manifest$usable_athlete_league_records, all(a$measurement_status == "usable"),
          !anyNA(a$height_cm), !anyNA(a$weight_kg), !anyDuplicated(a[, .(league, athlete_id)]))

SPORT_KEY <- c("American football" = "football", "Basketball" = "basketball", "Baseball" = "baseball",
               "Ice hockey" = "hockey", "Soccer" = "soccer")
LEAGUES <- c("NFL", "NBA", "MLB", "NHL", "MLS", "Premier League")
stopifnot(all(a$sport %in% names(SPORT_KEY)), all(a$league %in% LEAGUES))

p <- a[, .(id = athlete_id, name = athlete_name, sport = unname(SPORT_KEY[sport]), league, team = gsub("|", " / ", team, fixed = TRUE),
           position, code = position_code, h = round(height_cm, 2), w = round(weight_kg, 2), hin = height_in, lb = weight_lb,
           url = source_player_url, season = trimws(sub(" (English Premier League|MLS)$", "", season)))]
p[, key := paste0(league, ":", id)]
p[, pkey := paste0(sport, ":", code)]                                    # "basketball:G" and "soccer:G" are different positions
p[, search := stri_trans_tolower(stri_trans_general(paste(name, team), "Latin-ASCII"))]

# Percentile within the player's league: the share of the league strictly below, rounded half up
pct_below <- function(x) floor(100 * (frank(x, ties.method = "min") - 1) / length(x) + 0.5)
p[, `:=`(hp = pct_below(h), wp = pct_below(w)), by = league]

p <- p[order(stri_rank(name, opts_collator = stri_opts_collator(locale = "en")), league, id)]    # "Álvaro" sorts with the As
p[, i := .I]
set.seed(2026); p[, z := sample(.N)]                     # a fixed, shuffled drawing order, so no sport always sits on top
# A fixed nudge for drawing each dot, as a share (−100…100) of the spread the app allows. Heights are listed in whole
# inches and weights in whole pounds, and the app keeps every nudge inside that rounding (SPREAD in www/shape.js).
p[, `:=`(jh = sample(-100:100, .N, replace = TRUE), jw = sample(-100:100, .N, replace = TRUE))]

built <- as.Date(substr(manifest$built_at_utc, 1, 10))
cov <- coverage[, .(league, teams = teams_fetched, records = athlete_league_records, usable = usable_height_weight,
                    pct = coverage_pct, excluded = excluded_records)]
excl <- manifest$excluded_reason_combinations

saveRDS(list(players = p[], coverage = cov, snapshot = built, teams = sum(cov$teams), records = sum(cov$records),
             excluded = unlist(excl)), "data/athletes.rds")

cat(sprintf("%s players, %d teams, %d leagues · snapshot %s\n", format(nrow(p), big.mark = ","), sum(cov$teams), uniqueN(p$league), built))
cat(sprintf("height %.1f–%.1f cm · weight %.1f–%.1f kg · %d shared-coordinate groups, largest %d\n",
            min(p$h), max(p$h), min(p$w), max(p$w), p[, .N, by = .(h, w)][N > 1, .N], p[, .N, by = .(h, w)][, max(N)]))
