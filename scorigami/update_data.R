#!/usr/bin/env Rscript
# Data update for Premier League Scorigami. Run weekly (GitHub Actions) or by hand.
#
# Each run refetches the PREVIOUS and CURRENT seasons from football-data.co.uk and
# replaces them wholesale, so a season's final weeks are picked up even after the
# August rollover. The current season is worked out from today's date.
# Safety checks stop the run (and leave pl_history.rds untouched) if anything looks off.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

history_file <- "pl_history.rds"

season_of <- function(date) ifelse(month(date) >= 8, year(date), year(date) - 1)
current_season <- season_of(Sys.Date())
seasons_to_fetch <- c(current_season - 1, current_season)

fetch_season <- function(start_year) {
  code <- paste0(substr(start_year, 3, 4), substr(start_year + 1, 3, 4))
  url <- paste0("https://www.football-data.co.uk/mmz4281/", code, "/E0.csv")
  cat(sprintf("  Fetching %d/%d from %s\n", start_year, start_year + 1, url))
  df <- tryCatch(read_csv(url, show_col_types = FALSE, progress = FALSE), error = function(e) {
    cat(sprintf("  ⚠️  Could not fetch: %s\n", conditionMessage(e))); NULL
  })
  if (is.null(df) || !nrow(df)) return(NULL)
  df |>
    transmute(Date = as_date(parse_date_time(Date, orders = c("dmy", "dmY"))),
              Home = HomeTeam, Away = AwayTeam,
              HG = as.integer(FTHG), AG = as.integer(FTAG)) |>
    filter(!is.na(Date), !is.na(HG), !is.na(AG)) |>
    filter(season_of(Date) == start_year)
}

finish <- function(d) {
  d |>
    mutate(Score = paste0(HG, "-", AG),
           HomeTeam = Home, AwayTeam = Away, FTHG = HG, FTAG = AG,   # columns older app versions read
           Date_Parsed = Date,
           season_year = season_of(Date),
           Season = paste0(season_year, "/", season_year + 1)) |>
    arrange(Date, Home)
}

cat("🔄 Premier League data update\n")
old <- if (file.exists(history_file)) readRDS(history_file) else tibble()
if (nrow(old)) {
  old <- old |> mutate(Date = as_date(if ("Date_Parsed" %in% names(old)) Date_Parsed else Date))
  cat(sprintf("📂 %s matches on file, latest %s\n", format(nrow(old), big.mark = ","), max(old$Date)))
}

fresh <- lapply(seasons_to_fetch, fetch_season)
names(fresh) <- seasons_to_fetch
fetched <- bind_rows(fresh)
if (!nrow(fetched)) { cat("⚠️  Nothing fetched; data left unchanged.\n"); quit(status = 0) }

# Replace only the seasons we successfully fetched
replaced <- as.integer(names(Filter(function(x) !is.null(x) && nrow(x) > 0, fresh)))
keep <- if (nrow(old)) old |> filter(!season_of(Date) %in% replaced) |> select(Date, Home, Away, HG, AG) else tibble()
updated <- finish(bind_rows(keep, fetched))

# ---- Safety checks -------------------------------------------------------------
counts <- updated |> count(season_year)
complete <- counts |> filter(season_year < current_season)
expected <- ifelse(complete$season_year <= 1994, 462, 380)
bad <- complete[complete$n != expected, ]
dups <- updated |> count(Date, Home, Away) |> filter(n > 1)
problems <- c(
  if (nrow(bad)) paste("unexpected match counts:", paste0(bad$season_year, "=", bad$n, collapse = ", ")),
  if (nrow(dups)) paste(nrow(dups), "duplicate matches"),
  if (nrow(old) && nrow(updated) < nrow(old)) sprintf("would shrink the data (%d -> %d)", nrow(old), nrow(updated))
)
if (length(problems)) {
  cat("❌ Not saving:", paste(problems, collapse = "; "), "\n")
  quit(status = 1)
}

if (nrow(old) && identical(nrow(old), nrow(updated)) && max(old$Date) == max(updated$Date)) {
  cat("ℹ️  No new matches. Data is up to date.\n")
} else {
  saveRDS(updated, history_file)
  cat(sprintf("✅ Saved %s matches (+%d), latest %s\n", format(nrow(updated), big.mark = ","),
              nrow(updated) - nrow(old), max(updated$Date)))
}
