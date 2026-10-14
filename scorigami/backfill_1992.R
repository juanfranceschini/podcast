#!/usr/bin/env Rscript
# One-off: add the Premier League's first season (1992-93) to pl_history.rds.
# football-data.co.uk starts in 1993-94, so without this a "first ever" scoreline claim could be wrong.
# Source: engsoccerdata (github.com/jalapic/engsoccerdata), England tier 1, Season 1992.
# Refuses to save unless the season checks out: 462 matches, 22 clubs, and the final table's top three.

suppressPackageStartupMessages({ library(dplyr); library(readr); library(lubridate) })

history_file <- "pl_history.rds"
src <- "https://raw.githubusercontent.com/jalapic/engsoccerdata/master/data-raw/england.csv"
season_of <- function(date) ifelse(month(date) >= 8, year(date), year(date) - 1)

names_map <- c(
  "Arsenal" = "Arsenal", "Aston Villa" = "Aston Villa", "Blackburn Rovers" = "Blackburn", "Chelsea" = "Chelsea",
  "Coventry City" = "Coventry", "Crystal Palace" = "Crystal Palace", "Everton" = "Everton", "Ipswich Town" = "Ipswich",
  "Leeds United" = "Leeds", "Liverpool" = "Liverpool", "Manchester City" = "Man City", "Manchester United" = "Man United",
  "Middlesbrough" = "Middlesbrough", "Norwich City" = "Norwich", "Nottingham Forest" = "Nott'm Forest",
  "Oldham Athletic" = "Oldham", "Queens Park Rangers" = "QPR", "Sheffield United" = "Sheffield United",
  "Sheffield Wednesday" = "Sheffield Weds", "Southampton" = "Southampton", "Tottenham Hotspur" = "Tottenham",
  "Wimbledon" = "Wimbledon")

old <- readRDS(history_file)
if (any(season_of(as_date(old$Date)) == 1992)) { cat("ℹ️  1992-93 is already in", history_file, "\n"); quit(status = 0) }

raw <- read_csv(src, show_col_types = FALSE, progress = FALSE) |> filter(Season == 1992, tier == 1)
s <- raw |> transmute(Date = as_date(Date), Home = unname(names_map[home]), Away = unname(names_map[visitor]),
                      HG = as.integer(hgoal), AG = as.integer(vgoal))

# ---- checks ------------------------------------------------------------------------------
pts <- bind_rows(s |> transmute(team = Home, p = case_when(HG > AG ~ 3, HG == AG ~ 1, TRUE ~ 0)),
                 s |> transmute(team = Away, p = case_when(AG > HG ~ 3, HG == AG ~ 1, TRUE ~ 0))) |>
  group_by(team) |> summarise(pts = sum(p), .groups = "drop") |> arrange(desc(pts))
problems <- c(
  if (nrow(s) != 462) sprintf("%d matches, expected 462", nrow(s)),
  if (anyNA(s$Home) || anyNA(s$Away)) paste("unmapped clubs:", paste(unique(c(raw$home[is.na(s$Home)], raw$visitor[is.na(s$Away)])), collapse = ", ")),
  if (length(unique(c(s$Home, s$Away))) != 22) "expected 22 clubs",
  if (!identical(head(pts$team, 3), c("Man United", "Aston Villa", "Norwich")) || !identical(head(pts$pts, 3), c(84, 74, 72)))
    paste("final table does not match (top three:", paste0(head(pts$team, 3), " ", head(pts$pts, 3), collapse = ", "), ")"),
  if (any(season_of(s$Date) != 1992)) "dates outside 1992-93")
if (length(problems)) { cat("❌ Not saving:", paste(problems, collapse = "; "), "\n"); quit(status = 1) }
cat(sprintf("✓ 1992-93: %d matches, 22 clubs, top three %s\n", nrow(s), paste0(head(pts$team, 3), " ", head(pts$pts, 3), collapse = ", ")))

# ---- merge, with the same columns update_data.R writes -------------------------------------
updated <- bind_rows(s, old |> transmute(Date = as_date(Date), Home, Away, HG, AG)) |>
  mutate(Score = paste0(HG, "-", AG), HomeTeam = Home, AwayTeam = Away, FTHG = HG, FTAG = AG,
         Date_Parsed = Date, season_year = season_of(Date), Season = paste0(season_year, "/", season_year + 1)) |>
  arrange(Date, Home)
saveRDS(updated, history_file)
cat(sprintf("✅ Saved %s matches (%s seasons), from %s\n", format(nrow(updated), big.mark = ","), length(unique(updated$season_year)), min(updated$Date)))
