#!/usr/bin/env Rscript
# Data Update Script for Premier League Scorigami
# Fetches current season data and updates the cache
# Run this weekly to keep data fresh

library(tidyverse)
library(lubridate)

cat("🔄 Starting data update...\n")

# --- Configuration ---
current_year <- 2025  # Update this each August for new season
history_file <- "pl_history.rds"
backup_file <- paste0("pl_history_backup_", format(Sys.Date(), "%Y%m%d"), ".rds")

# --- Helper Function ---
fetch_season_data <- function(year) {
  y1 <- substr(year, 3, 4)
  y2 <- substr(year + 1, 3, 4)
  url <- paste0("https://www.football-data.co.uk/mmz4281/", y1, y2, "/E0.csv")

  cat(sprintf("  Fetching %s/%s season from %s\n", year, year + 1, url))

  tryCatch({
    df <- read_csv(url, show_col_types = FALSE) %>%
      mutate(Date = parse_date_time(Date, orders = c("dmy", "dmY", "mdy", "ymd"))) %>%
      transmute(
        Date  = as_date(Date),
        HomeTeam,
        AwayTeam,
        FTHG  = as.integer(FTHG),
        FTAG  = as.integer(FTAG),
        Home  = HomeTeam,
        Away  = AwayTeam,
        HG    = FTHG,
        AG    = FTAG,
        Score = paste0(HG, "-", AG)
      ) %>%
      filter(!is.na(Date), !is.na(HG), !is.na(AG))

    cat(sprintf("  ✅ Successfully fetched %d matches\n", nrow(df)))
    return(df)
  }, error = function(e) {
    cat(sprintf("  ⚠️  Failed to fetch: %s\n", e$message))
    return(NULL)
  })
}

# --- Load existing historical data ---
if (file.exists(history_file)) {
  cat(sprintf("📂 Loading existing data from %s\n", history_file))
  pl_history <- readRDS(history_file)

  # Ensure Date column exists (use Date_Parsed if Date doesn't exist)
  if (!"Date" %in% names(pl_history) && "Date_Parsed" %in% names(pl_history)) {
    pl_history <- pl_history %>% mutate(Date = Date_Parsed)
  }

  # Create backup
  cat(sprintf("💾 Creating backup: %s\n", backup_file))
  saveRDS(pl_history, backup_file)

  # Get the latest date in historical data
  latest_historical_date <- max(pl_history$Date, na.rm = TRUE)
  cat(sprintf("  Latest historical match: %s\n", latest_historical_date))

} else {
  cat("⚠️  No existing history file found. Creating new dataset from scratch...\n")
  pl_history <- tibble()
  latest_historical_date <- as.Date("1993-01-01")
}

# --- Fetch current season ---
cat(sprintf("\n🔽 Fetching current season (%s/%s)...\n", current_year, current_year + 1))
current_season <- fetch_season_data(current_year)

if (!is.null(current_season) && nrow(current_season) > 0) {

  # Check if we have new data
  new_matches <- current_season %>%
    filter(Date > latest_historical_date)

  if (nrow(new_matches) > 0) {
    cat(sprintf("  ✨ Found %d new matches to add\n", nrow(new_matches)))

    # Calculate current season year (e.g., 2024 for the 2024/2025 season)
    current_season_year <- current_year - 1

    # Combine with historical data, removing any duplicates from current season
    updated_data <- pl_history %>%
      # Remove any existing current season data to avoid duplicates
      # Season year logic: if month >= 8, season_year = year; else season_year = year - 1
      filter(!(ifelse(month(Date) >= 8, year(Date), year(Date) - 1) == current_season_year)) %>%
      # Add fresh current season data
      bind_rows(current_season) %>%
      arrange(Date)

    # Add season labels and ensure Date_Parsed exists
    updated_data <- updated_data %>%
      mutate(
        Date_Parsed = Date,
        season_year = ifelse(month(Date) >= 8, year(Date), year(Date) - 1),
        Season = paste0(season_year, "/", season_year + 1)
      )

    # Save updated data
    cat(sprintf("\n💾 Saving updated data (%d total matches)...\n", nrow(updated_data)))
    saveRDS(updated_data, history_file)

    cat("✅ Update complete!\n")
    cat(sprintf("   Total matches: %s\n", format(nrow(updated_data), big.mark = ",")))
    cat(sprintf("   Latest match: %s\n", max(updated_data$Date)))
    cat(sprintf("   New matches added: %d\n", nrow(new_matches)))

  } else {
    cat("  ℹ️  No new matches found. Data is up to date.\n")
  }

} else {
  cat("⚠️  Could not fetch current season data. Historical data remains unchanged.\n")
}

cat("\n🎉 Data update process finished!\n")
