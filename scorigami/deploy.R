# Deployment Script for shinyapps.io
# Run this script to deploy the Premier League Scorigami app

library(rsconnect)

# --- Configuration ---
app_name <- "premier-league-scorigami"  # Change this to your preferred app name
account_name <- NULL  # Will use your default account, or set explicitly

# --- Pre-deployment checks ---
cat("🔍 Running pre-deployment checks...\n\n")

# Check required files exist
required_files <- c("app.R", "pl_history.rds")
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(sprintf("❌ Missing required files: %s", paste(missing_files, collapse = ", ")))
}

cat("✅ All required files present\n")

# Check data freshness
pl_data <- readRDS("pl_history.rds")
latest_date <- max(pl_data$Date, na.rm = TRUE)
days_old <- as.numeric(Sys.Date() - latest_date)

cat(sprintf("📊 Data status:\n"))
cat(sprintf("   - Total matches: %s\n", format(nrow(pl_data), big.mark = ",")))
cat(sprintf("   - Latest match: %s (%d days ago)\n", latest_date, days_old))

if (days_old > 14) {
  warning("⚠️  Data is more than 14 days old. Consider running update_data.R first.")
  response <- readline("Continue with deployment? (y/n): ")
  if (tolower(response) != "y") {
    stop("Deployment cancelled by user")
  }
}

# --- Account Setup Check ---
cat("\n🔑 Checking rsconnect account setup...\n")

accounts <- rsconnect::accounts()

if (nrow(accounts) == 0) {
  cat("\n❌ No shinyapps.io account configured!\n\n")
  cat("📝 To set up your account, run:\n")
  cat("   rsconnect::setAccountInfo(name='your-account-name',\n")
  cat("                             token='your-token',\n")
  cat("                             secret='your-secret')\n\n")
  cat("Get your credentials from: https://www.shinyapps.io/admin/#/tokens\n")
  stop("Please configure your shinyapps.io account first")
}

cat(sprintf("✅ Found account(s): %s\n", paste(accounts$name, collapse = ", ")))

if (is.null(account_name)) {
  account_name <- accounts$name[1]
  cat(sprintf("   Using account: %s\n", account_name))
}

# --- Deploy ---
cat("\n🚀 Starting deployment...\n")
cat(sprintf("   App name: %s\n", app_name))
cat(sprintf("   Account: %s\n", account_name))
cat(sprintf("   URL will be: https://%s.shinyapps.io/%s\n\n", account_name, app_name))

response <- readline("Proceed with deployment? (y/n): ")

if (tolower(response) == "y") {

  deployApp(
    appName = app_name,
    account = account_name,
    forceUpdate = TRUE,
    launch.browser = TRUE
  )

  cat("\n✅ Deployment complete!\n")
  cat(sprintf("🌐 Your app is live at: https://%s.shinyapps.io/%s\n", account_name, app_name))

} else {
  cat("Deployment cancelled.\n")
}
