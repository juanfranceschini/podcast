# Premier League Scorigami - Deployment Guide

This guide will help you deploy the Premier League Scorigami app to shinyapps.io and set up automatic weekly data updates.

## 📋 Prerequisites

1. **R and RStudio** installed
2. **GitHub account** (for automated updates)
3. **shinyapps.io account** (free tier works fine)
   - Sign up at: https://www.shinyapps.io/

## 🚀 Part 1: Initial Deployment to shinyapps.io

### Step 1: Install Required Packages

```r
install.packages(c("rsconnect", "shiny", "tidyverse", "lubridate", "showtext", "bslib", "reactable", "htmltools"))
```

### Step 2: Configure shinyapps.io Account

1. Go to https://www.shinyapps.io/admin/#/tokens
2. Click "Show" next to your token
3. Copy the `setAccountInfo` command
4. Run it in R:

```r
rsconnect::setAccountInfo(
  name='your-account-name',
  token='your-token-here',
  secret='your-secret-here'
)
```

### Step 3: Deploy the App

Option A - Using the deployment script:
```r
source("deploy.R")
```

Option B - Manual deployment:
```r
library(rsconnect)
deployApp(
  appName = "premier-league-scorigami",
  forceUpdate = TRUE,
  launch.browser = TRUE
)
```

### Step 4: Configure App Settings (Optional)

1. Go to https://www.shinyapps.io/admin/#/applications
2. Click on your app
3. Recommended settings:
   - **Instance Size**: Small (512MB) - handles up to 100 concurrent users
   - **Max Worker Processes**: 3
   - **Max Connections**: 50
   - **Startup Timeout**: 60 seconds

## 🤖 Part 2: Automated Weekly Updates via GitHub Actions

### Step 1: Initialize Git Repository (if not already done)

```bash
cd /path/to/scorigami
git init
git add .
git commit -m "Initial commit: Premier League Scorigami app"
```

### Step 2: Create GitHub Repository

1. Go to https://github.com/new
2. Create a new repository (e.g., `premier-league-scorigami`)
3. **Important**: Make it public or ensure GitHub Actions is enabled for private repos
4. Don't initialize with README (you already have files)

### Step 3: Push to GitHub

```bash
git remote add origin https://github.com/YOUR-USERNAME/premier-league-scorigami.git
git branch -M main
git push -u origin main
```

### Step 4: Enable GitHub Actions

The workflow file (`.github/workflows/update-data.yml`) is already created. It will:
- Run every Sunday at 11 PM GMT
- Fetch the latest 2025/2026 season data
- Update `pl_history.rds` if new matches are found
- Commit and push changes back to the repository

### Step 5: Configure Repository Settings

1. Go to your repository on GitHub
2. Go to **Settings** → **Actions** → **General**
3. Under "Workflow permissions", select:
   - ✅ **Read and write permissions**
   - ✅ **Allow GitHub Actions to create and approve pull requests**
4. Click **Save**

### Step 6: Test the Workflow

You can manually trigger the workflow to test it:

1. Go to **Actions** tab in your repository
2. Click on "Update Premier League Data" workflow
3. Click **Run workflow** → **Run workflow**
4. Watch it run and check the logs

## 🔄 Part 3: Connecting Automated Updates to Shiny App

After GitHub Actions updates the data, you need to redeploy to shinyapps.io. You have two options:

### Option A: Manual Redeployment (Recommended for now)

After the automated update runs (Monday mornings):
```r
# Pull latest changes
git pull

# Redeploy
source("deploy.R")
```

### Option B: Fully Automated with GitHub Actions (Advanced)

Add shinyapps.io deployment to the GitHub Actions workflow:

1. Get your rsconnect credentials:
```r
rsconnect::accounts()
```

2. Add these secrets to your GitHub repository:
   - Go to **Settings** → **Secrets and variables** → **Actions**
   - Add the following secrets:
     - `SHINYAPPS_ACCOUNT`: Your account name
     - `SHINYAPPS_TOKEN`: Your token
     - `SHINYAPPS_SECRET`: Your secret

3. The workflow will automatically deploy after updating data

## 📊 Monitoring & Maintenance

### Check Data Freshness

Run locally to see when data was last updated:
```r
pl_data <- readRDS("pl_history.rds")
max(pl_data$Date)
```

### Manual Data Update

If you need to update immediately (not wait for Sunday):
```r
source("update_data.R")
git add pl_history.rds
git commit -m "Manual data update"
git push
source("deploy.R")
```

### Monitor GitHub Actions

- Check workflow runs: `https://github.com/YOUR-USERNAME/premier-league-scorigami/actions`
- You'll get email notifications if workflows fail

### Monitor Shiny App

- Dashboard: https://www.shinyapps.io/admin/#/dashboard
- Logs: https://www.shinyapps.io/admin/#/application/YOUR-APP-ID

## 🎯 Best Practices

1. **Seasonal Updates**: At the start of each season (August), update `current_year` in both:
   - `app.R` (line 39)
   - `update_data.R` (line 10)

2. **Data Backups**: The update script automatically creates backups before updating

3. **Cost Management**:
   - Free tier includes 25 active hours/month
   - Set app to sleep after inactivity to conserve hours
   - Consider paid tier (~$9/mo) for unlimited hours

4. **Performance**:
   - The cached `pl_history.rds` approach means fast load times
   - Only one API call per session for current season
   - Can handle hundreds of concurrent users

## 🆘 Troubleshooting

### Deployment fails with "package not found"
```r
# Install missing packages then redeploy
install.packages("package-name")
source("deploy.R")
```

### GitHub Actions fails
- Check the Actions tab for error logs
- Common issues:
  - Workflow permissions not set correctly
  - Rate limiting from data source (rare)

### App shows old data
```r
# Pull latest from GitHub
git pull

# Redeploy
source("deploy.R")
```

### Data update script fails
- Check internet connection
- The football-data.co.uk URL might have changed
- Check error messages in update_data.R output

## 📞 Support

- shinyapps.io docs: https://docs.posit.co/shinyapps.io/
- GitHub Actions docs: https://docs.github.com/en/actions
- Issues: Create an issue in your GitHub repository

---

**Your app will be live at**: `https://YOUR-ACCOUNT.shinyapps.io/premier-league-scorigami`
