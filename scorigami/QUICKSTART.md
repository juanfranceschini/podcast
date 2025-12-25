# Quick Start Guide 🚀

## Deploy in 5 Minutes

### 1. Install Dependencies
```r
install.packages(c("rsconnect", "shiny", "tidyverse", "lubridate", "showtext", "bslib", "reactable", "htmltools"))
```

### 2. Configure shinyapps.io
```r
# Get token from: https://www.shinyapps.io/admin/#/tokens
rsconnect::setAccountInfo(
  name = 'YOUR-ACCOUNT',
  token = 'YOUR-TOKEN',
  secret = 'YOUR-SECRET'
)
```

### 3. Deploy
```r
source("deploy.R")
```

Done! Your app is now live. 🎉

---

## Set Up Weekly Auto-Updates

### 1. Push to GitHub
```bash
git init
git add .
git commit -m "Initial commit"
git remote add origin https://github.com/YOUR-USERNAME/premier-league-scorigami.git
git push -u origin main
```

### 2. Enable GitHub Actions
1. Go to repo **Settings** → **Actions** → **General**
2. Enable "Read and write permissions"
3. Save

### 3. Done!
Data updates automatically every Sunday at 11 PM GMT.

---

## Weekly Routine (1 minute)

Every Monday morning:
```bash
git pull                # Get updated data
```

```r
source("deploy.R")      # Redeploy app
```

That's it! Your app stays fresh with minimal effort.

---

**See DEPLOYMENT.md for detailed instructions**
