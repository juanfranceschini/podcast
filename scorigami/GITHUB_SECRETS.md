# GitHub Secrets Setup for Auto-Deployment

The GitHub Actions workflow automatically updates data and deploys to shinyapps.io every Sunday. To enable auto-deployment, you need to add your shinyapps.io credentials as GitHub Secrets.

## Required Secrets

You need to add 3 secrets to your GitHub repository:

1. **SHINYAPPS_ACCOUNT** - Your shinyapps.io account name
2. **SHINYAPPS_TOKEN** - Your shinyapps.io token
3. **SHINYAPPS_SECRET** - Your shinyapps.io secret

## How to Get Your Credentials

### Step 1: Get Token and Secret from shinyapps.io

1. Go to https://www.shinyapps.io/admin/#/tokens
2. Click **"Show"** on an existing token, or click **"Add Token"** to create a new one
3. Copy the **Token** and **Secret** values (you'll need these in the next step)

### Step 2: Add Secrets to GitHub

1. Go to your GitHub repository: https://github.com/juanfranceschini/podcast
2. Click **Settings** (top menu)
3. In the left sidebar, click **Secrets and variables** → **Actions**
4. Click **"New repository secret"** button

Add each secret:

#### Secret 1: SHINYAPPS_ACCOUNT
- **Name:** `SHINYAPPS_ACCOUNT`
- **Value:** `plot-recommendations`
- Click **"Add secret"**

#### Secret 2: SHINYAPPS_TOKEN
- Click **"New repository secret"** again
- **Name:** `SHINYAPPS_TOKEN`
- **Value:** [Paste your token from shinyapps.io]
- Click **"Add secret"**

#### Secret 3: SHINYAPPS_SECRET
- Click **"New repository secret"** again
- **Name:** `SHINYAPPS_SECRET`
- **Value:** [Paste your secret from shinyapps.io]
- Click **"Add secret"**

## Verify Setup

After adding the secrets:

1. Go to **Actions** tab in your GitHub repo
2. Click on **"Update Premier League Data"** workflow
3. Click **"Run workflow"** → **"Run workflow"** (to test manually)
4. Wait for the workflow to complete
5. Check that it updates data AND deploys to shinyapps.io

## How It Works

**Every Sunday at 11 PM GMT**, the workflow automatically:
1. ✅ Fetches latest Premier League data
2. ✅ Updates `pl_history.rds` if new matches found
3. ✅ Commits changes to GitHub
4. ✅ **Deploys updated app to shinyapps.io**

You can also trigger it manually anytime from the Actions tab!

## Troubleshooting

**Workflow fails with "secret not found":**
- Double-check secret names are EXACTLY: `SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`
- Make sure they're added to the **podcast** repository (not your personal account)

**Deployment fails:**
- Verify your token hasn't expired on shinyapps.io
- Create a new token if needed

**Data updates but doesn't deploy:**
- Check the workflow log to see if deployment step ran
- Deployment only happens if new data was found
