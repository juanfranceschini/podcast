# Premier League Scorigami bot

After every Premier League match, the bot posts to X whether the final score is a **scorigami** (a scoreline never seen in Premier League history), a **team scorigami** (the first time a club has had that result at that venue), or when the same score last happened.

```
⚽ Coventry 4–1 Newcastle
🟩 No Scorigami. 4–1 has now happened 231 times, most recently Leeds 4–1 Newcastle (14 Sep 2026).

🏠 Coventry: last 4–1 home win vs Sheffield Wednesday, 6 May 2000
✈️ Newcastle: last 4–1 away loss at Leeds, 14 Sep 2026
```

🟪 is the signature: in the app's grid a magenta square is a score that has happened exactly once, so every scorigami lights up a new square.

## How it runs
- `.github/workflows/scorigami-bot.yml` runs every 10 minutes from 12:00 to 23:59 UTC.
- Each run asks football-data.org for finished Premier League matches and posts each new one once. It records what it posted in `state.json` and the results in `live_results.csv`.
- The weekly `update-data.yml` refreshes `pl_history.rds` and `pl_history.csv`, and drops live results once football-data.co.uk has them.
- Posts carry no links: on X's pay-per-use API a post with a URL costs about $0.20 instead of about $0.015. Put the app link in the account bio.

## Setup (once)
1. **X account and API keys.**
   - Create the account (e.g. @PL_Scorigami), then sign in at developer.x.com with it.
   - Create a project and an app. Under *User authentication settings*, set app permissions to **Read and Write**.
   - Under *Keys and tokens*, generate the **API Key and Secret** and the **Access Token and Secret**. Generate the access token *after* setting Read and Write.
   - Add a small amount of API credit. X has had no free tier for new developers since February 2026. Expect roughly $6 per season (about 380 posts).
2. **football-data.org.** Register for the free plan and copy your API token.
3. **GitHub secrets.** In github.com/juanfranceschini/podcast, go to *Settings → Secrets and variables → Actions → New repository secret* and add:
   `FOOTBALL_DATA_TOKEN`, `X_API_KEY`, `X_API_SECRET`, `X_ACCESS_TOKEN`, `X_ACCESS_SECRET`.
4. **Re-enable the weekly data update** in the *Actions* tab ("Update Premier League Data" → *Enable workflow*).

## Going live, safely
1. **Dry run.** *Actions → Scorigami bot → Run workflow*, mode `dry-run`. The run log shows the posts it would make for recent matches; nothing is posted.
2. **Bootstrap.** Run it once with mode `bootstrap`. This marks recent matches as already handled, so going live doesn't post old results.
3. **Live.** In *Settings → Secrets and variables → Actions → Variables*, add `SCORIGAMI_BOT_MODE` = `live`. Scheduled runs post from then on. Delete the variable (or set it to `dry-run`) to pause.

## When something needs you
- **"Unknown team name(s)"** in a run log (usually a newly promoted club): add a row to `teams.csv`. Use the football-data.org name, the name used in `pl_history.csv`, and the name to show in posts.
- **X API errors:** the match stays unposted and is retried on the next run. A post X rejects as a duplicate counts as done.
- **Anything older than about a day** is marked as handled without posting, so an outage never floods the feed.

## Test offline
```
python bot/scorigami_bot.py --replay 2023-09-24     # Sheffield United 0–8 Newcastle → 🚨🟪 SCORIGAMI
```
