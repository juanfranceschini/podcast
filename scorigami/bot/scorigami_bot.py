#!/usr/bin/env python3
"""Premier League Scorigami bot.

After each Premier League match, post to X whether the final score is a scorigami (never seen in
Premier League history), a team scorigami, or when the same score last happened.

  python scorigami_bot.py --mode dry-run     print the posts for newly finished matches, change nothing
  python scorigami_bot.py --mode live        post them to X and remember them in state.json
  python scorigami_bot.py --mode bootstrap   mark recent matches as handled without posting (first run)
  python scorigami_bot.py --replay 2023-09-24   retell a past matchday from history (offline test)

Scorelines are always home-away, like the app. Team lines compare the same result at the same venue.
Secrets (env): FOOTBALL_DATA_TOKEN, X_API_KEY, X_API_SECRET, X_ACCESS_TOKEN, X_ACCESS_SECRET.
"""
import argparse
import csv
import datetime as dt
import json
import os
import re
import sys
from pathlib import Path
from zoneinfo import ZoneInfo

HERE = Path(__file__).resolve().parent
HISTORY = HERE.parent / "pl_history.csv"     # written by update_data.R
LIVE = HERE / "live_results.csv"             # results the bot has seen since the last weekly refresh
STATE = HERE / "state.json"                  # matches already handled
TEAMS = HERE / "teams.csv"                   # football-data.org name -> app name -> display name
LONDON = ZoneInfo("Europe/London")
MATCHES_API = "https://api.football-data.org/v4/competitions/PL/matches"
X_POST = "https://api.x.com/2/tweets"
STALE_AFTER = dt.timedelta(hours=26)         # never dump a backlog after an outage


def season_of(d):
    return d.year if d.month >= 8 else d.year - 1


def norm(name):
    """'Hull City AFC' and 'Hull City FC' both become 'hull city'."""
    words = re.sub(r"[^a-z0-9& ]", " ", name.lower()).split()
    return " ".join(w for w in words if w not in ("fc", "afc"))


# ---- data ---------------------------------------------------------------------------------------
def read_matches(path):
    if not path.exists():
        return []
    with open(path, newline="", encoding="utf-8") as f:
        return [dict(date=dt.date.fromisoformat(r["Date"]), home=r["Home"], away=r["Away"],
                     hg=int(r["HG"]), ag=int(r["AG"]), kickoff=r.get("KickoffUTC") or "")
                for r in csv.DictReader(f)]


def load_history():
    """The full history plus anything the bot has seen since, each match once, in date order."""
    rows, seen = [], set()
    for m in read_matches(HISTORY) + read_matches(LIVE):
        key = (season_of(m["date"]), m["home"], m["away"])
        if key not in seen:
            seen.add(key)
            rows.append(m)
    return sorted(rows, key=lambda m: (m["date"], m["kickoff"]))


def load_teams():
    with open(TEAMS, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))
    return {norm(r["api_name"]): r["app_name"] for r in rows}, {r["app_name"]: r["display_name"] for r in rows}


def load_state():
    return json.loads(STATE.read_text()) if STATE.exists() else {"handled": {}}


def save_state(state):
    tmp = STATE.with_suffix(".tmp")
    tmp.write_text(json.dumps(state, indent=1, sort_keys=True) + "\n")
    tmp.replace(STATE)


def append_live(m, match_id):
    new = not LIVE.exists()
    with open(LIVE, "a", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        if new:
            w.writerow(["Date", "Home", "Away", "HG", "AG", "KickoffUTC", "MatchId"])
        w.writerow([m["date"].isoformat(), m["home"], m["away"], m["hg"], m["ag"], m["kickoff"], match_id])


# ---- the post ----------------------------------------------------------------------------------
def score(h, a):
    return f"{h}–{a}"


def day(d):
    return f"{d.day} {d.strftime('%b %Y')}"


def ordinal(n):
    return f"{n}{'th' if 10 <= n % 100 <= 20 else {1: 'st', 2: 'nd', 3: 'rd'}.get(n % 10, 'th')}"


def result_word(team_at_home, h, a):
    gf, ga = (h, a) if team_at_home else (a, h)
    return f"{'home' if team_at_home else 'away'} {'win' if gf > ga else 'draw' if gf == ga else 'loss'}"


def weighted_length(text):
    """X counts most emoji as 2 characters; use its own rules when available."""
    try:
        from twitter_text import parse_tweet
        return parse_tweet(text).weightedLength
    except ImportError:
        return sum(2 if ord(c) > 0x2000 and c not in "–’" else 1 for c in text)


def compose(m, hist, disp):
    """Every candidate post for match m (longest first); hist holds only matches played before it."""
    name = lambda t: disp.get(t, t)
    h, a, s = m["hg"], m["ag"], score(m["hg"], m["ag"])
    head = f"⚽ {name(m['home'])} {s} {name(m['away'])}"
    same = [x for x in hist if x["hg"] == h and x["ag"] == a]
    if not same:
        nth = len({(x["hg"], x["ag"]) for x in hist}) + 1
        return [f"🚨🟪 SCORIGAMI 🟪🚨\n{head}\n\nThe first {s} in Premier League history. "
                f"A new square on the grid: the {ordinal(nth)} different scoreline since 1992."]

    def team_line(team, at_home, short=False):
        prior = [x for x in same if (x["home"] if at_home else x["away"]) == team]
        what = f"{s} {result_word(at_home, h, a)}"
        if not prior:
            return f"🟪 Team Scorigami for {name(team)}: first {what} in their Premier League history."
        last = prior[-1]
        icon, where = ("🏠", f"vs {name(last['away'])}") if at_home else ("✈️", f"at {name(last['home'])}")
        return f"{icon} {name(team)}: last {what} {where}" + ("" if short else f", {day(last['date'])}")

    last = same[-1]
    recent = f", most recently {name(last['home'])} {s} {name(last['away'])} ({day(last['date'])})"
    league = f"🟩 No Scorigami. {s} has now happened {len(same) + 1:,} times"
    teams = f"{team_line(m['home'], True)}\n{team_line(m['away'], False)}"
    teams_short = f"{team_line(m['home'], True, True)}\n{team_line(m['away'], False, True)}"
    return [f"{head}\n{league}{recent}.\n\n{teams}",
            f"{head}\n{league}.\n\n{teams}",
            f"{head}\n{league}.\n\n{teams_short}"]


def best_post(candidates):
    for text in candidates:
        if weighted_length(text) <= 280:
            return text
    return candidates[-1][:270] + "…"


def post_to_x(text):
    from requests_oauthlib import OAuth1Session
    session = OAuth1Session(os.environ["X_API_KEY"], os.environ["X_API_SECRET"],
                            os.environ["X_ACCESS_TOKEN"], os.environ["X_ACCESS_SECRET"])
    r = session.post(X_POST, json={"text": text}, timeout=20)
    if r.status_code == 201:
        return r.json()["data"]["id"]
    if r.status_code == 403 and "duplicate" in r.text.lower():
        return "duplicate"
    raise RuntimeError(f"X API {r.status_code}: {r.text[:300]}")


# ---- modes -------------------------------------------------------------------------------------
def fetch_finished(token):
    import requests
    today = dt.datetime.now(dt.timezone.utc).date()
    r = requests.get(MATCHES_API, headers={"X-Auth-Token": token}, timeout=20,
                     params={"status": "FINISHED", "dateFrom": (today - dt.timedelta(days=2)).isoformat(),
                             "dateTo": today.isoformat()})
    r.raise_for_status()
    return sorted(r.json().get("matches", []), key=lambda x: x["utcDate"])


def run(mode):
    token = os.environ.get("FOOTBALL_DATA_TOKEN")
    if not token:
        print("Not configured yet: FOOTBALL_DATA_TOKEN is missing. Nothing to do.")
        return 0
    if mode == "live" and not all(os.environ.get(k) for k in ("X_API_KEY", "X_API_SECRET", "X_ACCESS_TOKEN", "X_ACCESS_SECRET")):
        print("Live mode needs the four X_* secrets. Nothing posted.")
        return 1
    to_app, disp = load_teams()
    state, hist = load_state(), load_history()
    try:
        matches = fetch_finished(token)
    except Exception as e:                      # API down or rate-limited: the next run retries
        print(f"football-data.org unavailable ({e}). Will retry next run.")
        return 0
    now = dt.datetime.now(dt.timezone.utc)
    for x in matches:
        mid = str(x["id"])
        if mid in state["handled"]:
            continue
        names = (x["homeTeam"].get("name") or "", x["awayTeam"].get("name") or "")
        unknown = [n for n in names if norm(n) not in to_app]
        if unknown:
            print(f"Unknown team name(s) {unknown}: add a row to bot/teams.csv. Nothing posted.")
            return 1
        kickoff = dt.datetime.fromisoformat(x["utcDate"].replace("Z", "+00:00"))
        ft = x["score"]["fullTime"]
        m = dict(date=kickoff.astimezone(LONDON).date(), home=to_app[norm(names[0])], away=to_app[norm(names[1])],
                 hg=int(ft["home"]), ag=int(ft["away"]), kickoff=x["utcDate"])
        key = (season_of(m["date"]), m["home"], m["away"])
        already_in_history = any((season_of(h["date"]), h["home"], h["away"]) == key for h in hist)
        prior = [h for h in hist if (season_of(h["date"]), h["home"], h["away"]) != key]
        text = best_post(compose(m, prior, disp))
        if mode == "dry-run":
            print(f"--- {mid} ({weighted_length(text)}/280)\n{text}\n")
            continue
        if mode == "bootstrap" or now - kickoff > STALE_AFTER:
            outcome = "bootstrap" if mode == "bootstrap" else "stale"
        else:
            outcome = post_to_x(text)           # raises on failure: the match stays unhandled and is retried
            print(f"Posted {mid} -> {outcome}\n{text}\n")
        if not already_in_history:
            append_live(m, mid)
            hist.append(m)
        state["handled"][mid] = {"post": outcome, "at": now.isoformat(timespec="seconds")}
        save_state(state)
    return 0


def replay(date_str):
    """Retell one past matchday, using only the history before each match. Prints; posts nothing."""
    d = dt.date.fromisoformat(date_str)
    _, disp = load_teams()
    hist = load_history()
    prior = [m for m in hist if m["date"] < d]
    todays = [m for m in hist if m["date"] == d]
    if not todays:
        print(f"No Premier League matches on {d}.")
        return 1
    for m in todays:
        text = best_post(compose(m, prior, disp))
        print(f"--- {weighted_length(text)}/280\n{text}\n")
        prior.append(m)
    return 0


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--mode", choices=["dry-run", "live", "bootstrap"], default=os.environ.get("BOT_MODE") or "dry-run")
    p.add_argument("--replay", metavar="YYYY-MM-DD")
    args = p.parse_args()
    sys.exit(replay(args.replay) if args.replay else run(args.mode))
