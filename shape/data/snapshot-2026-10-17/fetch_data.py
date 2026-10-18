#!/usr/bin/env python3
"""Fetch public ESPN rosters or rebuild CSVs from cached responses. Python 3 stdlib only."""
import argparse
import concurrent.futures
from collections import Counter
import csv
import datetime as dt
import gzip
import hashlib
import json
import math
from pathlib import Path
import statistics
import time
import urllib.request

LEAGUES = [
    ("American football", "NFL", "football", "nfl"),
    ("Basketball", "NBA", "basketball", "nba"),
    ("Baseball", "MLB", "baseball", "mlb"),
    ("Ice hockey", "NHL", "hockey", "nhl"),
    ("Soccer", "MLS", "soccer", "usa.1"),
    ("Soccer", "Premier League", "soccer", "eng.1"),
]
BASE = "https://site.api.espn.com/apis/site/v2/sports"
FIELDS = ["athlete_id", "athlete_name", "sport", "league", "team", "team_id",
          "position", "position_code", "height_cm", "weight_kg", "height_in", "weight_lb",
          "season", "roster_status", "roster_group", "measurement_status", "quality_notes",
          "height_display", "weight_display", "source_player_url", "source_roster_url",
          "retrieved_at_utc"]

def utcnow():
    return dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds")

def read_json(url, path, offline=False, refresh=False):
    if path.exists() and not refresh:
        with gzip.open(path, "rt", encoding="utf-8") as f:
            return json.load(f)
    if offline:
        raise FileNotFoundError(f"No cached response: {path}")
    for attempt in range(3):
        try:
            req = urllib.request.Request(url, headers={"Accept": "application/json", "User-Agent": "AthleteSizeResearch/1.0"})
            with urllib.request.urlopen(req, timeout=35) as r:
                raw = r.read()
            payload = json.loads(raw)
            record = {"url": url, "retrieved_at_utc": utcnow(),
                      "response_sha256": hashlib.sha256(raw).hexdigest(), "data": payload}
            path.parent.mkdir(parents=True, exist_ok=True)
            with gzip.open(path, "wt", encoding="utf-8") as f:
                json.dump(record, f, ensure_ascii=False)
            return record
        except Exception:
            if attempt == 2:
                raise
            time.sleep(1 + attempt)

def flatten(items, group=""):
    for item in items:
        if isinstance(item, dict) and "items" in item:
            yield from flatten(item["items"], item.get("position", group))
        elif isinstance(item, dict) and item.get("id") and (item.get("displayName") or item.get("fullName")):
            yield item, group

def number(value):
    try:
        result = float(value)
        return result if math.isfinite(result) else None
    except (ValueError, TypeError):
        return None

def normalize(player, group, team, league, record):
    sport, label, _, _ = league
    h, w = number(player.get("height")), number(player.get("weight"))
    notes = []
    if h is None or h <= 0:
        notes.append("missing_height")
    if w is None or w <= 0:
        notes.append("missing_weight")
    hc = round(h * 2.54, 3) if h is not None and h > 0 else None
    wk = round(w * 0.45359237, 3) if w is not None and w > 0 else None
    if hc is not None and not 130 <= hc <= 250:
        notes.append("height_review")
    if wk is not None and not 40 <= wk <= 200:
        notes.append("weight_review")
    if sport == "Soccer" and wk is not None and wk > 130:
        notes.append("soccer_weight_review")
    pos = player.get("position") or {}
    status = player.get("status") or {}
    links = player.get("links") or []
    player_url = next((l.get("href", "") for l in links if "playercard" in l.get("rel", [])), "")
    return {
        "athlete_id": str(player["id"]), "athlete_name": player.get("displayName") or player.get("fullName"),
        "sport": sport, "league": label, "team": team.get("displayName", ""), "team_id": str(team["id"]),
        "position": pos.get("displayName") or pos.get("name", ""), "position_code": pos.get("abbreviation", ""),
        "height_cm": hc, "weight_kg": wk, "height_in": h, "weight_lb": w,
        "season": record["data"].get("season", {}).get("displayName", ""),
        "roster_status": status.get("name", "") if isinstance(status, dict) else str(status),
        "roster_group": group, "measurement_status": "usable" if not notes else "excluded",
        "quality_notes": ";".join(notes), "height_display": player.get("displayHeight", ""),
        "weight_display": player.get("displayWeight", ""), "source_player_url": player_url,
        "source_roster_url": record["url"], "retrieved_at_utc": record["retrieved_at_utc"],
    }

def write_csv(path, rows, fields):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=Path(__file__).resolve().parent)
    parser.add_argument("--offline", action="store_true", help="Rebuild exclusively from saved raw JSON.")
    parser.add_argument("--refresh", action="store_true", help="Replace cached responses with a fresh live snapshot.")
    parser.add_argument("--workers", type=int, default=6)
    args = parser.parse_args()
    if args.offline and args.refresh:
        parser.error("--offline and --refresh cannot be combined")
    root = args.output
    tasks, failures, team_counts, team_records = [], [], {}, []
    for league in LEAGUES:
        sport, label, api_sport, code = league
        try:
            record = read_json(f"{BASE}/{api_sport}/{code}/teams?limit=1000",
                               root / "raw" / code / "teams.json.gz", args.offline, args.refresh)
            catalog = record["data"]["sports"][0]["leagues"][0]
            teams = [x["team"] for x in catalog["teams"] if not x["team"].get("isAllStar", False)]
            team_counts[label] = len(teams)
            for team in teams:
                tasks.append((league, team))
            print(f"{label}: {len(teams)} teams", flush=True)
        except Exception as e:
            failures.append({"league": label, "stage": "team_list", "error": str(e)})

    def fetch(task):
        league, team = task
        _, label, api_sport, code = league
        url = f"{BASE}/{api_sport}/{code}/teams/{team['id']}/roster"
        try:
            record = read_json(url, root / "raw" / code / f"roster_{team['id']}.json.gz", args.offline, args.refresh)
            rows = [normalize(p, g, team, league, record) for p, g in flatten(record["data"].get("athletes", []))]
            if not rows:
                raise ValueError("Empty roster returned")
            return rows, {"league": label, "team": team["displayName"], "team_id": str(team["id"]),
                          "rows_returned": len(rows), "source_url": url, "retrieved_at_utc": record["retrieved_at_utc"]}, None
        except Exception as e:
            return [], None, {"league": label, "team": team.get("displayName"), "source_url": url, "error": str(e)}

    memberships = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, min(args.workers, 8))) as pool:
        for i, (rows, meta, error) in enumerate(pool.map(fetch, tasks), 1):
            memberships.extend(rows)
            if meta:
                team_records.append(meta)
            if error:
                failures.append(error)
                print("FAILED " + json.dumps(error), flush=True)
            if i % 20 == 0 or i == len(tasks):
                print(f"Retrieved {i}/{len(tasks)} rosters; {len(memberships)} memberships", flush=True)

    # One row per athlete per league. Keep every original membership in a separate CSV.
    grouped = {}
    for row in memberships:
        grouped.setdefault((row["league"], row["athlete_id"]), []).append(row)
    players, conflicts = [], []
    for key, candidates in grouped.items():
        candidates.sort(key=lambda r: (r["measurement_status"] == "usable", r["roster_status"] == "Active", r["retrieved_at_utc"]), reverse=True)
        row = dict(candidates[0])
        teams = sorted({r["team"] for r in candidates})
        row["team"] = " | ".join(teams)
        row["team_id"] = " | ".join(sorted({r["team_id"] for r in candidates}))
        if len(teams) > 1:
            row["quality_notes"] = ";".join(filter(None, [row["quality_notes"], "multiple_roster_memberships"]))
        dimensions = {(r["height_in"], r["weight_lb"]) for r in candidates}
        if len(dimensions) > 1:
            row["quality_notes"] = ";".join(filter(None, [row["quality_notes"], "conflicting_measurements"]))
            row["measurement_status"] = "excluded"
            conflicts.append({"key": key, "dimensions": list(dimensions)})
        players.append(row)
    players.sort(key=lambda r: (r["league"], r["athlete_name"], r["athlete_id"]))
    usable = [r for r in players if r["measurement_status"] == "usable"]
    coverage = []
    for _, label, _, _ in LEAGUES:
        all_rows = [r for r in players if r["league"] == label]
        good = [r for r in usable if r["league"] == label]
        coverage.append({
            "league": label, "teams_listed": team_counts.get(label, 0),
            "teams_fetched": sum(t["league"] == label for t in team_records),
            "athlete_league_records": len(all_rows), "usable_height_weight": len(good),
            "coverage_pct": round(100 * len(good) / len(all_rows), 2) if all_rows else 0,
            "excluded_records": len(all_rows) - len(good),
            "mean_height_cm": round(statistics.mean(r["height_cm"] for r in good), 2) if good else None,
            "mean_weight_kg": round(statistics.mean(r["weight_kg"] for r in good), 2) if good else None,
            "seasons": " | ".join(sorted({r["season"] for r in all_rows})),
        })
    position_rows = []
    for league, position in sorted({(r["league"], r["position"]) for r in usable}):
        sub = [r for r in usable if r["league"] == league and r["position"] == position]
        position_rows.append({"league": league, "position": position, "n": len(sub),
                              "mean_height_cm": round(statistics.mean(r["height_cm"] for r in sub), 2),
                              "mean_weight_kg": round(statistics.mean(r["weight_kg"] for r in sub), 2)})
    write_csv(root / "athletes_all.csv", players, FIELDS)
    write_csv(root / "athletes_height_weight.csv", usable, FIELDS)
    write_csv(root / "roster_memberships.csv", memberships, FIELDS)
    write_csv(root / "coverage.csv", coverage, list(coverage[0]))
    write_csv(root / "position_summary.csv", position_rows, ["league", "position", "n", "mean_height_cm", "mean_weight_kg"])
    write_csv(root / "rosters.csv", team_records, ["league", "team", "team_id", "rows_returned", "source_url", "retrieved_at_utc"])
    ids = {(r["sport"], r["athlete_id"]) for r in usable}
    manifest = {
        "built_at_utc": utcnow(), "source": "ESPN public team roster feeds",
        "scope": "Men's league roster snapshot; roster listing does not imply match appearance or active squad eligibility.",
        "unit_of_observation": "One athlete per league per snapshot; league is part of the unique key.",
        "unit_conversions": {"height_cm": "height_in * 2.54", "weight_kg": "weight_lb * 0.45359237"},
        "screening": {"height_cm_inclusive": [130, 250], "weight_kg_inclusive": [40, 200],
                      "soccer_weight_manual_review_above_kg": 130,
                      "missing": "Null, non-finite or non-positive source measurements are unusable. No imputation.",
                      "note": "Broad error screens, not a validation of individual measurements."},
        "roster_memberships": len(memberships), "athlete_league_records": len(players),
        "usable_athlete_league_records": len(usable), "distinct_usable_sport_athlete_ids": len(ids),
        "duplicate_memberships_removed": len(memberships) - len(players),
        "excluded_reason_combinations": dict(Counter(r["quality_notes"] for r in players if r["measurement_status"] != "usable")),
        "within_league_measurement_conflicts": conflicts, "failures": failures, "coverage": coverage,
        "first_retrieved_at_utc": min((r["retrieved_at_utc"] for r in team_records), default=None),
        "last_retrieved_at_utc": max((r["retrieved_at_utc"] for r in team_records), default=None),
    }
    (root / "manifest.json").write_text(json.dumps(manifest, indent=2, ensure_ascii=False), encoding="utf-8")
    print(json.dumps(manifest, indent=2), flush=True)
    if failures:
        raise SystemExit("Some sources failed. Cached successes are retained. Rerun to retry failed requests.")

if __name__ == "__main__":
    main()
