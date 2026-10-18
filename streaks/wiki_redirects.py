#!/usr/bin/env python3
"""Where Wikipedia redirects the names in "Starring" that prep.R could not resolve.

Some links point at a redirect ("Beyoncé Knowles" -> "Beyoncé", "Mos Def" -> "Yasiin Bey"), and some unlinked
names are themselves redirects. Following them lets prep.R map those names to IMDb people through Wikidata.

In:  data/build/unresolved.tsv         tconst, pos, target, shown   (written by prep.R)
Out: data/reference/wiki_redirects.tsv from, to                     (committed; resumable)
"""
import csv
import json
import sys
import time
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parent / "data"
API = "https://en.wikipedia.org/w/api.php"
UA = "StarStreaks/0.1 (White Belt data project; https://jafranceschini.quarto.pub/juan-franceschini-lab)"


def main():
    names = set()
    for r in csv.DictReader(open(ROOT / "build" / "unresolved.tsv", encoding="utf-8"), delimiter="\t"):
        for k in ("target", "shown"):
            if r.get(k):
                names.add(r[k].strip())
    out = ROOT / "reference" / "wiki_redirects.tsv"
    done = set()
    if out.exists():
        done = {r["from"] for r in csv.DictReader(open(out, encoding="utf-8"), delimiter="\t")}
    todo = sorted(n for n in names if n and n not in done and "{{" not in n)
    new = not out.exists()
    with open(out, "a", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh, delimiter="\t")
        if new:
            w.writerow(["from", "to"])
        for i in range(0, len(todo), 50):
            batch = todo[i:i + 50]
            q = urllib.parse.urlencode({"action": "query", "redirects": "1", "format": "json", "formatversion": "2", "titles": "|".join(batch)})
            req = urllib.request.Request(f"{API}?{q}", headers={"User-Agent": UA})
            with urllib.request.urlopen(req, timeout=60) as r:
                j = json.load(r).get("query", {})
            alias = {x["from"]: x["to"] for k in ("normalized", "redirects") for x in j.get(k, [])}
            exists = {p["title"] for p in j.get("pages", []) if not p.get("missing") and not p.get("invalid")}
            for n in batch:
                t, seen = n, set()
                while t in alias and t not in seen:
                    seen.add(t); t = alias[t]
                w.writerow([n, t if t in exists else ""])
            fh.flush()
            print(f"\r{min(i + 50, len(todo))}/{len(todo)}", end="", file=sys.stderr)
            time.sleep(0.3)
    print(file=sys.stderr)


if __name__ == "__main__":
    main()
