#!/usr/bin/env python3
"""Billing and release dates from English Wikipedia film infoboxes.

IMDb's principal-cast order does not always follow the on-screen billing (Mission: Impossible III lists
Philip Seymour Hoffman fourth; he is billed second), so Wikipedia's "Starring" list, which follows the
poster billing, decides who the leads are wherever a film has an article. "Released" fills in dates.

In:  data/build/wiki_need.tsv        tconst, title (English Wikipedia article); written by prep.R
Out: data/reference/wiki_infobox.tsv tconst, article (after redirects), date (YYYY-MM-DD, month-only = 15th),
                                     starring (items separated by " ¦ "; each item "link target ‖ shown text")
     Committed, and resumable: rerunning fetches only films not already in it.

Reads only each article's lead section, 50 articles per request, one request at a time.
"""
import csv
import json
import re
import sys
import time
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parent / "data"
API = "https://en.wikipedia.org/w/api.php"
UA = "StarStreaks/0.1 (White Belt data project; https://jafranceschini.quarto.pub/juan-franceschini-lab)"
MONTHS = {m: i for i, m in enumerate(
    ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"], 1)}


def fetch(titles):
    q = urllib.parse.urlencode({"action": "query", "prop": "revisions", "rvprop": "content", "rvslots": "main",
                                "rvsection": "0", "redirects": "1", "format": "json", "formatversion": "2",
                                "titles": "|".join(titles)})
    err = None
    for attempt in range(5):
        try:
            req = urllib.request.Request(f"{API}?{q}", headers={"User-Agent": UA})
            with urllib.request.urlopen(req, timeout=60) as r:
                return json.load(r)
        except Exception as e:  # network hiccup or rate limit: back off and retry
            err = e
            time.sleep(2 ** attempt * 2)
    raise SystemExit(f"Wikipedia API failed: {err}")


def field(text, name):
    """An infobox parameter's raw value: up to the next top-level '|' or the end of the infobox."""
    m = re.search(r"\|\s*" + name + r"\s*=", text, flags=re.I)
    if not m:
        return None
    rest, depth, out, i = text[m.end():], 0, [], 0
    while i < len(rest):
        two = rest[i:i + 2]
        if two in ("{{", "[["):
            depth += 1; out.append(two); i += 2; continue
        if two in ("}}", "]]"):
            if depth == 0:
                break
            depth -= 1; out.append(two); i += 2; continue
        if rest[i] == "|" and depth == 0:
            break
        out.append(rest[i]); i += 1
    return "".join(out).strip()


def parse_date(raw):
    """First full (or month) date: {{Film date|1941|4|12}}, {{Start date|...}}, 'April 12, 1941', '12 April 1941'."""
    if not raw:
        return None
    t = re.search(r"\{\{\s*(?:film\s*date|start\s+date(?:\s+and\s+age)?|release\s+date)\s*\|([^}]*)\}\}", raw, flags=re.I)
    if t:
        nums = [p.strip() for p in t.group(1).split("|") if re.fullmatch(r"\s*\d{1,4}\s*", p)]
        if len(nums) >= 3 and len(nums[0]) == 4:
            return f"{int(nums[0]):04d}-{int(nums[1]):02d}-{int(nums[2]):02d}"
        if len(nums) == 2 and len(nums[0]) == 4:
            return f"{int(nums[0]):04d}-{int(nums[1]):02d}-15"
        return None
    plain = re.sub(r"<[^>]+>|\[\[|\]\]|\{\{[^}]*\}\}", " ", raw)
    m = re.search(r"([A-Za-z]+)\s+(\d{1,2}),?\s+(\d{4})", plain)
    if m and m.group(1).lower() in MONTHS:
        return f"{int(m.group(3)):04d}-{MONTHS[m.group(1).lower()]:02d}-{int(m.group(2)):02d}"
    m = re.search(r"(\d{1,2})\s+([A-Za-z]+)\s+(\d{4})", plain)
    if m and m.group(2).lower() in MONTHS:
        return f"{int(m.group(3)):04d}-{MONTHS[m.group(2).lower()]:02d}-{int(m.group(1)):02d}"
    m = re.search(r"([A-Za-z]+)\s+(\d{4})", plain)
    if m and m.group(1).lower() in MONTHS:
        return f"{int(m.group(2)):04d}-{MONTHS[m.group(1).lower()]:02d}-15"
    return None


def starring(raw):
    """The Starring list in billing order: [(link target, shown text), ...]."""
    if not raw:
        return []
    s = re.sub(r"<!--.*?-->", "", raw, flags=re.S)
    s = re.sub(r"<ref[^>]*/>|<ref[^>]*>.*?</ref>", "", s, flags=re.S | re.I)
    s = re.sub(r"\{\{\s*(?:sortname)\s*\|([^|}]*)\|([^|}]*)[^}]*\}\}", r"\1 \2", s, flags=re.I)        # {{sortname|First|Last}}
    s = re.sub(r"\{\{\s*(?:nowrap|nobr|small)\s*\|([^}]*)\}\}", r"\1", s, flags=re.I)
    s = re.sub(r"\{\{\s*(?:efn|refn|sfn|citation needed|cn)[^}]*\}\}", "", s, flags=re.I)
    # people without an English article: {{ill|Name|de|...}}, {{Interlanguage link|Name|...}} -> Name
    s = re.sub(r"\{\{\s*(?:ill|illm|interlanguage link(?: multi)?)\s*\|\s*([^|}]*)[^}]*\}\}", r"\1", s, flags=re.I)
    # list wrappers wherever they sit: {{Plainlist|, {{Indented plainlist|, {{Unbulleted list|, {{startplainlist}}, ...
    s = re.sub(r"\{\{\s*(?:indented plainlist|plainlist|plain list|ubl|unbulleted list|flatlist|hlist|bulleted list|ublist|pl|startplainlist|endplainlist|startflatlist|endflatlist)\s*(?:\||\}\})", "", s, flags=re.I)
    s = re.sub(r"\}\}\s*$", "", s.strip())
    # protect "|" inside links so it does not split items
    s = re.sub(r"\[\[([^\]|]*)\|([^\]]*)\]\]", lambda x: f"[[{x.group(1)}‖{x.group(2)}]]", s)
    parts = re.split(r"\n\s*\*+|<br\s*/?>|\n|\|", s, flags=re.I)
    if len([p for p in parts if p.strip()]) <= 1 and "," in s:
        parts = s.split(",")
    items = []
    for p in parts:
        p = p.strip(" *;\t")
        if not p:
            continue
        lk = re.search(r"\[\[([^\]]+)\]\]", p)
        if lk:
            target, _, shown = lk.group(1).partition("‖")
            items.append((target.strip(), (shown or target).strip()))
        else:
            txt = re.sub(r"\{\{[^}]*\}\}|'''?|\(.*?\)", "", p).strip(" ,.")
            if txt and not re.search(r"\d{4}|^(and|with)$", txt, flags=re.I):
                items.append(("", txt))
    return items


def main():
    rows = list(csv.DictReader(open(ROOT / "build" / "wiki_need.tsv", encoding="utf-8"), delimiter="\t"))
    out_path = ROOT / "reference" / "wiki_infobox.tsv"
    done = set()
    if out_path.exists():
        done = {r["tconst"] for r in csv.DictReader(open(out_path, encoding="utf-8"), delimiter="\t")}
    todo = [r for r in rows if r["tconst"] not in done]
    new = not out_path.exists()
    with open(out_path, "a", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh, delimiter="\t")
        if new:
            w.writerow(["tconst", "article", "date", "starring"])
        for i in range(0, len(todo), 50):
            batch = todo[i:i + 50]
            titles = [r["title"].replace("_", " ") for r in batch]
            q = fetch(titles).get("query", {})
            alias = {x["from"]: x["to"] for k in ("normalized", "redirects") for x in q.get(k, [])}
            pages = {p["title"]: p for p in q.get("pages", [])}
            for r, t in zip(batch, titles):
                seen = set()
                while t in alias and t not in seen:
                    seen.add(t); t = alias[t]
                p = pages.get(t, {})
                text = p["revisions"][0]["slots"]["main"]["content"] if p.get("revisions") else ""
                stars = " ¦ ".join(f"{a}‖{b}" for a, b in starring(field(text, "starring")))
                w.writerow([r["tconst"], t if text else "", parse_date(field(text, "released")) or "", stars])
            fh.flush()
            print(f"\r{min(i + 50, len(todo))}/{len(todo)}", end="", file=sys.stderr)
            time.sleep(0.3)
    print(file=sys.stderr)


if __name__ == "__main__":
    main()
