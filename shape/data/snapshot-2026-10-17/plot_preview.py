#!/usr/bin/env python3
"""Render a starter scatter plot from athletes_height_weight.csv. Requires numpy, matplotlib."""
import csv
import datetime as dt
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.ticker import MultipleLocator
import numpy as np

ROOT = Path(__file__).resolve().parent
COLORS = {"American football": "#D15B31", "Basketball": "#7458AF",
          "Baseball": "#BA913A", "Ice hockey": "#3F83AE", "Soccer": "#2A8A68"}
LABELS = {"American football": "NFL", "Basketball": "NBA", "Baseball": "MLB",
          "Ice hockey": "NHL", "Soccer": "Soccer · MLS + Premier League"}

def main():
    with (ROOT / "athletes_height_weight.csv").open(encoding="utf-8", newline="") as f:
        rows = list(csv.DictReader(f))
    grouped = {}
    for r in rows:
        grouped.setdefault((r["sport"], r["athlete_id"]), []).append(r)
    records, conflicts = [], []
    for key, group in grouped.items():
        if len({(r["height_cm"], r["weight_kg"]) for r in group}) > 1:
            conflicts.append(key)
            continue
        records.append(group[0])
    if conflicts:
        print(f"Excluded {len(conflicts)} cross-league measurement conflicts: {conflicts}")
    bg, ink = "#F8F6F0", "#25322F"
    plt.rcParams.update({"font.family": "DejaVu Sans", "font.size": 11,
                         "text.color": ink, "axes.labelcolor": ink,
                         "xtick.color": "#67706C", "ytick.color": "#67706C"})
    fig, ax = plt.subplots(figsize=(14, 10), dpi=150)
    fig.set_facecolor(bg)
    ax.set_facecolor(bg)
    fig.subplots_adjust(left=.083, right=.97, bottom=.18, top=.77)
    date = dt.datetime.fromisoformat(max(r["retrieved_at_utc"] for r in records)).strftime("%d %b %Y")
    fig.text(.083, .94, "THE SHAPE OF SPORT", fontsize=31, weight="bold", va="top")
    fig.text(.083, .886, f"{len(records):,} athletes. Five sports. Six leagues.", fontsize=16)
    fig.text(.083, .849, "Each dot is one player with a listed height and weight.", fontsize=11, color="#67706C")
    # Interleave sports deterministically so a single league is not always on top.
    rng = np.random.default_rng(42)
    order = rng.permutation(len(records))
    xs = np.array([float(r["weight_kg"]) for r in records])
    ys = np.array([float(r["height_cm"]) for r in records])
    cs = np.array([COLORS[r["sport"]] for r in records])
    ax.scatter(xs[order], ys[order], c=cs[order], s=13, alpha=.34, linewidths=0, zorder=2)
    ax.set_axisbelow(True)
    ax.grid(color="#D8DDD7", linewidth=.65, alpha=.7)
    for spine in ("top", "right"):
        ax.spines[spine].set_visible(False)
    for spine in ("left", "bottom"):
        ax.spines[spine].set_color("#A8B1A9")
    ax.tick_params(length=0, pad=9)
    ax.set_xlabel("Listed weight (kg)", labelpad=15, fontsize=12)
    ax.set_ylabel("Listed height (cm)", labelpad=15, fontsize=12)
    ax.set_xlim(np.floor((xs.min() - 4) / 10) * 10, np.ceil((xs.max() + 4) / 10) * 10)
    ax.set_ylim(np.floor((ys.min() - 3) / 10) * 10, np.ceil((ys.max() + 3) / 10) * 10)
    ax.xaxis.set_major_locator(MultipleLocator(20))
    ax.yaxis.set_major_locator(MultipleLocator(10))
    handles = [Line2D([], [], marker="o", color="none", markerfacecolor=COLORS[s],
                      markeredgecolor="none", markersize=8, label=LABELS[s]) for s in COLORS]
    fig.legend(handles=handles, ncol=5, frameon=False, loc="upper left", bbox_to_anchor=(.077, .828),
               handletextpad=.5, columnspacing=1.7, fontsize=10)
    fig.text(.083, .092, f"ROSTER SNAPSHOT  /  {date.upper()}  /  MEN'S COMPETITIONS", fontsize=9, weight="bold")
    fig.text(.083, .061, "Source: ESPN roster feeds. Listed measurements may be rounded or stale; collection methods vary.",
             fontsize=9, color="#67706C")
    fig.text(.083, .038, "Missing or flagged measurements excluded. No imputation. Roster inclusion does not imply match participation.",
             fontsize=9, color="#67706C")
    fig.savefig(ROOT / "athlete_size_preview.png", dpi=150, facecolor=bg)
    plt.close(fig)
    print(json.dumps({"plotted_unique_sport_athletes": len(records),
                      "cross_league_conflicts_excluded": len(conflicts),
                      "weight_range_kg": [float(xs.min()), float(xs.max())],
                      "height_range_cm": [float(ys.min()), float(ys.max())]}, indent=2))

if __name__ == "__main__":
    main()
