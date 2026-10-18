# Athlete size starter

Player-level height and weight data for a cross-sport visualization project.
The six selected men's leagues are NFL, NBA, MLB, NHL, MLS, and the English
Premier League. Soccer coverage is MLS plus Premier League, not world soccer.

## Start here

- `athletes_height_weight.csv`: usable athlete–league records with both measurements.
- `athletes_all.csv`: the same population including missing or flagged measurements.
- `coverage.csv`: league counts, measurement coverage, means, and season labels.
- `position_summary.csv`: observed means by league and the position ESPN supplies.
- `roster_memberships.csv`: every returned roster membership, before deduplication.
- `rosters.csv`: the team-level retrieval log.
- `manifest.json`: exact timestamps, screening rules, counts, and any failed requests.
- `raw/`: compressed source responses, including retrieval URLs and timestamps.
- `fetch_data.py`: repeatable retrieval and cleaning using only the Python standard library.
- `plot_preview.py`: optional first scatter plot, using NumPy and Matplotlib.

The snapshot date is the retrieval date, **not the measurement date**. Use the
manifest and each row's `season` for the exact date and competition period.
Different leagues are at different stages of their seasons. Roster feeds may
include injured, reserve, practice-squad, preseason, or other listed players.
No match-appearance or minutes threshold is applied.

## Original chart candidates

The closest match found is Tomer Cohen's SPORTCHART series:

- [Athlete Sizes – Update, May 30, 2014](https://sportchart.wordpress.com/2014/05/30/athlete-sizes-update/).
  The expanded chart added soccer, rugby, and other sports to the comparison.
- [Professional Athlete Size Comparison, May 27, 2014](https://sportchart.wordpress.com/2014/05/27/professional-athlete-size-comparison/).
- [Pro Sports Height and Weight, October 30, 2017](https://bradjballard.com/2017/10/30/pro-sports-height-and-weight/).
  A later post shows NFL, NBA, MLB, NHL, and MLS position averages.

This package is a newly collected roster snapshot. It is not the original
2014 chart's underlying dataset.

## Sources and definitions

All records come directly from ESPN's public team roster JSON feeds:

```
https://site.api.espn.com/apis/site/v2/sports/{sport}/{league}/teams?limit=1000
https://site.api.espn.com/apis/site/v2/sports/{sport}/{league}/teams/{team_id}/roster
```

| Competition | sport | league |
| --- | --- | --- |
| NFL | football | nfl |
| NBA | basketball | nba |
| MLB | baseball | mlb |
| NHL | hockey | nhl |
| MLS | soccer | usa.1 |
| Premier League | soccer | eng.1 |

Every row retains the actual roster URL and a player profile link where
provided. Raw responses retain the original source fields. Data attribution:
ESPN. No open-data license is asserted for the source roster data.

### Units and quality

ESPN's numeric roster fields are inches and pounds, checked against the
display strings. `height_cm = height_in × 2.54` and
`weight_kg = weight_lb × 0.45359237`. Conversions retain three decimals for
reproducibility, not as a claim of measurement accuracy. Display one decimal
or whole units in most charts.

Null, non-finite, zero, or negative measurements are missing. Broad screens
flag heights outside 130–250 cm and weights outside 40–200 kg. Soccer weights
above 130 kg are additionally flagged for manual review. Flagged records
remain in `athletes_all.csv` but do not enter the usable dataset. These screens
catch obvious problems; they do not verify the remaining measurements.
No missing height or weight is estimated.

In the delivered 17 October 2026 snapshot, 257 records lack at least one measurement. One additional
record, Newcastle goalkeeper Ewen Jaouen, is listed by ESPN at 432 lb
(approximately 196 kg). That source value is retained in the all-records file
and flagged `soccer_weight_review`; it is excluded from the usable dataset.
No replacement weight is asserted. The final usable dataset has 6,449 rows.

The source values are **listed roster measurements**. They may be rounded,
stale, or collected under different conventions (including height with or
without shoes). Individual measurements have not been independently verified.

### Observation and deduplication

The primary key is `(league, athlete_id)` within a snapshot. IDs are stored as
text. Do not assume name uniqueness. Duplicate memberships within a league
are collapsed. Team names and IDs are retained as pipe-separated lists, with
all original records available in `roster_memberships.csv`. Conflicting
within-league measurements are excluded from the usable dataset.

For an all-sport scatter plot, avoid counting a soccer player twice if both
leagues list them: use `(sport, athlete_id)`. The plotting script collapses
identical measurements across leagues and skips conflicting ones with a
printed notice. Preserve league membership for league comparisons.

`position` and `position_code` retain ESPN's categories. In some leagues these
are broad (for example, NBA Guard/Forward/Center and soccer Defender/Midfielder/
Forward). They should not be presented as detailed playing roles.

## Reproduce or refresh

Python 3.9+ is sufficient for data preparation. Work from this folder:

```bash
# Rebuild the delivered CSVs from the saved snapshot; no network needed.
python fetch_data.py --offline

# Fetch missing responses while retaining existing cached responses.
python fetch_data.py

# Collect a new snapshot in a different folder, preserving this one.
python fetch_data.py --output ../athlete-size-new-snapshot

# Optional plot. Requires numpy and matplotlib in your environment.
python plot_preview.py
```

`--refresh` deliberately replaces cached responses in the chosen output
folder. The fetcher uses at most eight workers, retries failed requests, and
returns a nonzero exit code if any team source remains unavailable. Public
endpoints can change; an empty roster is treated as a failure, not silently
counted as a team with zero players.

## Fields

| Field | Meaning |
| --- | --- |
| athlete_id | ESPN identifier, stored as text; use with league or sport |
| athlete_name | ESPN display name |
| sport, league | Explicit labels for the source competition |
| team, team_id | Roster team; multiple values separated by a pipe character |
| position, position_code | ESPN position name and abbreviation |
| height_cm, weight_kg | Metric conversions of the listed measurements |
| height_in, weight_lb | Original numeric values, including invalid values when present |
| season | Season display label returned by that roster feed |
| roster_status | Source player status, not a uniform cross-league eligibility definition |
| roster_group | Group returned by the feed, if any, such as offense |
| measurement_status | `usable` or `excluded` under the stated screens |
| quality_notes | Semicolon-separated missingness, review, or duplication notes |
| height_display, weight_display | Original human-readable measurement strings |
| source_player_url | ESPN player profile, when returned |
| source_roster_url | Exact JSON roster URL |
| retrieved_at_utc | Timestamp when that source response was downloaded |

## Good first projects

1. **The body map of sport:** one dot per athlete, weight on x, height on y,
   with sport, league, and position filters. Separate distributions from
   position averages so the overlap remains visible.
2. **Cross-sport twins:** find named players with similar height and weight
   across sports. Standardize the two dimensions before combining distances;
   treat similarity as descriptive, not a prediction of sporting ability.
3. **One sport, different bodies:** compare NFL position groups and soccer
   goalkeepers versus outfield players. Keep sample sizes visible.

Pool raw dots cautiously: league roster sizes differ. For comparing shapes,
use normalized densities within each sport or league rather than letting the
largest roster population dominate the view.
