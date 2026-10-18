# The Shape of Sport

6,449 pro athletes from the NFL, NBA, MLB, NHL, MLS and Premier League on one grid of listed height and weight. A Shiny app styled as a measuring wall: graph paper between two yellow tape measures. It opens on each league's contours with three pencilled notes, so you can see where each league's bodies cluster. Hover a dot to read the player off the tape, pin them to see the closest build in every other sport, or put in your own height and weight and get a doorframe-style pencil mark and your closest pros. Grew out of an athlete-size explorer built on ChatGPT Sites (17 October 2026 snapshot), itself inspired by SPORTCHART's 2014 athlete-size charts.

## Definitions

- **Athlete:** a player on an ESPN team roster in one of the six leagues on the snapshot date, with both a listed height and weight. 6,449 of 6,707 records. The 257 with a missing measurement, and one soccer weight listed at 432 lb (flagged for review), are left out. Nothing is imputed.
- **Position:** ESPN's position on the roster, keyed by sport (`basketball:G` and `soccer:G` are different positions).
- **Same size:** players listed at exactly the same height and weight: 5,932 players share their listing with someone, up to 49 on one spot. The pinned card lists everyone at that size.
- **Spread:** heights are listed in whole inches and weights in whole pounds, so each dot is drawn with a fixed random nudge of up to ±0.45 in and ±0.45 lb (seeded in `prep.R`). It never leaves its own listing, and the tape shows that listing as a band behind the reading. Readings, cards, percentiles and "same size" use the listed values. The Spread toggle above the grid shows exact positions.
- **Taller than X%:** the share of a group listed strictly shorter: the player's own league, and all 6,449 players. The tallest and shortest in a group are named as such rather than "100%" or "0%". For "you" (outside every group) the plain percentage stands.
- **Built like:** for each sport, the athlete whose listed size is closest, with height and weight each divided by their standard deviation across all 6,449 players. Ties (common, given shared dots) go to the first by name; the card says how many more share that size. Differences are written out ("15.2 cm shorter · 33.1 kg heavier"), and a match more than 0.75 standard deviations away is labelled "nearest, but nobody's close".
- **Contours:** per league, a 2D kernel density estimate (`MASS::kde2d`) of the players in view, with the lines enclosing the densest half and nine in ten of them. Recomputed in R for whatever is filtered; drawn for leagues with at least 25 players in view. On by default.
- **Pencil notes:** three facts computed in `app.R` from the data (the NFL linemen's median weight, the NBA's share at 2 m or taller, soccer's median weight and its gap to the next-lightest league), shown on the opening view only.

Metric values are canonical (height_in × 2.54, weight_lb × 0.45359237); the units toggle only changes the display. Imperial readings show ESPN's original inches and pounds.

## How it's built

- `prep.R` reads the snapshot CSVs and writes `data/athletes.rds`: one row per athlete, sorted by name, with league percentiles, a fixed shuffled drawing order and the fixed Spread nudges.
- `app.R` owns the state: the leagues on the wall ("All", or any set of the six), position (once a single sport is in view), name/team search, the cards, "you", contours, and the URL (`?leagues=NBA,MLS&pos=basketball:C&units=imperial&contours=0&me=180x75&p=NBA:5104157`; older `sports=`/`league=` links still work). It embeds the athletes and the pencil notes once in the page as compact JSON.
- `www/shape.js` draws the grid on a canvas, reads the tape on hover, pins on click, zooms (trackpad pinch, `+`/`−`, or the wheel once the grid has focus) and pans, and draws the contours, notes and the "you" mark. It hit-tests in the browser; R decides who is visible. On narrow screens a pinned player shows in a bar docked to the grid, since the card sits below it.
- `www/shape.css`: the measuring wall. The five sport colours were validated as a set on the paper colour (all pairs distinguishable for deutan and protan readers, all ≥ 3:1 against the paper).

## Rebuild

```
python3 data/snapshot-2026-10-17/fetch_data.py --offline    # reproduce the snapshot CSVs from the raw responses kept in raw/
Rscript prep.R                                             # builds data/athletes.rds
Rscript -e 'shiny::runApp()'
```

To refresh, collect a new snapshot into a new dated folder (`fetch_data.py --output data/snapshot-YYYY-MM-DD`, which calls ESPN), point `SNAP` in `prep.R` and the download path in `app.R` at it, and review the counts and the "How it works" text. Keep the old snapshot.

## Deploy

```r
rsconnect::deployApp(appName = "shape-of-sport", appTitle = "The Shape of Sport", account = "plot-recommendations",
                     appFiles = c("app.R", "www/shape.js", "www/shape.css", "www/favicon.svg", "data/athletes.rds",
                                  "data/snapshot-2026-10-17/athletes_height_weight.csv"))
```

## Sources

- ESPN public team roster feeds (`site.api.espn.com/apis/site/v2/sports/{sport}/{league}/teams/{id}/roster`), collected 17 October 2026. Every raw response is kept, gzipped, in `data/snapshot-2026-10-17/raw/`. No open-data licence is asserted for ESPN's data; each player links to their ESPN profile.

## Known limits

- Listed, not measured: values can be rounded or out of date, and measuring conventions vary (with or without shoes, for one). Heights are listed in whole inches, hence the rows.
- Rosters include reserve, injured and preseason players; being listed doesn't mean playing.
- Soccer is MLS and the Premier League only, and MLS lists fewer measurements (80% coverage, Premier League 89%, the rest 100%).
- Men's competitions only. Leagues have very different roster sizes, so the NFL's 2,564 players dominate the raw cloud.
- Similar size says nothing about skill or suitability for a sport.
