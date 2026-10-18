#!/usr/bin/env bash
# Download the raw inputs for prep.R into data/raw (about 1.4 GB; not committed).
#   IMDb non-commercial datasets (personal and non-commercial use): https://developer.imdb.com/non-commercial-datasets/
#   Wikidata via the QLever SPARQL endpoint: release dates, countries, Freebase and Wikipedia links
#   tmdb-movies.csv (2016 TMDB dump, for release dates only) comes from the blockbuster-streaks project.
set -euo pipefail
cd "$(dirname "$0")/data/raw"

for f in title.basics title.ratings title.principals name.basics; do
  echo "IMDb $f"
  curl -sfL --retry 3 -o "$f.tsv.gz.part" "https://datasets.imdbws.com/$f.tsv.gz" && mv "$f.tsv.gz.part" "$f.tsv.gz"
done

P='PREFIX wdt: <http://www.wikidata.org/prop/direct/> PREFIX wd: <http://www.wikidata.org/entity/> PREFIX p: <http://www.wikidata.org/prop/>
PREFIX psv: <http://www.wikidata.org/prop/statement/value/> PREFIX pq: <http://www.wikidata.org/prop/qualifier/>
PREFIX wikibase: <http://wikiba.se/ontology#> PREFIX schema: <http://schema.org/>'
q() { echo "Wikidata $1"; curl -sfL --max-time 900 -X POST "https://qlever.dev/api/wikidata" -H "Accept: text/tab-separated-values" \
        --data-urlencode "query=$P $2" -o "$1.part" && mv "$1.part" "$1"; }
FILM='?f wdt:P345 ?imdb . FILTER(STRSTARTS(?imdb, "tt"))'
q wd_dates_full.tsv      "SELECT ?imdb ?d ?prec ?rank ?place WHERE { $FILM ?f p:P577 ?st . ?st psv:P577 ?v . ?v wikibase:timeValue ?d . ?v wikibase:timePrecision ?prec . ?st wikibase:rank ?rank . OPTIONAL { ?st pq:P291 ?place } }"
q wd_countries.tsv       "SELECT ?imdb ?c WHERE { $FILM ?f wdt:P495 ?c . }"
q wd_freebase.tsv        "SELECT ?imdb ?fb WHERE { $FILM ?f wdt:P646 ?fb . }"
q wd_enwiki.tsv          "SELECT ?imdb ?article WHERE { $FILM ?article schema:about ?f ; schema:isPartOf <https://en.wikipedia.org/> . }"
q wd_people_enwiki.tsv   "SELECT ?nm ?article WHERE { ?p wdt:P345 ?nm . FILTER(STRSTARTS(?nm, \"nm\")) ?article schema:about ?p ; schema:isPartOf <https://en.wikipedia.org/> . }"
echo "Done. Next: Rscript prep.R, python3 wiki_infobox.py, Rscript prep.R"
