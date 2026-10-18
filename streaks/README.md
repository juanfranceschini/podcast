# Star Streaks

Which Hollywood actors went four lead roles in a row with every film rated 7+ on IMDb, and in which years? A Shiny app styled as the Hollywood Walk of Fame: every film is a star on the sidewalk, every streak a run of stars. Covers Hollywood films since 1960. Grew out of the *blockbuster-streaks* project (box office and acclaim streaks, 1977–2015) and moves it to current IMDb ratings.

## Definitions

- **Film:** a Hollywood feature film released in 1960 or later: an IMDb feature (`titleType = movie`, not adult) with an IMDb rating, 40+ minutes long (or runtime unknown), and a US production (Wikidata lists the United States among its countries of origin, so co-productions like Harry Potter count; British-only films like *The King's Speech* do not). Non-US films, earlier films, documentaries, animation (voice roles), TV films, shorts and videos neither count toward a streak nor break one.
- **Lead role:** one of the first three names billed. Wikipedia's infobox *Starring* list (poster billing) wherever the film has an article and the names resolve to IMDb people (through Wikidata, Wikipedia redirects, or the film's credited cast). Otherwise, IMDb's principal-cast order. IMDb's order alone is not reliable for this: *Mission: Impossible III* lists Philip Seymour Hoffman fourth, but he is billed second. When a poster bills an ensemble alphabetically (five or more names in surname order at the top), nobody is billed above anyone else, so the whole block counts: *The Bling Ring* is an Emma Watson lead role even though she is sixth.
- **Streak:** consecutive lead roles, in release order, each rated at or above the bar with at least the vote floor. Every other lead role ends the run, including a well-rated film with too few votes.
- **Release order:** the US release date where Wikidata has one, then the earliest dated release from Wikidata, Freebase (via the CMU Movie Summary Corpus), the 2016 TMDB dump, or the Wikipedia infobox. Films dated only by year sort mid-year and are marked "month unknown".

The app's defaults are 4 in a row, rated 7.0+ by 1,000+ voters. All three are adjustable in the opening question.

## Rebuild

```
./get_data.sh                 # IMDb datasets + Wikidata extracts into data/raw (about 1.4 GB, not committed)
Rscript prep.R                # builds data/streaks.rds and the list of Wikipedia articles to read
python3 wiki_infobox.py       # reads Starring and Released from each film's Wikipedia infobox (resumable)
Rscript prep.R                # rebuild with Wikipedia billing and dates; lists names it could not resolve
python3 wiki_redirects.py     # follows Wikipedia redirects for those names ("Beyoncé Knowles" -> "Beyoncé")
Rscript prep.R                # final build
Rscript -e 'shiny::runApp()'
```

`data/reference/wiki_infobox.tsv` and `wiki_redirects.tsv` are committed, so a rebuild only reads articles it hasn't seen.

## Sources

- IMDb non-commercial datasets: ratings, votes, credits. Information courtesy of IMDb (https://www.imdb.com), used with permission, for personal and non-commercial use.
- Wikipedia film infoboxes: billing and release dates (facts extracted from CC BY-SA articles).
- Wikidata via the QLever endpoint: release dates, countries of origin, Freebase and Wikipedia links (CC0).
- CMU Movie Summary Corpus (Freebase release dates) and the 2016 TMDB dump from *blockbuster-streaks*: release dates only.

## Known limits

- Ratings are a snapshot from the build date; a streak right at 7.0 can come and go as ratings drift.
- Older films have fewer votes and are rated by enthusiasts, which favours the 1960s and 70s at a 1,000-vote floor. Raise the floor to compare eras on equal footing.
- A film with no country of origin on Wikidata is left out (about 5% of films since 1960 with 1,000+ votes).
- Billing comes from Wikipedia where it resolves; elsewhere it is IMDb's order, which can differ from the poster.
- A few films have only a year for a release date; where an actor has two films in that year, their order is a guess.
