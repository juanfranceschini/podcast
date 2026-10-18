# The Shape of Sport — 6,449 pro athletes on one height-and-weight grid, and who is built like whom.
# Data: data/athletes.rds, built by prep.R from ESPN's public team roster feeds (snapshot of 17 October 2026).
# R decides who is on the grid and writes the cards; www/shape.js draws the grid, reads the tape and handles the pointer.

library(shiny)
suppressPackageStartupMessages({ library(data.table); library(stringi) })

`%||%` <- function(a, b) if (is.null(a) || !length(a) || identical(a, "")) b else a

# ---- data -----------------------------------------------------------------------------------------
d <- readRDS("data/athletes.rds")
P <- d$players                                     # one row per athlete, sorted by name; P$i is the row number
N <- nrow(P)
SNAPSHOT <- d$snapshot

SPORT_ORDER <- c("football", "basketball", "baseball", "hockey", "soccer")
SPORT_LABEL <- setNames(c("NFL", "NBA", "MLB", "NHL", "Soccer"), SPORT_ORDER)
# validated as a set on the paper colour: every pair stays apart for deutan and protan readers
SPORT_COLOR <- setNames(c("#b33a14", "#9f66c0", "#a67e0e", "#3b5993", "#23906f"), SPORT_ORDER)
LEAGUES <- c("NFL", "NBA", "MLB", "NHL", "MLS", "Premier League")
LEAGUE_SPORT <- c(NFL = "football", NBA = "basketball", MLB = "baseball", NHL = "hockey", MLS = "soccer", `Premier League` = "soccer")
LEAGUE_SHORT <- c(NFL = "NFL", NBA = "NBA", MLB = "MLB", NHL = "NHL", MLS = "MLS", `Premier League` = "EPL")
TEAMS <- sort(unique(P$team))
SPORT_ROWS <- split(P$i, P$sport)
LEAGUE_ROWS <- split(P$i, P$league)
SD_H <- sd(P$h); SD_W <- sd(P$w)                   # "closest build" weighs height and weight by their spread
FAMOUS <- c("Victor Wembanyama", "Lionel Messi", "Patrick Mahomes", "Shohei Ohtani", "Connor McDavid", "Erling Haaland", "Stephen Curry")
FAMOUS <- P[name %in% FAMOUS][match(FAMOUS, name, nomatch = 0)]$i

# ---- helpers ----------------------------------------------------------------------------------------
esc <- function(x) htmltools::htmlEscape(x)
fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
fmt1 <- function(x) sub("\\.0$", "", sprintf("%.1f", x))
ft_in <- function(inches) {
  ft <- floor(inches / 12); i <- round(inches - ft * 12, 1)
  ifelse(i >= 12, sprintf("%d′ 0″", ft + 1), sprintf("%d′ %s″", ft, fmt1(i)))
}
cm_to_in <- function(cm) cm / 2.54
kg_to_lb <- function(kg) kg / 0.45359237
dot <- function(sport) sprintf('<i class="dot" style="--c:%s"></i>', SPORT_COLOR[sport])
X_ICON <- '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M6.5 6.5l11 11M17.5 6.5l-11 11"/></svg>' 

# A size in the chosen units: list(h, hu, w, wu) for the big readout, and the other system for the small one
size_txt <- function(h, w, hin, lb, units) {
  if (units == "metric") list(h = fmt1(h), hu = "cm", w = fmt1(w), wu = "kg", alt_h = ft_in(hin), alt_w = paste(fmt1(lb), "lb"))
  else list(h = ft_in(hin), hu = "", w = fmt1(lb), wu = "lb", alt_h = paste(fmt1(h), "cm"), alt_w = paste(fmt1(w), "kg"))
}
# Three pencil notes for the default view. Every number is computed here from the data; positions are in kg and cm,
# placed on empty paper, with a leader to what they describe.
NOTES <- local({
  ol <- P[league == "NFL" & code %in% c("OT", "G", "C", "DT", "OL")]
  nba <- P[league == "NBA"]; soc <- P[sport == "soccer"]
  tall <- round(100 * mean(nba$h >= 200)); ow <- median(ol$w); sw <- median(soc$w)
  gap <- min(P[sport != "soccer", median(w), by = league]$V1) - sw
  list(
    list(lw = 141, lh = 217, aw = ow, ah = median(ol$h) + 6,
         metric = c(sprintf("%s NFL linemen,", fmt_n(nrow(ol))), sprintf("a crowd of their own near %d kg", round(ow))),
         imperial = c(sprintf("%s NFL linemen,", fmt_n(nrow(ol))), sprintf("a crowd of their own near %d lb", round(kg_to_lb(ow))))),
    list(lw = 52, lh = 226, aw = median(nba$w) - 3, ah = 214,
         metric = c(sprintf("%d%% of the NBA is listed", tall), "at 2 m or taller"),
         imperial = c(sprintf("%d%% of the NBA is listed", tall), "at 6′7″ or taller")),
    list(lw = 50, lh = 205, aw = sw - 2, ah = 191,
         metric = c(sprintf("Soccer's median weight: %d kg,", round(sw)), sprintf("the lightest by %d kg", round(gap))),
         imperial = c(sprintf("Soccer's median weight: %d lb,", round(kg_to_lb(sw))), sprintf("the lightest by %d lb", round(kg_to_lb(gap)))))
  )
})

# Everything the canvas needs, embedded once in the page (compact columns; teams and leagues as indices)
DATA_JSON <- local({
  x <- list(name = P$name, team = match(P$team, TEAMS) - 1L, lg = match(P$league, LEAGUES) - 1L, code = P$code,
            h = P$h, w = P$w, hin = P$hin, lb = P$lb, z = P$z - 1L, jh = P$jh, jw = P$jw,
            teams = TEAMS, leagues = unname(LEAGUE_SHORT[LEAGUES]), lsport = match(LEAGUE_SPORT[LEAGUES], SPORT_ORDER) - 1L,
            sports = lapply(SPORT_ORDER, function(k) list(key = k, label = SPORT_LABEL[[k]], color = SPORT_COLOR[[k]])), notes = NOTES)
  gsub("</", "<\\/", jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA), fixed = TRUE)
})

# How a match differs, in words: "15.2 cm shorter · 33.1 kg heavier"
diff_words <- function(dh_cm, dw_kg, units) {
  if (units == "metric") { hv <- round(dh_cm, 1); wv <- round(dw_kg, 1); hu <- " cm"; wu <- " kg" }
  else { hv <- round(cm_to_in(dh_cm), 1); wv <- round(kg_to_lb(dw_kg)); hu <- "″"; wu <- " lb" }
  if (hv == 0 && wv == 0) return("same listing")
  h <- if (hv == 0) "same height" else sprintf("%s%s %s", fmt1(abs(hv)), hu, if (hv > 0) "taller" else "shorter")
  w <- if (wv == 0) "same weight" else sprintf("%s%s %s", fmt1(abs(wv)), wu, if (wv > 0) "heavier" else "lighter")
  paste(h, w, sep = "<br>")
}

# The athlete in a sport whose listed size is closest (height and weight in standard deviations).
# Ties are common, since many players share a listed size: the first by name stands for them all.
nearest <- function(h, w, sport, exclude = 0L) {
  rows <- SPORT_ROWS[[sport]]; rows <- rows[rows != exclude]
  dd <- ((P$h[rows] - h) / SD_H)^2 + ((P$w[rows] - w) / SD_W)^2
  best <- rows[dd <= min(dd) + 1e-12]
  list(i = best[1], n = length(best), d = sqrt(min(dd)))
}
FAR <- 0.75                                        # past this many standard deviations, the closest build isn't close
sport_word <- function(k) if (k == "soccer") "soccer" else SPORT_LABEL[[k]]

# One "built like" row. For a player, the right column says how the match differs. For "you" it gives the match's own
# listing instead: your entry isn't a listing, so "0.3 cm taller" would be noise.
twin_row <- function(t, h, w, units, you = FALSE) {
  r <- P[t$i]; far <- t$d >= FAR
  right <- if (you) { s <- size_txt(r$h, r$w, r$hin, r$lb, units); sprintf("%s%s<br>%s %s", s$h, if (nzchar(s$hu)) paste0(" ", s$hu) else "", s$w, s$wu) }
           else diff_words(r$h - h, r$w - w, units)
  sprintf('<button type="button" class="twin%s" data-pin="%d"><span class="twin__sport">%s%s</span><span class="twin__who"><b>%s</b><small>%s · %s%s</small>%s</span><span class="twin__diff">%s</span></button>',
          if (far) " twin--far" else "", r$i, dot(r$sport), SPORT_LABEL[[r$sport]], esc(r$name), esc(r$position), esc(r$team),
          if (t$n > 1) sprintf(" · and %d more at this size", t$n - 1) else "",
          if (far) sprintf("<em>Nearest %s build, but nobody's close</em>", sport_word(r$sport)) else "", right)
}

reads_html <- function(s) {
  sprintf('<div class="reads"><div class="read"><span>Height</span><b>%s<small>%s</small></b><em>%s</em></div><div class="read"><span>Weight</span><b>%s<small>%s</small></b><em>%s</em></div></div>',
          s$h, s$hu, s$alt_h, s$w, s$wu, s$alt_w)
}

# How a size compares with a group: the share listed strictly below it. For a player inside the group the extremes get
# words ("Tallest"), so nobody is "taller than 100%" of a league they're in. "You" are outside every group, so 0-100% stands.
measure_cell <- function(v, all, top, bottom, member) {
  below <- sum(all < v); above <- sum(all > v); tied <- sum(all == v) > 1
  p <- floor(100 * below / length(all) + 0.5)
  txt <- if (member && above == 0) sprintf("<b>%s</b>%s", top, if (tied) " (tied)" else "")
    else if (member && below == 0) sprintf("<b>%s</b>%s", bottom, if (tied) " (tied)" else "")
    else sprintf("<b>%d%%</b>", if (member) min(max(p, 1L), 99L) else p)
  sprintf('<td><i style="--p:%d%%"></i>%s</td>', if (member && above == 0) 100L else as.integer(p), txt)
}
measure_table <- function(h, w, groups, member) {
  rows <- vapply(seq_along(groups), function(k) {
    g <- groups[[k]]
    sprintf('<tr%s><th scope="row">%s</th>%s%s</tr>', if (k == length(groups)) ' class="all"' else "", names(groups)[k],
            measure_cell(h, P$h[g], "Tallest", "Shortest", member), measure_cell(w, P$w[g], "Heaviest", "Lightest", member))
  }, "")
  sprintf('<table><thead><tr><th scope="col"><span class="sr-only">Compared with</span></th><th scope="col">Taller than</th><th scope="col">Heavier than</th></tr></thead><tbody>%s</tbody></table>',
          paste(rows, collapse = ""))
}
ALL_ROW <- sprintf("All %s", fmt_n(N))

# ---- contours: where each league's players gather ----------------------------------------------------
# A smoothed 2D density of listed sizes (kernel density estimate), and the lines that enclose the densest half and
# nine in ten of the league's players in view: a highest-density region, found from the density at each player.
league_contours <- function(rows, shares = c(core = .5, outer = .9)) {
  if (length(rows) < 25) return(NULL)
  x <- P$w[rows]; y <- P$h[rows]
  bw <- pmax(c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y)), c(4, 4))   # floors keep tiny groups from fragmenting
  k <- MASS::kde2d(x, y, h = bw, n = 110, lims = c(30, 190, 140, 250))
  dens <- k$z[cbind(findInterval(x, k$x, all.inside = TRUE), findInterval(y, k$y, all.inside = TRUE))]
  lapply(shares, function(sh) {
    lines <- grDevices::contourLines(k$x, k$y, k$z, levels = quantile(dens, 1 - sh, names = FALSE))
    lapply(lines, function(l) list(w = I(round(l$x, 1)), h = I(round(l$y, 1))))
  })
}

# ---- cards --------------------------------------------------------------------------------------------
player_card <- function(i, units, vis) {
  r <- P[i]
  s <- size_txt(r$h, r$w, r$hin, r$lb, units)
  others <- setdiff(SPORT_ORDER, r$sport)
  twins <- paste(vapply(others, function(k) twin_row(nearest(r$h, r$w, k, i), r$h, r$w, units), ""), collapse = "")
  peers <- P[h == r$h & w == r$w & (i %in% vis | i == r$i)]
  peers_html <- if (nrow(peers) > 1) sprintf('<section class="peers"><h3>%d players at exactly this size</h3><div class="peers__list">%s</div></section>',
    nrow(peers), paste(sprintf('<button type="button" class="peer" data-pin="%d" aria-pressed="%s">%s%s<small>%s</small></button>',
                               peers$i, tolower(peers$i == r$i), vapply(peers$sport, dot, ""), esc(peers$name), LEAGUE_SHORT[peers$league]), collapse = "")) else ""
  HTML(sprintf('<article class="slip" data-sport="%s">
    <button type="button" class="slip__x" data-pin="0" aria-label="Unpin %s">%s</button>
    <h2 class="slip__name">%s</h2><p class="slip__meta">%s%s · %s · %s · %s roster</p>%s
    <section class="among"><h3>How they measure up</h3>%s</section>
    <section class="twins"><h3>Built like</h3><p class="slip__hint">The closest listed size in each other sport.</p>%s</section>%s
    <div class="slip__foot">%s<button type="button" class="slip__copy" data-copy>Copy link to this player</button></div></article>',
    r$sport, esc(r$name), X_ICON, esc(r$name), dot(r$sport), esc(r$league), esc(r$team), esc(r$position), esc(r$season), reads_html(s),
    measure_table(r$h, r$w, setNames(list(LEAGUE_ROWS[[r$league]], P$i), c(paste0(dot(r$sport), esc(r$league)), ALL_ROW)), TRUE),
    twins, peers_html,
    if (nzchar(r$url %||% "")) sprintf('<a class="slip__src" href="%s" target="_blank" rel="noopener">ESPN profile ↗</a>', esc(r$url)) else ""))
}

me_card <- function(m, units) {
  s <- size_txt(m[1], m[2], cm_to_in(m[1]), kg_to_lb(m[2]), units)
  twins <- paste(vapply(SPORT_ORDER, function(k) twin_row(nearest(m[1], m[2], k), m[1], m[2], units, you = TRUE), ""), collapse = "")
  among <- measure_table(m[1], m[2], c(setNames(LEAGUE_ROWS[LEAGUES], paste0(vapply(LEAGUE_SPORT[LEAGUES], dot, ""), LEAGUES)),
                                        setNames(list(P$i), ALL_ROW)), FALSE)
  HTML(sprintf('<article class="slip slip--me">
    <button type="button" class="slip__x" data-clear-me aria-label="Take yourself off the wall">%s</button>
    <h2 class="slip__name">Your build</h2><p class="slip__meta"><i class="dot dot--me"></i>Pencilled on the wall</p>%s
    <section class="twins"><h3>Built like you</h3><p class="slip__hint">The pro in each sport whose listed size is closest to yours, and their listing.</p>%s</section>
    <section class="among"><h3>Among the pros</h3>%s</section>
    <div class="slip__foot"><button type="button" class="slip__copy" data-copy>Copy link to my build</button></div></article>', X_ICON, reads_html(s), twins, among))
}

empty_card <- function() {
  famous <- paste(sprintf('<button type="button" class="famous" data-pin="%d">%s<b>%s</b><small>%s</small></button>',
                          FAMOUS, vapply(P$sport[FAMOUS], dot, ""), esc(P$name[FAMOUS]), LEAGUE_SHORT[P$league[FAMOUS]]), collapse = "")
  HTML(sprintf('<article class="slip slip--empty">
    <h2 class="slip__name">Every dot is a player</h2>
    <p class="slip__lede"><span class="hint--pointer">Hover the grid to read a player off the tape. Click to pin them here,</span><span class="hint--touch">Tap a dot to read a player off the tape and pin them here,</span> with the closest build in every other sport.</p>
    <section><h3>Try a famous build</h3><div class="famous__list">%s</div></section>
    <p class="slip__hint">Or put yourself on the wall: choose your height and weight above the grid.</p></article>', famous))
}

# ---- "you": height and weight selects whose values carry their unit, so a units switch can never misread them
ME_H <- list(metric = 155:230, imperial = 61:90)
ME_W <- list(metric = 45:178, imperial = 100:390)
me_choices <- function(what, units) {
  if (what == "h") v <- ME_H[[units]] else v <- ME_W[[units]]
  u <- if (what == "h") c(metric = "cm", imperial = "in")[[units]] else c(metric = "kg", imperial = "lb")[[units]]
  lab <- if (what == "h" && units == "imperial") ft_in(v) else paste(v, u)
  c(setNames("", if (what == "h") "height" else "weight"), setNames(paste0(u, ":", v), lab))
}
me_parse <- function(x) {
  if (is.null(x) || !nzchar(x)) return(NA_real_)
  u <- sub(":.*", "", x); v <- suppressWarnings(as.numeric(sub(".*:", "", x)))
  switch(u, cm = v, kg = v, `in` = v * 2.54, lb = v * 0.45359237, NA_real_)
}
me_nearest <- function(metric_value, what, units) {
  if (is.na(metric_value)) return("")
  v <- if (what == "h") ME_H[[units]] else ME_W[[units]]
  x <- if (units == "metric") metric_value else if (what == "h") cm_to_in(metric_value) else kg_to_lb(metric_value)
  u <- if (what == "h") c(metric = "cm", imperial = "in")[[units]] else c(metric = "kg", imperial = "lb")[[units]]
  paste0(u, ":", v[which.min(abs(v - x))])
}
me_select <- function(id, what) {
  ch <- me_choices(what, "metric")
  HTML(sprintf('<span class="ask__pick"><select id="%s" aria-label="Your %s">%s</select></span>', id, if (what == "h") "height" else "weight",
               paste(sprintf('<option value="%s">%s</option>', ch, names(ch)), collapse = "")))
}

# ---- UI -------------------------------------------------------------------------------------------------
# The league chips double as the legend: "All", then one chip per league in its sport's colour, with its count
chips_html <- function() {
  paste0(sprintf('<button type="button" class="chip chip--all" data-league="all" data-state="on" aria-pressed="true">All<small>%s</small></button>', fmt_n(N)),
         paste(sprintf('<button type="button" class="chip" data-league="%s" data-state="all" aria-pressed="false"><i class="dot" style="--c:%s"></i>%s<small>%s</small></button>',
                       LEAGUES, SPORT_COLOR[LEAGUE_SPORT[LEAGUES]], LEAGUE_SHORT[LEAGUES], fmt_n(lengths(LEAGUE_ROWS[LEAGUES]))), collapse = ""))
}
ICON <- list(
  plus = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 5v14M5 12h14"/></svg>',
  minus = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M5 12h14"/></svg>',
  reset = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M4.5 12a7.5 7.5 0 1 0 2.2-5.3M4.5 4.5v4h4"/></svg>',
  full = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M8 3H3v5m13-5h5v5M3 16v5h5m13-5v5h-5"/></svg>',
  contours = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M4 15c0-5 4-9 9-9s7 3 7 6-3 7-8 7-8-1-8-4z"/><path d="M8.5 14c0-2.6 2-4.6 4.6-4.6 2 0 3.4 1.3 3.4 3s-1.6 3.4-4 3.4-4-.6-4-1.8z"/></svg>')

ui <- fluidPage(
  title = "The Shape of Sport — 6,449 athletes by height and weight",
  lang = "en",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = "Every NFL, NBA, MLB, NHL, MLS and Premier League player on one grid of height and weight. Hover to read the tape, find who is built like whom, and where you would fit."),
    tags$meta(name = "theme-color", content = "#f5f1e6"),
    tags$link(rel = "icon", href = "favicon.svg", type = "image/svg+xml"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Big+Shoulders+Display:wght@700;800;900&family=Barlow:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;500;600&family=Caveat:wght@600&display=swap"),
    tags$link(rel = "stylesheet", href = "shape.css"),
    tags$script(id = "athletes", type = "application/json", HTML(DATA_JSON)),
    tags$script(src = "shape.js")
  ),

  div(class = "tape-edge", `aria-hidden` = "true"),
  tags$header(class = "hero",
    h1("The Shape of Sport"),
    a(class = "hero__link", href = "https://jafranceschini.quarto.pub/juan-franceschini-lab/apps.html", target = "_blank", rel = "noopener", "a White Belt project ↗"),
    p(class = "lede", HTML(sprintf("<b>%s</b> pro athletes from six leagues, placed by listed height and weight. Same game, very different bodies.", fmt_n(N)))),
    div(class = "hero__ask",
      p(class = "ask", "I'm ", me_select("me_h", "h"), " and ", span(class = "nowrap", me_select("me_w", "w"), "."), " ", tags$b("Who's built like me?")),
      uiOutput("answer", container = tags$p, class = "answer"))),

  tags$section(class = "bench", `aria-label` = "Find and filter athletes",
    div(class = "find",
      HTML('<svg viewBox="0 0 24 24" aria-hidden="true"><circle cx="10.5" cy="10.5" r="6.5"/><path d="m15.5 15.5 5 5"/></svg>'),
      tags$input(id = "q", type = "search", placeholder = "Find a player or team", autocomplete = "off", spellcheck = "false",
                 role = "combobox", `aria-label` = "Find a player or team", `aria-controls` = "finds", `aria-expanded` = "false", `aria-autocomplete` = "list"),
      tags$kbd("/"),
      div(id = "finds", class = "finds", role = "listbox", hidden = NA)),
    div(class = "chips", role = "group", `aria-label` = "Leagues on the wall", HTML(chips_html())),
    div(class = "pick pick--pos", tags$label(`for` = "pos", "Position"),
      HTML('<select id="pos" disabled><option value="all">Pick one sport first</option></select>')),
    div(class = "units", role = "group", `aria-label` = "Units",
      tags$button(type = "button", class = "is-on", `data-units` = "metric", `aria-pressed` = "true", "cm · kg"),
      tags$button(type = "button", `data-units` = "imperial", `aria-pressed` = "false", "ft · lb"))),

  div(class = "room",
    tags$section(class = "wall", id = "wall", `aria-label` = "Height and weight of every athlete",
      div(class = "wall__head",
        uiOutput("count", container = tags$p, class = "count"),
        div(class = "tools", role = "group", `aria-label` = "Grid tools",
          tags$button(type = "button", id = "contours", class = "tools__pill", `aria-pressed` = "true",
            title = "Outline where each league's players gather", HTML(ICON$contours), "Contours"),
          tags$button(type = "button", id = "spread", class = "tools__switch", role = "switch", `aria-checked` = "true",
            title = "Spread players who share a listed size, within their listed inch and pound", span(class = "sw", `aria-hidden` = "true"), "Spread"),
          span(class = "tools__zoom", id = "zoom", "1.0×"),
          tags$button(type = "button", id = "zoom-in", `aria-label` = "Zoom in", title = "Zoom in", HTML(ICON$plus)),
          tags$button(type = "button", id = "zoom-out", `aria-label` = "Zoom out", title = "Zoom out", HTML(ICON$minus)),
          tags$button(type = "button", id = "zoom-reset", `aria-label` = "Reset the view", title = "Reset the view", HTML(ICON$reset)),
          tags$button(type = "button", id = "full", `aria-label` = "Full screen", title = "Full screen", HTML(ICON$full)))),
      div(class = "board", id = "board",
        tags$canvas(id = "plot", tabindex = "0", role = "img", `aria-label` = "Height against weight for every athlete", `aria-describedby` = "how-to"),
        tags$canvas(id = "overlay", `aria-hidden` = "true"),
        div(id = "tip", class = "tip", hidden = NA),
        div(id = "board-msg", class = "board__msg", hidden = NA),
        div(id = "contour-key", class = "contour-key", hidden = NA,
          span(class = "ck-on", tags$i(class = "ck ck--core"), "half of each league"), span(class = "ck-on", tags$i(class = "ck ck--outer"), "nine in ten"),
          span(class = "ck-none", "Too few players in view for contours")),
        tags$button(type = "button", id = "pinbar", class = "pinbar", hidden = NA)),
      p(class = "wall__foot", id = "how-to",
        span(class = "hint--pointer", "Hover a dot to read the tape · click to pin · pinch, or click the grid and scroll, to zoom · drag to pan · / to search"),
        span(class = "hint--touch", "Tap a dot to pin it · zoom with + and −, then drag to pan"))),
    tags$aside(class = "card", id = "spotlight", `aria-label` = "Player details", uiOutput("card"))),

  tags$footer(class = "foot",
    tags$details(class = "how", tags$summary("How it works"),
      p(tags$b("Every dot is a player"), sprintf(" on a %s roster in the NFL, NBA, MLB, NHL, MLS or Premier League, from ESPN's public team rosters. Of %s players listed, %s have both a height and a weight. The %s without one, and one goalkeeper listed at 432 lb, are left out. Nothing is estimated.",
        trimws(format(SNAPSHOT, "%e %B %Y")), fmt_n(d$records), fmt_n(N), fmt_n(sum(d$excluded) - 1L))),
      p(tags$b("Listed, not measured."), " Heights are listed in whole inches, which is why the dots line up in rows. Listings can be rounded or out of date, and teams measure differently (with or without shoes, for one)."),
      p(tags$b("Dots are spread, a little."), sprintf(" Heights are listed in whole inches and weights in whole pounds, so %s players share their exact listing with someone, up to %d on a single spot. To show the crowd, each dot is nudged by a fixed random amount of less than half an inch and half a pound, so it always stays inside its own listing: a 6′0″ player sits between 5′11½″ and 6′0½″. The tape always reads the listed value. Turn the Spread switch above the grid off to see exact positions, and pin a dot to see everyone listed at that size.",
        fmt_n(P[, .N, by = .(h, w)][N > 1, sum(N)]), P[, .N, by = .(h, w)][, max(N)])),
      p(tags$b("Contours"), " (on when you arrive) outline where each league's players gather. The solid line encloses the densest half of the league's players in view, the dashed line nine in ten. They come from a smoothed density of listed sizes (a kernel density estimate), follow your filters, and need at least 25 players from a league to draw."),
      p(tags$b("Built like"), " finds, in each sport, the player whose listed size is closest. Height and weight are each measured against their spread across all players, so neither one dominates. Similar size says nothing about skill."),
      p(tags$b("Taller than 80%"), sprintf(" means 80%% of the group is listed shorter: the player's own league, or all %s players across the six leagues. Rosters", fmt_n(N)), " include reserve, injured and preseason players, so being on one doesn't mean playing. Soccer here is MLS and the Premier League, men's competitions only."),
      HTML(sprintf('<table class="how__cov"><thead><tr><th scope="col">League</th><th scope="col">Teams</th><th scope="col">Listed</th><th scope="col">On the grid</th></tr></thead><tbody>%s</tbody></table>',
        paste(sprintf("<tr><th scope=\"row\">%s</th><td>%d</td><td>%s</td><td>%s</td></tr>", d$coverage$league, d$coverage$teams, fmt_n(d$coverage$records), fmt_n(d$coverage$usable)), collapse = ""))),
      p(downloadLink("csv", "Download the data (CSV) ↓"))),
    p("Built by ", a(href = "https://jafranceschini.quarto.pub/juan-franceschini-lab/", target = "_blank", rel = "noopener", "Juan Franceschini"), " · a White Belt project"),
    p(class = "foot__small", sprintf("Data: ESPN public team roster feeds, collected %s. Listed measurements, not independently verified. Inspired by ", trimws(format(SNAPSHOT, "%e %B %Y"))),
      a(href = "https://sportchart.wordpress.com/2014/05/30/athlete-sizes-update/", target = "_blank", rel = "noopener", "SPORTCHART's athlete-size charts", .noWS = "after"),
      ". Not affiliated with ESPN or any league.")),

  div(id = "sr", class = "sr-only", role = "status", `aria-live` = "polite")
)

# ---- server ---------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  leagues <- reactiveVal(LEAGUES)                    # the leagues on the wall
  pinned <- reactiveVal(NA_integer_)
  me_h <- reactiveVal(NA_real_); me_w <- reactiveVal(NA_real_)     # "you", always in cm and kg
  pending_pos <- reactiveVal(NULL)                   # a position to apply when the menu is next rebuilt (URL, reset)
  menu_tick <- reactiveVal(0)
  url_contours <- reactiveVal(TRUE)                  # contours lead: on unless a link says otherwise
  url_units <- reactiveVal("metric")                 # until the browser reports its toggle, a shared link's units win
  units <- reactive(if (identical(input$units %||% url_units(), "imperial")) "imperial" else "metric")

  send_pin <- function(i) session$sendCustomMessage("pin", list(i = if (is.na(i)) -1L else i - 1L))

  observeEvent(TRUE, once = TRUE, {
    q <- parseQueryString(isolate(session$clientData$url_search))
    lg <- if (!is.null(q$leagues)) intersect(LEAGUES, strsplit(q$leagues, ",")[[1]])
          else if (!is.null(q$league)) intersect(LEAGUES, q$league)                                      # older links
          else if (!is.null(q$sports)) LEAGUES[LEAGUE_SPORT[LEAGUES] %in% strsplit(q$sports, ",")[[1]]]
    if (length(lg)) leagues(lg)
    if (!is.null(q$pos)) pending_pos(q$pos)
    if (identical(q$contours, "0")) { url_contours(FALSE); session$sendCustomMessage("contours", list(on = FALSE)) }
    if (identical(q$units, "imperial")) { url_units("imperial"); session$sendCustomMessage("units", "imperial") }
    if (!is.null(q$me) && grepl("^[0-9.]+x[0-9.]+$", q$me)) {
      v <- as.numeric(strsplit(q$me, "x")[[1]]); u <- if (identical(q$units, "imperial")) "imperial" else "metric"
      me_h(v[1]); me_w(v[2])
      updateSelectInput(session, "me_h", choices = me_choices("h", u), selected = me_nearest(v[1], "h", u))
      updateSelectInput(session, "me_w", choices = me_choices("w", u), selected = me_nearest(v[2], "w", u))
    }
    if (!is.null(q$p)) { i <- P[key == q$p]$i; if (length(i)) { pinned(i); send_pin(i) } }
  })

  # ---- filters: one set of leagues. From "All", a chip picks just its league; after that chips add and remove,
  # and taking off the last one goes back to all.
  observeEvent(input$league_pick, {
    l <- input$league_pick$l; cur <- leagues()
    leagues(if (identical(l, "all") || !(l %in% LEAGUES)) LEAGUES
            else if (length(cur) == length(LEAGUES)) l
            else if (l %in% cur) { if (length(cur) == 1) LEAGUES else setdiff(cur, l) }
            else intersect(LEAGUES, c(cur, l)))
  })
  observe(session$sendCustomMessage("chips", I(leagues())))

  observeEvent(input$reset, {
    leagues(LEAGUES)
    pending_pos("all"); menu_tick(menu_tick() + 1)       # rebuild the menu now, so it can't restore the old position
    updateTextInput(session, "q", value = "")
  })

  # Positions only mean something within one sport, so the menu opens when the leagues in view share a sport
  one_sport <- reactive({ s <- unique(LEAGUE_SPORT[leagues()]); if (length(s) == 1) s else NA_character_ })
  observe({
    s <- one_sport(); lg <- leagues(); menu_tick()
    if (is.na(s)) {
      updateSelectInput(session, "pos", choices = c(`Pick one sport first` = "all"), selected = "all")
      session$sendCustomMessage("pos_state", list(on = FALSE)); isolate(pending_pos(NULL)); return()
    }
    cand <- unique(P[league %in% lg, .(pkey, position)])[order(position)]
    keep <- isolate(pending_pos() %||% input$pos %||% "all")
    if (!(keep %in% cand$pkey)) keep <- "all"
    updateSelectInput(session, "pos", choices = c(`All positions` = "all", setNames(cand$pkey, cand$position)), selected = keep)
    session$sendCustomMessage("pos_state", list(on = TRUE))
    isolate(pending_pos(NULL))
  })
  position <- reactive(if (is.na(one_sport())) "all" else input$pos %||% "all")

  query <- reactive(stri_trans_tolower(stri_trans_general(trimws(input$q %||% ""), "Latin-ASCII")))   # the box is debounced in the browser
  visible <- reactive({
    ps <- position(); q <- query()
    m <- P$league %in% leagues()
    if (ps != "all") m <- m & P$pkey == ps
    if (nzchar(q)) m <- m & stri_detect_fixed(P$search, q)
    which(m)
  })
  filtered <- reactive(length(leagues()) < length(LEAGUES) || position() != "all" || nzchar(query()))

  observe({
    v <- visible(); q <- query()
    session$sendCustomMessage("vis", list(all = length(v) == N, idx = if (length(v) == N) NULL else I(v - 1L),
                                          q = nzchar(q), matches = I(head(v, 12) - 1L), total = length(v),
                                          narrowed = length(leagues()) < length(LEAGUES) || position() != "all"))
  })

  contours_on <- reactive(isTRUE(input$contours %||% url_contours()))
  observe({
    if (!contours_on()) return(session$sendCustomMessage("contours", list(on = FALSE)))
    v <- visible(); by_lg <- split(v, P$league[v])
    out <- lapply(intersect(LEAGUES, names(by_lg)), function(l) {
      cl <- league_contours(by_lg[[l]])
      if (!is.null(cl)) list(lg = match(l, LEAGUES) - 1L, n = length(by_lg[[l]]), core = cl$core, outer = cl$outer)
    })
    session$sendCustomMessage("contours", list(on = TRUE, leagues = Filter(Negate(is.null), out)))
  })

  # ---- pins and "you"
  observeEvent(input$pin, {
    i <- suppressWarnings(as.integer(input$pin$i))
    pinned(if (is.na(i) || i < 1 || i > N) NA_integer_ else i)
  })

  # The selects' values carry their unit ("in:71"), so they parse the same whatever the units toggle says.
  # ignoreInit keeps the page's empty first values from wiping a "you" restored from the URL.
  observeEvent(input$me_h, me_h(me_parse(input$me_h)), ignoreInit = TRUE)
  observeEvent(input$me_w, me_w(me_parse(input$me_w)), ignoreInit = TRUE)
  me <- reactive(if (is.na(me_h()) || is.na(me_w())) NULL else c(me_h(), me_w()))
  observe({
    m <- me()
    session$sendCustomMessage("me", if (is.null(m)) list(on = FALSE) else list(on = TRUE, h = m[1], w = m[2]))
  })
  observeEvent(input$me_clear, {
    me_h(NA_real_); me_w(NA_real_)
    updateSelectInput(session, "me_h", selected = ""); updateSelectInput(session, "me_w", selected = "")
  })
  # Switching units relabels the selects in the new units, keeping the nearest value
  observeEvent(units(), {
    u <- units()
    updateSelectInput(session, "me_h", choices = me_choices("h", u), selected = me_nearest(isolate(me_h()), "h", u))
    updateSelectInput(session, "me_w", choices = me_choices("w", u), selected = me_nearest(isolate(me_w()), "w", u))
  }, ignoreInit = TRUE)

  observe({
    lg <- leagues(); ps <- position(); i <- pinned(); m <- me(); co <- contours_on()
    updateQueryString(paste0("?",
      paste(c(if (length(lg) < length(LEAGUES)) paste0("leagues=", utils::URLencode(paste(lg, collapse = ","), TRUE)),
              if (ps != "all") paste0("pos=", utils::URLencode(ps, TRUE)), if (units() == "imperial") "units=imperial", if (!co) "contours=0",
              if (!is.null(m)) sprintf("me=%sx%s", fmt1(m[1]), fmt1(m[2])), if (!is.na(i)) paste0("p=", utils::URLencode(P$key[i], TRUE))), collapse = "&")),
      mode = "replace")
  })

  # ---- outputs
  output$count <- renderUI({
    v <- visible()
    HTML(sprintf("<b>%s</b> %s%s", fmt_n(length(v)), if (length(v) == 1) "athlete" else "athletes",
                 if (filtered()) ' <button type="button" class="linkish" data-reset>Show everyone</button>' else " · one grid"))
  })

  output$answer <- renderUI({
    m <- me(); req(m)
    best <- lapply(SPORT_ORDER, function(k) nearest(m[1], m[2], k))
    t <- best[[which.min(vapply(best, `[[`, 0, "d"))]]; r <- P[t$i]
    HTML(sprintf('Closest build in the pros: <button type="button" class="linkish" data-pin="%d">%s</button>, %s %s%s.',
                 r$i, esc(r$name), esc(r$team), esc(tolower(r$position)),
                 if (abs(r$h - m[1]) < .5 && abs(r$w - m[2]) < .5) ", listed at your size" else ""))
  })

  output$card <- renderUI({
    i <- pinned(); u <- units(); m <- me()
    if (!is.na(i)) player_card(i, u, visible()) else if (!is.null(m)) me_card(m, u) else empty_card()
  })

  output$csv <- downloadHandler(
    filename = function() sprintf("athletes_height_weight_%s.csv", SNAPSHOT),
    content = function(file) file.copy("data/snapshot-2026-10-17/athletes_height_weight.csv", file),
    contentType = "text/csv")
}

shinyApp(ui, server)
