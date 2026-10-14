# Premier League Scorigami — every scoreline since 1992: which happened once, which never.
# Data: pl_history.rds, refreshed weekly by update_data.R (GitHub Actions), plus live results from
# football-data.org when FOOTBALL_DATA_TOKEN is set (scorigami/.Renviron, deployed with the app).
# The previous version of this app is in git history (commit b5049f5).

library(shiny)

`%||%` <- function(a, b) if (is.null(a) || !length(a) || identical(a, "")) b else a

# ---- data -------------------------------------------------------------------------
season_of <- function(date) ifelse(as.integer(format(date, "%m")) >= 8, as.integer(format(date, "%Y")), as.integer(format(date, "%Y")) - 1L)
# One shape for history and live results: each pairing once per season, in date order
prep <- function(x) {
  x$season <- season_of(x$date)
  x <- x[!duplicated(paste(x$season, x$home, x$away)), ]
  x <- x[order(x$date, x$home), ]
  x$key <- paste0(x$hg, "-", x$ag)
  rownames(x) <- NULL
  x
}
raw <- readRDS("pl_history.rds")
d_hist <- prep(data.frame(date = as.Date(raw$Date_Parsed %||% raw$Date), home = raw$Home, away = raw$Away,
                          hg = as.integer(raw$HG), ag = as.integer(raw$AG)))
d <- d_hist     # the filters below are built once, from the history

season_label <- function(y) sprintf("%d-%02d", y, (y + 1) %% 100)
latest_season <- max(d$season)
in_progress <- sum(d$season == latest_season) < 380
season_choices <- c("All-time" = "all", setNames(as.character(sort(unique(d$season), decreasing = TRUE)),
  paste0(season_label(sort(unique(d$season), decreasing = TRUE)),
         ifelse(sort(unique(d$season), decreasing = TRUE) == latest_season & in_progress, " (in progress)", ""))))
teams <- sort(unique(c(d$home, d$away)))
team_choices <- c("All teams" = "", setNames(teams, teams))
# The seasons each team actually played, so the Season list never offers an empty view
team_seasons <- lapply(setNames(teams, teams), function(t) sort(unique(d$season[d$home == t | d$away == t])))
season_choices_for <- function(team) {
  if (!nzchar(team)) return(season_choices)
  season_choices[season_choices == "all" | season_choices %in% as.character(team_seasons[[team]])]
}

# First-ever occurrence of each scoreline (all-time, home-away order)
firsts <- d[!duplicated(d$key), ]
firsts$order <- seq_len(nrow(firsts))
latest_first <- firsts[nrow(firsts), ]

fmt_date <- function(x) paste(as.integer(format(x, "%d")), format(x, "%b %Y")) # portable "7 Sep 2023"
fmt_n <- function(x) format(x, big.mark = ",")
ordinal <- function(n) paste0(n, if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
yt <- function(m) paste0("https://www.youtube.com/results?search_query=",
                         utils::URLencode(paste(m$home, m$away, m$hg, m$ag, format(m$date, "%Y"), "highlights"), reserved = TRUE))
esc <- function(x) htmltools::htmlEscape(x)
short_day <- function(x) paste(format(x, "%a"), as.integer(format(x, "%d")), format(x, "%b"))   # "Sun 12 Oct"

# ---- live results: football-data.org on top of the weekly history ----------------------------
# Fetched at most every 10 minutes and shared by every visitor. Without a token the app uses the weekly data.
if (file.exists(".Renviron")) readRenviron(".Renviron")
norm_team <- function(x) { w <- strsplit(gsub("[^a-z0-9& ]", " ", tolower(x)), " +")[[1]]; paste(w[nzchar(w) & !w %in% c("fc", "afc")], collapse = " ") }
teams_csv <- read.csv("bot/teams.csv", stringsAsFactors = FALSE)               # shared with the X bot
to_app <- setNames(teams_csv$app_name, vapply(teams_csv$api_name, norm_team, ""))
display_names <- setNames(teams_csv$display_name, teams_csv$app_name)
disp <- function(x) ifelse(x %in% names(display_names), display_names[x], x)

live <- new.env()
live$rows <- d_hist[0, c("date", "home", "away", "hg", "ag")]; live$at <- NULL; live$ok <- FALSE; live$data <- d_hist
fetch_live <- function() {
  body <- if (nzchar(Sys.getenv("SCORIGAMI_LIVE_FILE"))) {                       # local testing with a saved response
    jsonlite::fromJSON(Sys.getenv("SCORIGAMI_LIVE_FILE"), simplifyVector = FALSE)
  } else {
    tok <- Sys.getenv("FOOTBALL_DATA_TOKEN"); if (!nzchar(tok)) return(NULL)
    to <- Sys.Date(); from <- max(max(d_hist$date) + 1, to - 9)
    resp <- tryCatch(httr2::request("https://api.football-data.org/v4/competitions/PL/matches") |>
      httr2::req_url_query(status = "FINISHED", dateFrom = format(from), dateTo = format(to)) |>
      httr2::req_headers(`X-Auth-Token` = tok) |> httr2::req_timeout(8) |> httr2::req_perform(), error = function(e) NULL)
    if (is.null(resp)) return(NULL)
    httr2::resp_body_json(resp)
  }
  rows <- lapply(body$matches, function(x) {
    h <- unname(to_app[norm_team(x$homeTeam$name %||% "")]); a <- unname(to_app[norm_team(x$awayTeam$name %||% "")])
    ft <- x$score$fullTime
    if (is.na(h) || is.na(a) || is.null(ft$home) || is.null(ft$away)) return(NULL)   # unknown club: skip, never guess
    kick <- as.POSIXct(x$utcDate, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    data.frame(date = as.Date(format(kick, tz = "Europe/London")), home = h, away = a, hg = as.integer(ft$home), ag = as.integer(ft$away))
  })
  do.call(rbind, c(list(live$rows[0, ]), rows))
}
data_now <- function() {
  if (is.null(live$at) || difftime(Sys.time(), live$at, units = "mins") >= 10) {
    rows <- fetch_live(); live$at <- Sys.time()
    if (!is.null(rows)) {
      live$ok <- TRUE; live$rows <- rows
      live$data <- if (nrow(rows)) prep(rbind(d_hist[, c("date", "home", "away", "hg", "ag")], rows)) else d_hist
    }
  }
  live$data
}

# The latest results (the last four days of matches), each told the way the X bot tells it:
# scorigami or not, how often the score has happened, and each club's last time at that venue
latest_round <- function(x) {
  idx <- which(x$date >= max(x$date) - 3)
  lapply(rev(idx), function(i) {
    m <- x[i, ]; prior <- x[seq_len(i - 1), ]
    same <- prior[prior$hg == m$hg & prior$ag == m$ag, ]
    hp <- same[same$home == m$home, ]; ap <- same[same$away == m$away, ]
    list(m = m, scorigami = !nrow(same), n = nrow(same) + 1, nth = length(unique(prior$key)) + 1,
         last = if (nrow(same)) same[nrow(same), ], home_last = if (nrow(hp)) hp[nrow(hp), ], away_last = if (nrow(ap)) ap[nrow(ap), ])
  })
}
# Standing records: the latest PL scorigami, and the latest team scorigami (a club's first time with a
# result at that venue). In date order, a club's first (venue, score) is its team scorigami.
records_of <- function(x) {
  new_home <- !duplicated(paste(x$home, x$key)); new_away <- !duplicated(paste(x$away, x$key))
  pl <- x[!duplicated(x$key), ]; pl <- pl[nrow(pl), ]
  i <- max(which(new_home | new_away)); t <- x[i, ]
  team <- if (new_home[i] && new_away[i]) "Both teams" else if (new_home[i]) disp(t$home) else disp(t$away)
  what <- if (new_home[i] && new_away[i]) "first for both clubs"
          else paste("first", sprintf("%d–%d", t$hg, t$ag), result_word(new_home[i], t$hg, t$ag))
  list(pl = pl, team_match = t, team = team, what = what)
}

result_word <- function(at_home, h, a) {
  gf <- if (at_home) h else a; ga <- if (at_home) a else h
  paste(if (at_home) "home" else "away", if (gf > ga) "win" else if (gf == ga) "draw" else "loss")
}
result_card <- function(r) {
  m <- r$m; s <- sprintf("%d–%d", m$hg, m$ag)
  team_li <- function(team, at_home, last) {
    what <- paste(s, result_word(at_home, m$hg, m$ag))
    if (is.null(last)) return(sprintf('<li class="rs__team rs__team--new"><span class="rs__venue">%s</span><span><b>Team Scorigami!</b> %s&rsquo;s first %s in the Premier League.</span></li>',
                                     if (at_home) "Home" else "Away", esc(disp(team)), what))
    other <- if (at_home) paste("vs", disp(last$away)) else paste("at", disp(last$home))
    sprintf('<li class="rs__team"><span class="rs__venue">%s</span><span>%s: last %s %s, %s</span></li>',
            if (at_home) "Home" else "Away", esc(disp(team)), what, esc(other), fmt_date(last$date))
  }
  # Two verdicts for every game: PL Scorigami (never in the league) and Team Scorigami (a club's first time
  # with this result at this venue). A PL scorigami is a team scorigami for both clubs.
  new_home <- is.null(r$home_last); new_away <- is.null(r$away_last); team_sg <- new_home || new_away
  who <- if (new_home && new_away) "both teams" else if (new_home) disp(m$home) else if (new_away) disp(m$away) else ""
  verdicts <- paste0('<div class="rs__verdicts">',
    if (r$scorigami) '<span class="vd vd--pl"><i></i>PL Scorigami!</span>' else '<span class="vd"><i></i>No PL scorigami</span>',
    if (team_sg) sprintf('<span class="vd vd--team"><i></i>Team Scorigami! <em>%s</em></span>', esc(who)) else '<span class="vd"><i></i>No team scorigami</span>',
    '</div>')
  league <- if (r$scorigami) sprintf("The first %s in Premier League history: a new square on the grid, the %s different scoreline since 1992.", s, ordinal(r$nth))
            else sprintf("%s has now happened %s times. Most recently %s %s %s, %s.", s, fmt_n(r$n), esc(disp(r$last$home)), s, esc(disp(r$last$away)), fmt_date(r$last$date))
  sprintf('<article class="rs%s" data-score="%d-%d"><header><span class="rs__date">%s</span></header>
    <p class="rs__score"><span>%s</span><b>%s</b><span>%s</span></p>%s<p class="rs__league">%s</p>%s
    <button class="rs__see" type="button" data-score="%d-%d">See %s on the grid</button></article>',
    if (r$scorigami) " rs--sg" else if (team_sg) " rs--team" else "", m$hg, m$ag, short_day(m$date),
    esc(disp(m$home)), s, esc(disp(m$away)), verdicts, league,
    if (r$scorigami) "" else paste0('<ul class="rs__teams">', team_li(m$home, TRUE, r$home_last), team_li(m$away, FALSE, r$away_last), "</ul>"),
    m$hg, m$ag, s)
}

# ---- view helpers -------------------------------------------------------------------

# Matches in view. Every view uses the same axes: rows = home goals, cols = away goals.
# A team view only filters to that team's matches.
view_data <- function(d, season, team) {
  x <- if (season == "all") d else d[d$season == as.integer(season), ]
  if (nzchar(team)) x <- x[x$home == team | x$away == team, ]
  x$r <- x$hg; x$c <- x$ag
  x
}

# Four frequency bins for scorelines seen 2+ times, scaled to the view
bin_breaks <- function(m) {
  if (m < 2) return(c(2, Inf, Inf, Inf))
  nice <- function(x) ifelse(x < 10, round(x), signif(x, 1))   # 2, 10, 50, 300…
  b <- unique(pmax(2, nice(exp(seq(log(2), log(max(m, 3)), length.out = 5)))[1:4]))
  c(b, rep(Inf, 4 - length(b)))
}
bin_of <- function(n, br) ifelse(n == 0, "never", ifelse(n == 1, "once", paste0("b", findInterval(n, br))))

score_line <- function(m) sprintf("%s %d–%d %s", m$home, m$hg, m$ag, m$away)
# Team name as HTML, highlighted when it is the team being viewed
team_html <- function(name, team) if (nzchar(team) && name == team) sprintf('<span class="mt__me">%s</span>', esc(name)) else esc(name)

grid_html <- function(v, team) {
  tab <- table(factor(v$r, 0:9), factor(v$c, 0:9))
  maxg <- max(9, v$r, v$c)
  if (maxg > 9) tab <- table(factor(v$r, 0:maxg), factor(v$c, 0:maxg))
  br <- bin_breaks(max(tab))
  total <- nrow(v)
  rows_lbl <- "Home goals"; cols_lbl <- "Away goals"
  cells <- character()
  for (r in 0:maxg) {
    cells <- c(cells, sprintf('<span class="g__axis g__axis--row">%d</span>', r))
    for (cc in 0:maxg) {
      n <- tab[r + 1, cc + 1]
      sub <- if (n > 0) v[v$r == r & v$c == cc, ] else NULL
      tip <- if (n == 0) sprintf("%d–%d|Never happened%s", r, cc, if (nzchar(team)) paste(" for", team) else " in the Premier League")
        else sprintf("%d–%d|%s %s (%s)|First: %s, %s|%s", r, cc, fmt_n(n), if (n == 1) "match" else "matches",
                     if (100 * n / total < .1) "<0.1%" else sprintf("%.1f%%", 100 * n / total),
                     fmt_date(sub$date[1]), score_line(sub[1, ]),
                     if (n > 1) paste0("Latest: ", fmt_date(sub$date[n]), ", ", score_line(sub[n, ])) else "Happened exactly once: a scorigami")
      cells <- c(cells, sprintf('<button class="g__cell g__cell--%s" type="button" tabindex="-1" data-score="%d-%d" data-tip="%s" aria-label="%d home, %d away: %s">%s</button>',
        bin_of(n, br), r, cc, esc(tip), r, cc, if (n == 0) "never happened" else if (n == 1) "1 match" else paste(fmt_n(n), "matches"), if (n > 0) fmt_n(n) else ""))
    }
  }
  head <- paste0('<span class="g__corner"></span>', paste0(sprintf('<span class="g__axis g__axis--col">%d</span>', 0:maxg), collapse = ""))
  legend_ranges <- function(i) { lo <- br[i]; hi <- if (i < 4 && is.finite(br[i + 1])) br[i + 1] - 1 else NA
    if (!is.finite(lo)) return(NULL); if (is.na(hi)) paste0(fmt_n(lo), "+") else if (hi == lo) fmt_n(lo) else paste0(fmt_n(lo), "–", fmt_n(hi)) }
  legend <- paste0(
    '<span class="lg"><i class="g__cell--never"></i>Never</span>',
    '<span class="lg"><i class="g__cell--once"></i>Once (scorigami)</span>',
    paste0(unlist(lapply(1:4, function(i) { lab <- legend_ranges(i); if (is.null(lab)) NULL else sprintf('<span class="lg"><i class="g__cell--b%d"></i>%s</span>', i, lab) })), collapse = ""))
  HTML(sprintf(
    '<div class="g-wrap"><div class="g__ylabel">%s ↓</div><div class="g__xlabel">%s →</div>
       <div class="g" style="--n:%d">%s%s</div></div><div class="legend" aria-label="Legend">%s</div>',
    esc(rows_lbl), cols_lbl, maxg + 1, head, paste(cells, collapse = ""), legend))
}

# New scorelines per season, for the whole league or one team. Every season keeps its slot;
# seasons the team was not in the league are marked, and the selected season is lit.
timeline_html <- function(d, team = "", season = "all") {
  x <- if (nzchar(team)) d[d$home == team | d$away == team, ] else d
  fx <- x[!duplicated(x$key), ]
  all_s <- sort(unique(d$season))
  played <- all_s %in% unique(x$season)
  n <- tabulate(match(fx$season, all_s), length(all_s))
  m <- max(n, 1)
  bars <- vapply(seq_along(all_s), function(i) {
    lab <- season_label(all_s[i])
    scores <- with(fx[fx$season == all_s[i], ], sprintf("%d–%d", hg, ag))
    tip <- if (!played[i]) sprintf("%s|Not in the Premier League", lab)
           else if (n[i] == 0) sprintf("%s|No new scorelines", lab)
           else sprintf("%s|%d new scoreline%s|%s", lab, n[i], if (n[i] == 1) "" else "s", paste(scores, collapse = ", "))
    # Say what was new right on the bar: the scorelines themselves when there are a few, else the count
    note <- if (n[i] == 0) "" else if (n[i] <= 3) paste0("<span>", scores, "</span>", collapse = "") else sprintf("<span>%d</span>", n[i])
    cls <- paste0("tl__bar", if (!played[i]) " tl__bar--out" else if (n[i] == 0) " tl__bar--zero" else "",
                  if (season != "all" && all_s[i] == as.integer(season)) " is-current" else "")
    # label the first and last seasons and every 5th, skipping any too close to the ends
    yr <- if (i == 1 || i == length(all_s) || (all_s[i] %% 5 == 0 && i > 3 && i < length(all_s) - 2)) substr(lab, 3, 7) else ""
    sprintf('<button type="button" class="%s" data-tip="%s" aria-label="%s" style="--h:%.1f%%"><em>%s</em><i></i><b>%s</b></button>',
            cls, esc(tip), esc(gsub("|", ": ", tip, fixed = TRUE)), 100 * n[i] / m, note, yr)
  }, "")
  sprintf('<div class="tl">%s</div>', paste(bars, collapse = ""))
}

# ---- UI -----------------------------------------------------------------------------------

ui <- fluidPage(
  title = "Premier League Scorigami",
  lang = "en",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = "Every Premier League scoreline since 1992. Which have happened only once, and which have never happened at all?"),
    tags$link(rel = "icon", href = "favicon.svg", type = "image/svg+xml"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@400;600;800;900&family=IBM+Plex+Mono:wght@400;500&display=swap"),
    tags$link(rel = "stylesheet", href = "scorigami.css"),
    tags$script(src = "scorigami.js")
  ),

  tags$header(class = "hero",
    div(class = "hero__mark", `aria-hidden` = "true", HTML(paste(rep("<i></i>", 9), collapse = ""))),
    h1("SCORIGAMI"),
    p(class = "lede", "Every scoreline since 1992. Which have happened only once, and which have ", tags$em("never"), " happened at all?"),
    div(class = "ticker", uiOutput("ticker")),
    div(class = "records-wrap", uiOutput("records"))
  ),

  # Filters and the totals they change, in one row right above the grid
  tags$section(class = "toolbar", `aria-label` = "Filters",
    div(class = "controls",
      selectInput("season", "Season", choices = season_choices, selected = "all"),
      selectizeInput("team", "Team", choices = team_choices, selected = "",
                     options = list(placeholder = "All teams"))),
    uiOutput("stats")
  ),

  div(class = "main",
    tags$section(class = "card card--grid", `aria-label` = "Scoreline grid",
      div(class = "card__head", h2(textOutput("grid_title", inline = TRUE)),
          p(class = "hint",
            span(class = "hint--pointer", "Hover a square for the story, click it for every match. Arrow keys move between squares."),
            span(class = "hint--touch", "Tap a square for every match."))),
      uiOutput("grid")),
    tags$aside(class = "card card--detail", `aria-live` = "polite", uiOutput("detail"))
  ),

  tags$section(class = "card card--latest", id = "latest", `aria-label` = "Latest results", uiOutput("latest")),

  tags$section(class = "card card--timeline", `aria-label` = "Timeline", uiOutput("timeline")),

  div(id = "tip", class = "tip", role = "tooltip", hidden = NA),

  tags$footer(class = "foot",
    p("Built by ", a(href = "https://jafranceschini.quarto.pub/juan-franceschini-lab/", target = "_blank", rel = "noopener", "Juan Franceschini"),
      " · a White Belt project · ", a(href = "https://www.linkedin.com/in/juan-franceschini-uy/", target = "_blank", rel = "noopener", "LinkedIn")),
    p(class = "foot__small", "Results from football-data.co.uk, refreshed weekly, and live from football-data.org. Scorelines are always home–away. Not affiliated with the Premier League."))
)

# ---- server ----------------------------------------------------------------------------------

server <- function(input, output, session) {
  score <- reactiveVal(paste0(latest_first$hg, "-", latest_first$ag))

  observeEvent(TRUE, once = TRUE, {
    q <- parseQueryString(isolate(session$clientData$url_search))
    if (!is.null(q$season) && q$season %in% season_choices) updateSelectInput(session, "season", selected = q$season)
    if (!is.null(q$team) && q$team %in% teams) updateSelectizeInput(session, "team", selected = q$team)
    if (!is.null(q$score) && grepl("^[0-9]{1,2}-[0-9]{1,2}$", q$score)) score(q$score)
  })

  # Picking a team narrows the Season list to the seasons it played (keeping the current one if it can)
  observeEvent(input$team, {
    ch <- season_choices_for(input$team %||% "")
    sel <- if ((input$season %||% "all") %in% ch) input$season else "all"
    updateSelectInput(session, "season", choices = ch, selected = sel)
  }, ignoreInit = TRUE)

  observeEvent(input$cell, score(input$cell))
  observe(updateQueryString(paste0("?season=", input$season %||% "all",
                                   if (nzchar(input$team %||% "")) paste0("&team=", utils::URLencode(input$team, reserved = TRUE)) else "",
                                   "&score=", score()), mode = "replace"))

  dat <- reactive({ invalidateLater(10 * 60 * 1000); data_now() })               # live results every 10 minutes
  view <- reactive(view_data(dat(), input$season %||% "all", input$team %||% ""))
  recent <- reactive(latest_round(dat()))

  output$ticker <- renderUI({
    rr <- recent(); x <- dat()
    days <- range(vapply(rr, function(r) as.numeric(r$m$date), 0))
    when <- if (days[1] == days[2]) short_day(as.Date(days[1])) else paste(short_day(as.Date(days[1])), "–", short_day(as.Date(days[2])))
    sg <- Filter(function(r) r$scorigami, rr)
    # Clubs with a team scorigami this round (not counting PL scorigamis, which cover both clubs)
    team_clubs <- unlist(lapply(Filter(function(r) !r$scorigami, rr), function(r)
      c(if (is.null(r$home_last)) disp(r$m$home), if (is.null(r$away_last)) disp(r$m$away))))
    pl <- if (length(sg)) sprintf("PL SCORIGAMI! %s %d–%d %s", disp(sg[[1]]$m$home), sg[[1]]$m$hg, sg[[1]]$m$ag, disp(sg[[1]]$m$away))
          else "No PL scorigami"
    team <- if (length(team_clubs)) paste("TEAM SCORIGAMI!", paste(team_clubs, collapse = ", ")) else "No team scorigami"
    verdict <- paste(pl, "·", team)
    a(class = paste("latest", if (length(sg)) "latest--sg"), href = "#latest",
      span(class = "latest__tag", "Latest results"),
      span(class = "latest__score", sprintf("%s · %d result%s", when, length(rr), if (length(rr) == 1) "" else "s")),
      span(class = "latest__meta", verdict))
  })

  output$records <- renderUI({
    r <- records_of(dat()); p <- r$pl; t <- r$team_match
    rec <- function(cls, label, who, m, extra) tags$button(class = paste("rec", cls), type = "button", `data-score` = sprintf("%d-%d", m$hg, m$ag),
      span(class = "rec__lbl", label), who,
      span(class = "rec__game", disp(m$home), tags$b(sprintf("%d–%d", m$hg, m$ag)), disp(m$away)),
      span(class = "rec__meta", extra))
    div(class = "records",
      rec("rec--pl", "Last PL scorigami", NULL, p, fmt_date(p$date)),
      rec("rec--team", "Last team scorigami", tags$em(r$team), t, paste0(r$what, " · ", fmt_date(t$date))))
  })

  output$latest <- renderUI({
    rr <- recent()
    src <- if (live$ok) sprintf("Live from football-data.org, checked %s UK time.", format(live$at, "%H:%M", tz = "Europe/London"))
           else sprintf("From the weekly data (latest match %s).", fmt_date(max(dat()$date)))
    tagList(div(class = "card__head", h2("Latest results"),
                p(class = "hint", "Every result of the last few days, checked against Premier League history. ", src)),
            HTML(sprintf('<div class="rs-grid">%s</div>', paste(vapply(rr, result_card, ""), collapse = ""))))
  })

  output$grid_title <- renderText({
    s <- if ((input$season %||% "all") == "all") "All-time" else season_label(as.integer(input$season))
    if (nzchar(input$team %||% "")) paste(input$team, "·", s) else paste("Every scoreline ·", s)
  })

  output$stats <- renderUI({
    v <- view(); n <- nrow(v)
    k <- table(paste0(v$r, "-", v$c))
    stat <- function(val, lbl, cls = NULL) span(class = paste(c("stat", cls), collapse = " "), tags$b(val), lbl)
    div(class = "stats",
      stat(fmt_n(n), if (n == 1) "match" else "matches"),
      stat(if (n) sprintf("%.2f", mean(v$r + v$c)) else "–", "goals a game"),
      stat(length(k), "different scorelines"),
      stat(sum(k == 1), "happened once", "stat--once"))
  })

  output$grid <- renderUI({
    v <- view(); team <- input$team %||% ""
    if (!nrow(v)) {
      s <- suppressWarnings(as.integer(input$season))
      if (nzchar(team) && !is.na(s)) {
        near <- team_seasons[[team]][which.min(abs(team_seasons[[team]] - s))]
        return(p(class = "empty", sprintf("%s weren't in the Premier League in %s. Their nearest season is %s.", team, season_label(s), season_label(near))))
      }
      return(p(class = "empty", "No matches in this view."))
    }
    grid_html(v, team)
  })

  output$detail <- renderUI({
    v <- view(); team <- input$team %||% ""
    parts <- as.integer(strsplit(score(), "-")[[1]])
    m <- v[v$r == parts[1] & v$c == parts[2], ]
    m <- m[order(m$date, decreasing = TRUE), ]
    heading <- div(class = "detail__head",
      span(class = "detail__score", sprintf("%d–%d", parts[1], parts[2])),
      span(class = "detail__count", if (!nrow(m)) "Never happened" else if (nrow(m) == 1) "Happened once" else paste(fmt_n(nrow(m)), "matches")))
    back <- tags$button(class = "detail__back", type = "button", "Back to the grid")
    if (!nrow(m)) {
      return(tagList(heading, p(class = "detail__never",
        if (nzchar(team)) sprintf("No %s match in this view has ended %d–%d.", team, parts[1], parts[2])
        else sprintf("No Premier League match has ever ended %d–%d%s. Yet.", parts[1], parts[2], if ((input$season %||% "all") == "all") "" else " this season")),
        div(class = "detail__foot", back)))
    }
    first <- m[nrow(m), ]
    shown <- head(m, 40)
    rows <- vapply(seq_len(nrow(shown)), function(i) {
      x <- shown[i, ]
      sprintf('<li><span class="mt__date">%s<small>%s</small></span><span class="mt__teams">%s <b>%d–%d</b> %s</span><a class="mt__yt" href="%s" target="_blank" rel="noopener" aria-label="Search highlights for %s">▶</a></li>',
              fmt_date(x$date), season_label(x$season), team_html(x$home, team), x$hg, x$ag, team_html(x$away, team), yt(x), esc(score_line(x)))
    }, "")
    # Phones show the latest 5 first; the rest is one tap away
    more <- if (nrow(shown) > 5) tags$button(class = "mt__all", type = "button",
              if (nrow(m) <= 40) sprintf("Show all %d matches", nrow(m)) else sprintf("Show the latest 40 of %s", fmt_n(nrow(m))))
    tagList(heading,
      if (nrow(m) > 1) p(class = "detail__first", HTML(sprintf("First: %s, %s %d–%d %s", fmt_date(first$date),
                                                      team_html(first$home, team), first$hg, first$ag, team_html(first$away, team)))),
      HTML(sprintf('<ol class="mt">%s</ol>', paste(rows, collapse = ""))),
      div(class = "detail__foot", more, back),
      if (nrow(m) > 40) p(class = "detail__more", sprintf("Showing the latest 40 of %s.", fmt_n(nrow(m)))))
  })

  output$timeline <- renderUI({
    team <- input$team %||% ""; season <- input$season %||% "all"
    d <- dat()
    x <- if (nzchar(team)) d[d$home == team | d$away == team, ] else d
    fx <- x[!duplicated(x$key), ]
    first_s <- min(x$season)
    n_first <- sum(fx$season == first_s); n_late <- sum(fx$season >= 2015)
    late <- if (n_late == 0) "none since 2015" else sprintf("just %d since 2015", n_late)
    head <- if (nzchar(team))
      div(class = "card__head", h2(sprintf("When each %s scoreline first happened", team)),
          p(class = "hint", sprintf("Only matches involving %s. %d new scorelines in their first season (%s); %s.", team, n_first, season_label(first_s), late)))
    else
      div(class = "card__head", h2("When each scoreline first happened"),
          p(class = "hint", sprintf("League-wide, every season. %d in the first season alone; %s.", n_first, late)))
    tagList(head, HTML(timeline_html(d, team, season)))
  })
}

shinyApp(ui, server)
