# Premier League Scorigami — every scoreline since 1992: which happened once, which never.
# Data: pl_history.rds, refreshed weekly by update_data.R (GitHub Actions).
# The previous version of this app is in git history (commit b5049f5).

library(shiny)

`%||%` <- function(a, b) if (is.null(a) || !length(a) || identical(a, "")) b else a

# ---- data -------------------------------------------------------------------------
raw <- readRDS("pl_history.rds")
d <- data.frame(
  date = as.Date(raw$Date_Parsed %||% raw$Date),
  home = raw$Home, away = raw$Away,
  hg = as.integer(raw$HG), ag = as.integer(raw$AG)
)
d$season <- ifelse(as.integer(format(d$date, "%m")) >= 8, as.integer(format(d$date, "%Y")), as.integer(format(d$date, "%Y")) - 1L)
d <- d[order(d$date, d$home), ]

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
updated_on <- max(d$date)

# First-ever occurrence of each scoreline (all-time, home-away order)
d$key <- paste0(d$hg, "-", d$ag)
firsts <- d[!duplicated(d$key), ]
firsts$order <- seq_len(nrow(firsts))
latest_first <- firsts[nrow(firsts), ]

fmt_date <- function(x) paste(as.integer(format(x, "%d")), format(x, "%b %Y")) # portable "7 Sep 2023"
fmt_n <- function(x) format(x, big.mark = ",")
ordinal <- function(n) paste0(n, if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
yt <- function(m) paste0("https://www.youtube.com/results?search_query=",
                         utils::URLencode(paste(m$home, m$away, m$hg, m$ag, format(m$date, "%Y"), "highlights"), reserved = TRUE))
esc <- function(x) htmltools::htmlEscape(x)

# ---- view helpers -------------------------------------------------------------------

# Matches in view. Every view uses the same axes: rows = home goals, cols = away goals.
# A team view only filters to that team's matches.
view_data <- function(season, team) {
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
timeline_html <- function(team = "", season = "all") {
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
    div(class = "latest",
      span(class = "latest__tag", "Latest scorigami"),
      span(class = "latest__score", sprintf("%s %d–%d %s", latest_first$home, latest_first$hg, latest_first$ag, latest_first$away)),
      span(class = "latest__meta", sprintf("%s · the %s different scoreline in Premier League history", fmt_date(latest_first$date), ordinal(latest_first$order))))
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

  tags$section(class = "card card--timeline", `aria-label` = "Timeline", uiOutput("timeline")),

  div(id = "tip", class = "tip", role = "tooltip", hidden = NA),

  tags$footer(class = "foot",
    p("Built by ", a(href = "https://jafranceschini.quarto.pub/juan-franceschini-lab/", target = "_blank", rel = "noopener", "Juan Franceschini"),
      " · a White Belt project · ", a(href = "https://www.linkedin.com/in/juan-franceschini-uy/", target = "_blank", rel = "noopener", "LinkedIn")),
    p(class = "foot__small", sprintf("Results from football-data.co.uk, updated weekly (latest match: %s). Scorelines are always home–away. Not affiliated with the Premier League.", fmt_date(updated_on))))
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

  view <- reactive(view_data(input$season %||% "all", input$team %||% ""))

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
    tagList(head, HTML(timeline_html(team, season)))
  })
}

shinyApp(ui, server)
