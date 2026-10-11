# Premier League Scorigami — every scoreline since 1993: which happened once, which never.
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
team_choices <- c("Every match (home vs away)" = "", setNames(teams, teams))
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
        else sprintf("%d–%d|%s %s (%.1f%%)|First: %s, %s|%s", r, cc, fmt_n(n), if (n == 1) "match" else "matches", 100 * n / total,
                     fmt_date(sub$date[1]), score_line(sub[1, ]),
                     if (n > 1) paste0("Latest: ", fmt_date(sub$date[n]), ", ", score_line(sub[n, ])) else "Happened exactly once: a scorigami")
      cells <- c(cells, sprintf('<button class="g__cell g__cell--%s" type="button" data-score="%d-%d" data-tip="%s" aria-label="%d to %d: %s">%s</button>',
        bin_of(n, br), r, cc, esc(tip), r, cc, if (n == 0) "never happened" else paste(n, "matches"), if (n > 0) fmt_n(n) else ""))
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

timeline_html <- function() {
  firsts$season <- ifelse(as.integer(format(firsts$date, "%m")) >= 8, as.integer(format(firsts$date, "%Y")), as.integer(format(firsts$date, "%Y")) - 1L)
  all_s <- sort(unique(d$season))
  n <- tabulate(match(firsts$season, all_s), length(all_s))
  m <- max(n)
  bars <- vapply(seq_along(all_s), function(i) {
    new <- firsts[firsts$season == all_s[i], ]
    tip <- if (n[i] == 0) sprintf("%s|No new scorelines", season_label(all_s[i]))
           else sprintf("%s|%d new scoreline%s|%s", season_label(all_s[i]), n[i], if (n[i] == 1) "" else "s",
                        paste(sprintf("%d–%d", new$hg, new$ag), collapse = ", "))
    sprintf('<span class="tl__bar%s" data-tip="%s" style="--h:%.1f%%"><i></i><b>%s</b></span>', if (n[i] == 0) " tl__bar--zero" else "", esc(tip), 100 * n[i] / m,
            # label the first and last seasons and every 5th, skipping any too close to the ends
            if (i == 1 || i == length(all_s) || (all_s[i] %% 5 == 0 && i > 3 && i < length(all_s) - 2)) substr(season_label(all_s[i]), 3, 7) else "")
  }, "")
  HTML(sprintf('<div class="tl" role="img" aria-label="New scorelines per season, from %d in 1993-94 to %d in %s">%s</div>',
               n[1], n[length(n)], season_label(max(all_s)), paste(bars, collapse = "")))
}

# ---- UI -----------------------------------------------------------------------------------

ui <- fluidPage(
  title = "Premier League Scorigami",
  lang = "en",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = "Every Premier League scoreline since 1993. Which have happened only once, and which have never happened at all?"),
    tags$link(rel = "icon", href = "favicon.svg", type = "image/svg+xml"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@400;600;800;900&family=IBM+Plex+Mono:wght@400;500&display=swap"),
    tags$link(rel = "stylesheet", href = "scorigami.css"),
    tags$script(src = "scorigami.js")
  ),

  tags$header(class = "hero",
    div(class = "hero__mark", `aria-hidden` = "true", HTML(paste(rep("<i></i>", 9), collapse = ""))),
    p(class = "eyebrow", sprintf("Premier League · %d seasons · %s matches", length(unique(d$season)), fmt_n(nrow(d)))),
    h1("SCORIGAMI"),
    p(class = "lede", "Every scoreline since 1993. Which have happened only once, and which have ", tags$em("never"), " happened at all?"),
    div(class = "latest",
      span(class = "latest__tag", "Latest scorigami"),
      span(class = "latest__score", sprintf("%s %d–%d %s", latest_first$home, latest_first$hg, latest_first$ag, latest_first$away)),
      span(class = "latest__meta", sprintf("%s · the %s different scoreline in Premier League history", fmt_date(latest_first$date), ordinal(latest_first$order))))
  ),

  tags$section(class = "controls", `aria-label` = "Filters",
    selectInput("season", "Season", choices = season_choices, selected = "all"),
    selectizeInput("team", "Team view", choices = team_choices, selected = "",
                   options = list(placeholder = "Every match (home vs away)"))
  ),

  uiOutput("stats"),

  div(class = "main",
    tags$section(class = "card card--grid", `aria-label` = "Scoreline grid",
      div(class = "card__head", h2(textOutput("grid_title", inline = TRUE)), p(class = "hint", "Hover a square for the story, click it for every match.")),
      uiOutput("grid")),
    tags$aside(class = "card card--detail", `aria-live` = "polite", uiOutput("detail"))
  ),

  tags$section(class = "card card--timeline", `aria-label` = "Timeline",
    div(class = "card__head", h2("When each scoreline first happened"),
        p(class = "hint", sprintf("New scorelines per season. %d in the first season alone; just %d since 2015.",
                                  sum(format(firsts$date, "%Y-%m") < "1994-08"), sum(firsts$date >= as.Date("2015-08-01"))))),
    timeline_html()),

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
    tile <- function(lbl, val, sub = NULL) div(class = "stat", span(class = "stat__lbl", lbl), span(class = "stat__val", val), if (!is.null(sub)) span(class = "stat__sub", sub))
    div(class = "stats",
      tile("Matches", fmt_n(n)),
      tile("Goals per game", if (n) sprintf("%.2f", mean(v$r + v$c)) else "–"),
      tile("Different scorelines", length(k)),
      tile("Happened once", sum(k == 1), "scorigamis in this view"))
  })

  output$grid <- renderUI({
    v <- view()
    if (!nrow(v)) return(p(class = "empty", "No matches for this team in this season."))
    grid_html(v, input$team %||% "")
  })

  output$detail <- renderUI({
    v <- view(); team <- input$team %||% ""
    parts <- as.integer(strsplit(score(), "-")[[1]])
    m <- v[v$r == parts[1] & v$c == parts[2], ]
    m <- m[order(m$date, decreasing = TRUE), ]
    heading <- div(class = "detail__head",
      span(class = "detail__score", sprintf("%d–%d", parts[1], parts[2])),
      span(class = "detail__count", if (!nrow(m)) "Never happened" else if (nrow(m) == 1) "Happened once" else paste(fmt_n(nrow(m)), "matches")))
    if (!nrow(m)) {
      return(tagList(heading, p(class = "detail__never",
        if (nzchar(team)) sprintf("No %s match in this view has ended %d–%d.", team, parts[1], parts[2])
        else sprintf("No Premier League match has ever ended %d–%d%s. Yet.", parts[1], parts[2], if ((input$season %||% "all") == "all") "" else " this season"))))
    }
    first <- m[nrow(m), ]
    shown <- head(m, 40)
    rows <- vapply(seq_len(nrow(shown)), function(i) {
      x <- shown[i, ]
      sprintf('<li><span class="mt__date">%s<small>%s</small></span><span class="mt__teams">%s <b>%d–%d</b> %s</span><a class="mt__yt" href="%s" target="_blank" rel="noopener" aria-label="Search highlights for %s">▶</a></li>',
              fmt_date(x$date), season_label(x$season), team_html(x$home, team), x$hg, x$ag, team_html(x$away, team), yt(x), esc(score_line(x)))
    }, "")
    tagList(heading,
      if (nrow(m) > 1) p(class = "detail__first", HTML(sprintf("First: %s, %s %d–%d %s", fmt_date(first$date),
                                                      team_html(first$home, team), first$hg, first$ag, team_html(first$away, team)))),
      HTML(sprintf('<ol class="mt">%s</ol>', paste(rows, collapse = ""))),
      if (nrow(m) > 40) p(class = "detail__more", sprintf("Showing the latest 40 of %s.", fmt_n(nrow(m)))))
  })
}

shinyApp(ui, server)
