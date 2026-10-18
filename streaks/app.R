# Star Streaks — which Hollywood actors went N lead roles in a row with every film rated 7+ on IMDb, and when.
# Hollywood films (US productions) since 1960; prep.R builds the data.
# Data: data/streaks.rds, built by prep.R from IMDb's non-commercial datasets (ratings and credits),
# Wikipedia film infoboxes (billing) and Wikidata, Wikipedia, Freebase and TMDB (release dates).

library(shiny)
suppressPackageStartupMessages(library(data.table))

`%||%` <- function(a, b) if (is.null(a) || !length(a) || identical(a, "")) b else a

# ---- data -----------------------------------------------------------------------------------------
d <- readRDS("data/streaks.rds")
people <- d$people
L <- d$films[d$leads, on = "fid"]                 # one row per lead role
setorder(L, pid, date, fid)
L[, i := .I]
L[, last_i := max(i), by = pid]                    # each actor's latest lead role
THIS_YEAR <- as.integer(format(Sys.Date(), "%Y"))
if (!"bill_src" %in% names(L)) L[, bill_src := "imdb"]
dup <- people[, .N, by = name][N > 1, name]        # namesakes get their birth year
people[, label := fifelse(name %in% dup, sprintf("%s (%s)", name, fifelse(is.na(born), "?", as.character(born))), name)]
setkey(people, pid)
BUILT <- d$built
YEARS <- seq(1960L, max(L$year))

K_CHOICES <- 3:10
T_CHOICES <- c("6.0+" = "6.0", "6.5+" = "6.5", "7.0+" = "7.0", "7.5+" = "7.5", "8.0+" = "8.0", "8.5+" = "8.5")
V_CHOICES <- c("1,000+" = 1000, "5,000+" = 5000, "10,000+" = 10000, "25,000+" = 25000, "100,000+" = 100000)

# ---- helpers ----------------------------------------------------------------------------------------
esc <- function(x) htmltools::htmlEscape(x)
fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_votes <- function(v) ifelse(v >= 1e6, sprintf("%.1fM", v / 1e6), ifelse(v >= 1e4, sprintf("%.0fk", v / 1e3), fmt_n(v)))
yy <- function(a, b) if (a == b) as.character(a) else sprintf("%d–%d", a, b)

# Every run of consecutive lead roles that clear the bar, k or longer
find_runs <- function(k, t, v) {
  hit <- L$rating >= t & L$votes >= v
  x <- data.table(rid = rleid(L$pid, hit), pid = L$pid, i = L$i, rating = L$rating)[hit]
  R <- x[, .(pid = pid[1], s = i[1], e = i[.N], len = .N, avg = mean(rating)), by = rid][len >= k]
  R[, `:=`(from = L$year[s], to = L$year[e], d_from = L$date[s])]
  setorder(R, -len, -avg, d_from)
  R
}

# One sidewalk tile: a star for a film that clears the bar, a bare square for one that does not
tile <- function(r, t, v, cls = "") {
  hit <- r$rating >= t & r$votes >= v
  why <- if (hit) "" else if (r$rating >= t) sprintf(" · below the %s-vote floor", fmt_n(v)) else ""
  tip <- sprintf("%s|%s · rated %.1f from %s votes%s|%s billed #%d%s", r$title, if (r$dated) format(r$date, "%e %b %Y") else paste(r$year, "(month unknown)"),
                 r$rating, fmt_n(r$votes), why, switch(r$bill_src, wikipedia = "Poster billing:", `wikipedia-alpha` = "Poster billing (alphabetical ensemble):", "IMDb billing:"), r$bill,
                 "")
  sprintf('<span class="t %s %s" role="img" aria-label="%s, %d: rated %.1f" data-tip="%s"><b>%.1f</b></span>',
          if (hit) "t--hit" else "t--miss", cls, esc(r$title), r$year, r$rating, esc(trimws(tip)), r$rating)
}

edge_note <- function(r, side, t, v, p) {
  if (is.null(r)) {
    if (side == "before") return(if (p$first_year <= 1962) "from 1960, where the record starts" else "from their first lead role")
    alive <- is.na(p$died)
    return(if (alive && (as.integer(format(Sys.Date(), "%Y")) - p$last_year) <= 3) "still going" else if (!alive) "to the end of their career" else "their last lead role so far")
  }
  why <- if (r$rating >= t && r$votes < v) sprintf("%.1f, under %s votes", r$rating, fmt_n(v)) else sprintf("%.1f", r$rating)
  sprintf("%s %s (%s)", if (side == "before") "after" else "until", esc(r$title), why)
}

plaque <- function(R, j, t, v) {
  r <- R[j]; films <- L[r$s:r$e]
  prev <- if (r$s > 1 && L$pid[r$s - 1] == r$pid) L[r$s - 1] else NULL
  nxt <- if (r$e < nrow(L) && L$pid[r$e + 1] == r$pid) L[r$e + 1] else NULL
  p <- people[.(r$pid)]; yrs <- L[pid == r$pid]$year; p[, `:=`(first_year = min(yrs), last_year = max(yrs))]
  tiles <- paste0(if (!is.null(prev)) tile(prev, t, v, "t--edge") else "",
                  paste(vapply(seq_len(nrow(films)), function(i) tile(films[i], t, v, "t--run"), ""), collapse = ""),
                  if (!is.null(nxt)) tile(nxt, t, v, "t--edge") else "")
  sprintf('<article class="pq" data-pid="%d">
    <button class="pq__head" type="button" data-pid="%d" aria-label="Spotlight %s: %d in a row, %s">
      <span class="pq__rank">%d</span><span class="pq__name">%s</span>
      <span class="pq__len"><b>%d</b> in a row</span><span class="pq__years">%s</span>
    </button>
    <div class="walk">%s</div>
    <p class="pq__note">%s · average %.1f · %s</p></article>',
    r$pid, r$pid, esc(p$name), r$len, yy(r$from, r$to), j, esc(p$name), r$len, yy(r$from, r$to), tiles,
    paste(c(edge_note(prev, "before", t, v, p), edge_note(nxt, "after", t, v, p))[1:2], collapse = ", "), r$avg,
    if (r$len >= 2) sprintf("%s → %s", esc(films$title[1]), esc(films$title[nrow(films)])) else esc(films$title[1]))
}

# ---- UI ---------------------------------------------------------------------------------------------
sel <- function(id, choices, selected, label) {
  opts <- paste(sprintf('<option value="%s"%s>%s</option>', choices, ifelse(choices == selected, " selected", ""), names(choices) %||% choices), collapse = "")
  HTML(sprintf('<span class="ask__pick"><select id="%s" aria-label="%s">%s</select></span>', id, label, opts))
}
named <- function(x) setNames(x, x)

ui <- fluidPage(
  title = "Star Streaks — who went four films in a row rated 7+?",
  lang = "en",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = "Which Hollywood actors went four lead roles in a row with every film rated 7+ on IMDb, and when? Every streak since 1960, from Al Pacino to Zendaya."),
    tags$link(rel = "icon", href = "favicon.svg", type = "image/svg+xml"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Limelight&family=Josefin+Sans:wght@500;600;700&family=Figtree:wght@400;500;600;700&display=swap"),
    tags$link(rel = "stylesheet", href = "streaks.css"),
    tags$script(src = "streaks.js")
  ),

  tags$header(class = "top",
    a(class = "top__mark", href = "./", span(class = "top__star", `aria-hidden` = "true"), "Star Streaks"),
    a(class = "top__link", href = "https://jafranceschini.quarto.pub/juan-franceschini-lab/apps.html", target = "_blank", rel = "noopener", "a White Belt project ↗")),

  tags$section(class = "hero",
    div(class = "beams", `aria-hidden` = "true", tags$i(), tags$i()),
    h1("Star Streaks"),
    p(class = "ask",
      "Who went ", sel("k", named(K_CHOICES), 4, "How many in a row"), " Hollywood films in a row as a lead, every one rated ",
      sel("t", T_CHOICES, "7.0", "Minimum IMDb rating"), " on IMDb by ",
      sel("v", V_CHOICES, 1000, "Minimum IMDb votes"), " voters, since 1960?"),
    uiOutput("answer", container = tags$p, class = "answer")),

  tags$section(class = "years", `aria-label` = "When the streaks happened",
    div(class = "sec__head", h2("When the streaks happened"), uiOutput("year_note", inline = TRUE)),
    uiOutput("years")),

  div(class = "main",
    tags$section(class = "walk-of-fame", `aria-label` = "The streaks",
      div(class = "sec__head", h2(textOutput("walk_title", inline = TRUE)),
          p(class = "hint", span(class = "hint--pointer", "Hover a star for the film. Click a name for the whole career."),
            span(class = "hint--touch", "Tap a name for the whole career."))),
      uiOutput("walk"),
      uiOutput("more")),
    tags$aside(class = "spotlight", id = "spotlight", `aria-live` = "polite",
      div(class = "spot__search", selectizeInput("who", NULL, choices = NULL, width = "100%", options = list(placeholder = "Look up any actor…", maxOptions = 30))),
      uiOutput("spot"),
      tags$button(class = "spot__back", type = "button", "Back to the list"))),

  div(id = "tip", class = "tip", role = "tooltip", hidden = NA),

  tags$footer(class = "foot",
    tags$details(class = "how", tags$summary("How it works"),
      p(tags$b("A lead role"), " is one of the first three names billed: the film's Wikipedia “Starring” list, which follows the poster, or IMDb's credits order where a film has no article."),
      p(tags$b("Films"), " are Hollywood feature films released since 1960: US productions (the United States among the film's countries of origin on Wikidata), 40 minutes or longer, with an IMDb rating. Films from other countries, documentaries, animation (voice roles), TV films, shorts and videos neither count toward a streak nor break one. Every other lead role counts, so a film below the bar, or with fewer votes than the floor, ends the run."),
      p(tags$b("Order"), " is by release date: the US release where known (Wikidata, Wikipedia, Freebase and TMDB), otherwise the first release. A few films are known only by year; they sort mid-year and say so when you hover them."),
      p(tags$b("Ratings"), sprintf(" are IMDb's, as of %s. They move a little over time, so a streak on the edge of 7.0 can come and go.", format(BUILT, "%e %B %Y")))),
    p("Built by ", a(href = "https://jafranceschini.quarto.pub/juan-franceschini-lab/", target = "_blank", rel = "noopener", "Juan Franceschini"), " · a White Belt project"),
    p(class = "foot__small", "Ratings and credits: IMDb non-commercial datasets (information courtesy of ",
      a(href = "https://www.imdb.com", target = "_blank", rel = "noopener", "IMDb", .noWS = "after"),
      "; used with permission). Billing: Wikipedia film infoboxes. Release dates: Wikidata, Wikipedia, Freebase (CMU Movie Summary Corpus) and TMDB. Not affiliated with IMDb or the Hollywood Walk of Fame."))
)

# ---- server -------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  chosen <- reactiveVal(NULL)       # pid in the spotlight; NULL follows the top streak
  limit <- reactiveVal(25)
  on_year <- reactiveVal(NULL)

  updateSelectizeInput(session, "who", choices = setNames(people$pid, people$label), selected = character(), server = TRUE)

  observeEvent(TRUE, once = TRUE, {
    q <- parseQueryString(isolate(session$clientData$url_search))
    if (!is.null(q$k) && q$k %in% K_CHOICES) updateSelectInput(session, "k", selected = q$k)
    if (!is.null(q$t) && q$t %in% T_CHOICES) updateSelectInput(session, "t", selected = q$t)
    if (!is.null(q$v) && q$v %in% V_CHOICES) updateSelectInput(session, "v", selected = q$v)
    if (!is.null(q$actor)) { p <- people[nconst == q$actor]; if (nrow(p)) chosen(p$pid) }
    if (!is.null(q$year) && grepl("^[0-9]{4}$", q$year)) on_year(as.integer(q$year))
  })

  prm <- reactive(list(k = as.integer(input$k %||% 4), t = as.numeric(input$t %||% 7), v = as.numeric(input$v %||% 1000)))
  runs <- reactive({ p <- prm(); find_runs(p$k, p$t, p$v) })
  shown <- reactive({ R <- runs(); y <- on_year(); if (is.null(y)) R else R[from <= y & to >= y] })

  observeEvent(input$pick, chosen(as.integer(input$pick)))
  observeEvent(input$who, if (nzchar(input$who %||% "")) chosen(as.integer(input$who)))
  observeEvent(input$more, limit(limit() + 25))
  observeEvent(input$year_pick, { y <- as.integer(input$year_pick); on_year(if (identical(on_year(), y) || is.na(y)) NULL else y); limit(25) })
  observeEvent(prm(), limit(25), ignoreInit = TRUE)

  spot_pid <- reactive(chosen() %||% { R <- shown(); if (nrow(R)) R$pid[1] else runs()$pid[1] %||% NA_integer_ })

  observe({
    p <- prm(); a <- chosen(); y <- on_year()
    updateQueryString(paste0("?k=", p$k, "&t=", sprintf("%.1f", p$t), "&v=", p$v,
                             if (!is.null(a)) paste0("&actor=", people[.(a)]$nconst) else "", if (!is.null(y)) paste0("&year=", y) else ""), mode = "replace")
  })

  output$answer <- renderUI({
    R <- runs(); p <- prm()
    if (!nrow(R)) return(HTML("Nobody did. Try fewer films in a row, a lower rating or a lower vote floor."))
    top <- R[1]; who <- people[.(top$pid)]$name
    # still alive: the run reaches the actor's latest lead role, released in the last two years
    now <- R[e == L$last_i[e] & L$year[e] >= THIS_YEAR - 2][people[, .(pid, died)], on = "pid", nomatch = 0][is.na(died)][order(-len)]
    now_txt <- if (nrow(now)) sprintf('<span class="now"><span class="now__lbl">On a streak right now</span>%s</span>', paste(sprintf(
      '<button type="button" class="now__pick" data-pid="%d">%s <b>%d</b></button>', now$pid, vapply(now$pid, function(x) esc(people[.(x)]$name), ""), now$len)[seq_len(min(4, nrow(now)))], collapse = " · ")) else ""
    HTML(sprintf("<b>%s</b> %s%s. Longest: <b>%s</b>, %d in a row, %s.%s",
                 fmt_n(uniqueN(R$pid)), if (uniqueN(R$pid) == 1) "actor did it" else "actors did it",
                 if (nrow(R) == uniqueN(R$pid)) "" else sprintf(" (%s streaks)", fmt_n(nrow(R))),
                 esc(who), top$len, yy(top$from, top$to), now_txt))
  })

  output$years <- renderUI({
    R <- runs(); y <- on_year()
    cnt <- if (nrow(R)) R[, .(yr = from:to), by = .I][, .N, by = yr] else data.table(yr = integer(), N = integer())
    n <- setNames(integer(length(YEARS)), YEARS); n[as.character(cnt$yr)] <- cnt$N
    mx <- max(1, n)
    stop_at <- if (!is.null(y) && y %in% YEARS) y else YEARS[which.max(n)]     # the one tab stop; arrows move from it
    bars <- vapply(seq_along(YEARS), function(j) {
      yr <- YEARS[j]; k <- n[j]
      sprintf('<button type="button" class="yr%s%s" data-year="%d" tabindex="%d" aria-pressed="%s" aria-label="%d: %d %s on a streak" data-tip="%d|%d %s on a streak"><i style="--h:%.1f%%"></i>%s</button>',
              if (!is.null(y) && y == yr) " is-on" else "", if (k == 0) " yr--zero" else "", yr, if (yr == stop_at) 0L else -1L,
              tolower(!is.null(y) && y == yr), yr, k, if (k == 1) "actor" else "actors",
              yr, k, if (k == 1) "actor" else "actors", 100 * k / mx, if (yr %% 10 == 0) sprintf("<b>%d</b>", yr) else "")
    }, "")
    HTML(sprintf('<div class="yrs-scroll"><div class="yrs" role="group" aria-label="Actors on a streak, by year. Arrow keys move between years.">%s</div></div>', paste(bars, collapse = "")))
  })

  output$year_note <- renderUI({
    y <- on_year()
    if (is.null(y)) span(class = "hint", "How many actors were mid-streak each year. Click a year to see who.")
    else span(class = "hint", sprintf("Showing streaks that include %d. ", y), tags$button(class = "linkish", type = "button", `data-year` = y, "Show every year"))
  })

  output$walk_title <- renderText({
    p <- prm(); y <- on_year()
    sprintf("%d+ in a row at %.1f+%s", p$k, p$t, if (!is.null(y)) sprintf(", during %d", y) else "")
  })

  output$walk <- renderUI({
    R <- shown(); p <- prm()
    if (!nrow(R)) return(p(class = "empty", "No streaks here. Lower the bar above, or pick another year."))
    n <- min(nrow(R), limit())
    HTML(paste(vapply(seq_len(n), function(j) plaque(R, j, p$t, p$v), ""), collapse = ""))
  })

  output$more <- renderUI({
    R <- shown(); n <- limit()
    if (nrow(R) > n) tags$button(class = "more", type = "button", sprintf("Show %d more of %s", min(25, nrow(R) - n), fmt_n(nrow(R) - n)))
  })

  output$spot <- renderUI({
    pid0 <- spot_pid(); req(!is.na(pid0)); p <- prm(); pp <- people[.(pid0)]
    x <- copy(L[pid == pid0])
    x[, hit := rating >= p$t & votes >= p$v]
    r <- rle(x$hit); x[, runlen := rep(r$lengths, r$lengths)][, star := hit & runlen >= p$k]
    best <- if (any(x$hit)) max(r$lengths[r$values]) else 0
    bi <- if (best) { e <- cumsum(r$lengths)[which(r$values & r$lengths == best)[1]]; c(e - best + 1, e) } else NULL
    life <- if (is.na(pp$born)) "" else if (is.na(pp$died)) sprintf("born %d", pp$born) else sprintf("%d–%d", pp$born, pp$died)
    x[, dec := (year %/% 10) * 10]
    rows <- vapply(split(seq_len(nrow(x)), x$dec), function(ix) {
      sprintf('<div class="dec"><span class="dec__lbl">%ds</span><div class="dec__walk">%s</div></div>', x$dec[ix[1]],
              paste(vapply(ix, function(i) tile(x[i], p$t, p$v, if (x$star[i]) "t--run" else ""), ""), collapse = ""))
    }, "")
    runs_here <- {
      rr <- which(r$values & r$lengths >= p$k); ends <- cumsum(r$lengths)
      vapply(rr, function(q) { e <- ends[q]; s <- e - r$lengths[q] + 1
        sprintf("<li><b>%d in a row</b>, %s: %s</li>", r$lengths[q], yy(x$year[s], x$year[e]), esc(paste(x$title[s:e], collapse = " · "))) }, "")
    }
    tagList(
      div(class = "spot__plaque",
        div(class = "spot__name", pp$name),
        div(class = "spot__life", life, a(href = sprintf("https://www.imdb.com/name/%s/", pp$nconst), target = "_blank", rel = "noopener", "IMDb ↗"))),
      HTML(sprintf('<p class="spot__stats"><b>%d</b> Hollywood lead roles since 1960 · <b>%d</b> rated %.1f+ · longest run <b>%d</b>%s</p>',
                   nrow(x), sum(x$hit), p$t, best, if (!is.null(bi)) sprintf(" (%s)", yy(x$year[bi[1]], x$year[bi[2]])) else "")),
      HTML(sprintf('<div class="career">%s</div>', paste(rows, collapse = ""))),
      HTML(sprintf('<div class="legend"><span><i class="t t--hit t--run"></i>part of a %d+ streak</span><span><i class="t t--hit"></i>rated %.1f+</span><span><i class="t t--miss"></i>below the bar</span></div>', p$k, p$t)),
      if (length(runs_here)) HTML(sprintf('<ol class="spot__runs">%s</ol>', paste(runs_here, collapse = ""))) else p(class = "hint", sprintf("No run of %d+ at these settings.", p$k)))
  })
}

shinyApp(ui, server)
