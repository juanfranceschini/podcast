library(shiny)
library(tidyverse)
library(lubridate)
library(showtext)
library(bslib)
library(reactable)
library(htmltools)

# --- 1. SETUP & CONSTANTS ----------------------------------------------------

# Load Fonts
font_add_google("Outfit", "outfit")
showtext_auto()

# --- DESIGN SYSTEM ---
pl_purple    <- "#37003c"
pl_magenta   <- "#EA00D9"
pl_neon      <- "#00FF87"
pl_white     <- "#FFFFFF"
pl_text_mute <- "rgba(255, 255, 255, 0.6)"
pl_card_bg   <- "rgba(40, 25, 60, 0.7)"

max_g <- 10

# --- DATA HELPERS ---

# 1. Load History (Cached)
if (file.exists("pl_history.rds")) {
  pl_history <- readRDS("pl_history.rds") |>
    mutate(
      Date = as.Date(Date_Parsed),
      season_year = ifelse(month(Date) >= 8, year(Date), year(Date) - 1),
      Season = paste0(season_year, "/", season_year + 1)
    )
} else {
  pl_history <- tibble(Date = as.Date(character()), Season = character())
}

# 2. Season Choices
current_year <- 2025
years_range <- current_year:1993
seasons_choices <- c(
  "ALL-TIME (1993-Present)",
  map_chr(years_range, function(y) paste0(y, "/", y + 1))
)

# Note: Current season data is included in pl_history.rds
# and updated weekly via GitHub Actions (no live fetching needed)

# --- 2. UI -------------------------------------------------------------------

theme_pl <- bs_theme(
  bg = pl_purple,
  fg = pl_white,
  primary = pl_magenta,
  base_font = font_google("Outfit"),
  heading_font = font_google("Outfit")
)

ui <- page_fluid(
  theme = theme_pl,
  title = "Premier League Scorigami",
  
  tags$head(
    tags$style(HTML(paste0("
      body {
        background: radial-gradient(circle at top right, #5c0064 0%, ", pl_purple, " 40%, #0f0a1e 100%);
        min-height: 100vh;
      }
      .pl-card {
        background: ", pl_card_bg, ";
        backdrop-filter: blur(12px);
        -webkit-backdrop-filter: blur(12px);
        border: 1px solid rgba(255, 255, 255, 0.1);
        border-radius: 16px;
        box-shadow: 0 4px 30px rgba(0, 0, 0, 0.3);
        transition: border-color 0.3s ease;
      }
      .pl-card:hover { border-color: ", pl_magenta, "; }
      
      /* Input Styles */
      .selectize-input {
        background: rgba(0, 0, 0, 0.3) !important;
        border: 1px solid ", pl_magenta, " !important;
        color: ", pl_white, " !important;
        font-weight: bold;
        border-radius: 8px;
      }
      .selectize-dropdown {
        background: ", pl_purple, ";
        border: 1px solid ", pl_magenta, ";
        color: ", pl_white, ";
      }
      
      /* Typography */
      .stat-val { font-size: 3.5rem; font-weight: 800; line-height: 1; margin: 10px 0; }
      .stat-lbl { font-size: 0.8rem; letter-spacing: 2px; text-transform: uppercase; color: ", pl_text_mute, "; }

      /* Table Search Input */
      .rt-search { background: rgba(0,0,0,0.3) !important; border: 1px solid rgba(255,255,255,0.2) !important; color: white !important; border-radius: 6px; }

      /* Plot Interactivity */
      #scorigamiPlot { cursor: pointer; user-select: none; -webkit-user-select: none; }
    ")))
  ),
  
  # --- HEADER ---
  div(
    style = "text-align: center; padding: 40px 0 20px 0;",
    img(src = "https://upload.wikimedia.org/wikipedia/en/f/f2/Premier_League_Logo.svg", height = "80px", style = "margin-bottom: 20px; filter: brightness(0) invert(1);"),
    h1("SCORIGAMI", style = "font-weight: 900; letter-spacing: -2px; font-size: 4rem; margin: 0; line-height: 0.9;"),
    p("THE OFFICIAL ARCHIVE", style = paste0("color: ", pl_neon, "; letter-spacing: 6px; font-weight: 700; margin-top: 10px; font-size: 1rem;")),
    
    div(
      style = "max-width: 320px; margin: 30px auto;",
      selectInput("season", NULL, choices = seasons_choices, selected = "ALL-TIME (1993-Present)")
    )
  ),
  
  # --- STATS ROW ---
  fluidRow(
    style = "margin-bottom: 30px;",
    column(4, 
      div(class = "pl-card p-4 text-center",
        div(class = "stat-lbl", "Matches Played"),
        div(class = "stat-val", style = paste0("color:", pl_white), textOutput("stat_matches"))
      )
    ),
    column(4, 
      div(class = "pl-card p-4 text-center",
        div(class = "stat-lbl", "Total Goals"),
        div(class = "stat-val", style = paste0("color:", pl_neon), textOutput("stat_goals"))
      )
    ),
    column(4, 
      div(class = "pl-card p-4 text-center",
        div(class = "stat-lbl", "Unique Scores"),
        div(class = "stat-val", style = paste0("color:", pl_magenta), textOutput("stat_unique"))
      )
    )
  ),
  
  # --- CHART ---
  div(class = "pl-card", style = "padding: 20px; margin-bottom: 40px;",
    plotOutput("scorigamiPlot", height = "850px", click = "plot_click"),
    div(
      style = paste0("text-align: center; margin-top: 15px; color: ", pl_text_mute, "; font-size: 0.85rem;"),
      span(style = paste0("color: ", pl_magenta, "; font-weight: 700;"), "💡 TIP: "),
      "Click any ",
      span(style = paste0("color: ", pl_magenta, "; font-weight: 700;"), "magenta tile"),
      " to view match details"
    )
  ),
  
  # --- TABLE ---
  div(class = "pl-card", style = "padding: 30px; margin-bottom: 50px;",
    div(
      style = "border-bottom: 2px solid rgba(255,255,255,0.1); padding-bottom: 15px; margin-bottom: 20px;",
      h3("MATCH REGISTRY", style = "font-weight: 800; margin: 0; letter-spacing: 1px;"),
      span("Singular occurrences in selected history", style = paste0("color:", pl_text_mute, "; font-size: 0.9rem;"))
    ),
    reactableOutput("scorigamiTable")
  )
)

# --- 3. SERVER ---------------------------------------------------------------

server <- function(input, output, session) {
  
  # --- Data Logic ---
  data_r <- reactive({
    req(input$season)

    # Use cached data only (updated weekly via GitHub Actions)
    # No live fetching to avoid double-counting
    if (input$season == "ALL-TIME (1993-Present)") return(pl_history)
    return(pl_history |> filter(Season == input$season))
  })

  # --- Grid Data (for plot and click handler) ---
  grid_r <- reactive({
    df <- data_r()
    req(nrow(df) > 0)

    counts <- df |> count(HG, AG, name = "n")
    grid   <- expand.grid(HG = 0:max_g, AG = 0:max_g) |>
      as_tibble() |>
      left_join(counts, by = c("HG", "AG")) |>
      mutate(
        n = replace_na(n, 0),
        type = case_when(
          n == 0 ~ "empty",
          n == 1 ~ "scorigami",
          n <= 6 ~ "rare",
          TRUE   ~ "common"
        )
      )
    grid
  })

  # --- Stats ---
  output$stat_matches <- renderText({ format(nrow(data_r()), big.mark = ",") })
  output$stat_goals   <- renderText({ format(sum(data_r()$HG + data_r()$AG), big.mark = ",") })
  output$stat_unique  <- renderText({ n_distinct(data_r()$Score) })
  
  # --- Plot (FIXED) ---
  output$scorigamiPlot <- renderPlot({
    grid <- grid_r()
    req(nrow(grid) > 0)

    grid <- grid |> mutate(label = ifelse(n == 0, "", as.character(n)))

    ggplot(grid, aes(x = AG, y = HG)) +
      geom_tile(aes(fill = type), color = pl_purple, linewidth = 3) +

      scale_fill_manual(values = c(
        "empty"     = "transparent",
        "scorigami" = pl_magenta,
        "rare"      = pl_neon,
        "common"    = pl_white
      ), guide = "none") +

      # Text
      geom_text(aes(label = label, color = type), family = "outfit", fontface = "bold", size = 10) +
      scale_color_manual(values = c(
        "empty"     = "transparent",
        "scorigami" = pl_white,
        "rare"      = pl_purple,
        "common"    = pl_purple
      ), guide = "none") +

      scale_x_continuous(breaks = 0:max_g, position = "top") +
      scale_y_reverse(breaks = 0:max_g) +
      labs(x = "AWAY GOALS", y = "HOME GOALS") +

      theme_minimal(base_family = "outfit") +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        axis.text = element_text(color = pl_white, size = 20, face = "bold"),
        axis.title = element_text(color = pl_neon, size = 30, face = "bold", margin = margin(15,15,15,15)),
        axis.ticks = element_blank()
      ) +
      coord_fixed(expand = FALSE)
  }, bg = "transparent")

  # --- Click Handler for Scorigami Tiles ---
  observeEvent(input$plot_click, {
    # Identify clicked tile
    clicked <- nearPoints(grid_r(), input$plot_click,
                         xvar = "AG", yvar = "HG",
                         threshold = 10, maxpoints = 1)
    req(nrow(clicked) == 1)

    # Extract tile info
    home_goals <- clicked$HG
    away_goals <- clicked$AG
    tile_type  <- clicked$type

    # Only show modal for scorigami tiles
    if (tile_type != "scorigami") return()

    # Find match data
    match_data <- data_r() |>
      filter(HG == home_goals, AG == away_goals)
    req(nrow(match_data) == 1)

    # Build YouTube URL
    yt_url <- paste0(
      "https://www.youtube.com/results?search_query=",
      URLencode(paste(match_data$Home, "vs", match_data$Away,
                     format(match_data$Date, "%Y"), "highlights"))
    )

    # Show modal with match details
    showModal(modalDialog(
      title = div(
        style = paste0("color:", pl_magenta, "; font-weight:800; font-size:1.5rem; letter-spacing:1px;"),
        "⚡ SCORIGAMI MATCH"
      ),

      div(
        style = "padding:10px;",

        # Date badge
        div(
          style = paste0(
            "display:inline-block; background:", pl_neon, "; color:", pl_purple,
            "; padding:4px 12px; border-radius:6px; font-size:0.85rem; ",
            "font-weight:700; margin-bottom:20px;"
          ),
          format(match_data$Date, "%B %d, %Y")
        ),

        # Match display (Home vs Away)
        div(
          style = "display:flex; align-items:center; justify-content:center; margin-bottom:20px; gap:15px;",

          # Home Team
          div(
            style = "font-size:1.3rem; font-weight:700;",
            match_data$Home
          ),

          # VS Separator
          div(style = paste0("color:", pl_text_mute, "; font-size:1rem; font-weight:700;"), "VS"),

          # Away Team
          div(
            style = "font-size:1.3rem; font-weight:700;",
            match_data$Away
          )
        ),

        # Final score badge (single score display)
        div(
          style = paste0(
            "text-align:center; margin-bottom:20px; padding:15px; ",
            "background:", pl_card_bg, "; border-radius:8px; ",
            "border:2px solid ", pl_magenta
          ),
          div(style = paste0("color:", pl_text_mute, "; font-size:0.75rem; letter-spacing:2px; margin-bottom:8px;"), "FINAL SCORE"),
          div(style = paste0("color:", pl_magenta, "; font-size:2.5rem; font-weight:900;"), match_data$Score)
        ),

        # YouTube button
        div(
          style = "text-align:center;",
          tags$a(
            href = yt_url,
            target = "_blank",
            style = paste0(
              "display:inline-block; background:", pl_magenta,
              "; color:", pl_white,
              "; padding:14px 28px; border-radius:8px; ",
              "text-decoration:none; font-weight:700; font-size:1rem; ",
              "transition: opacity 0.2s ease; letter-spacing:1px;"
            ),
            onmouseover = "this.style.opacity='0.8'",
            onmouseout = "this.style.opacity='1'",
            "▶ WATCH HIGHLIGHTS"
          )
        )
      ),

      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # --- Auto-close modal on season change ---
  observeEvent(input$season, {
    removeModal()
  })

  # --- Table ---
  output$scorigamiTable <- renderReactable({
    df <- data_r()
    req(nrow(df) > 0)
    
    tbl_data <- df |>
      group_by(Score) |>
      mutate(Count = n()) |>
      ungroup() |>
      filter(Count == 1) |>
      select(Date, Home, Away, Score) |>
      arrange(desc(Date)) |>
      mutate(
        Year = year(Date),
        SeasonStr = ifelse(month(Date) >= 8, paste0(Year, "-", substr(Year+1, 3, 4)), paste0(Year-1, "-", substr(Year, 3, 4))),
        Details = NA 
      )
    
    reactable(
      tbl_data,
      theme = reactableTheme(
        backgroundColor = "transparent",
        color = pl_white,
        borderColor = "rgba(255,255,255,0.1)",
        stripedColor = "rgba(255,255,255,0.05)",
        highlightColor = "rgba(234, 0, 217, 0.2)",
        inputStyle = list(backgroundColor = "rgba(0,0,0,0.5)", color = pl_white)
      ),
      columns = list(
        Year = colDef(show = FALSE),
        SeasonStr = colDef(show = FALSE),
        
        Date = colDef(
          name = "DATE",
          minWidth = 120,
          style = list(color = pl_text_mute),
          cell = function(value) {
            format(value, "%b-%d %Y")
          }
        ),
        Home = colDef(name = "HOME", minWidth = 150, style = list(fontWeight = "bold")),
        Away = colDef(name = "AWAY", minWidth = 150, style = list(fontWeight = "bold")),
        
        Score = colDef(
          name = "SCORE", 
          align = "center",
          width = 100,
          style = list(color = pl_magenta, fontWeight = "800", fontSize = "1.2rem")
        ),
        
        Details = colDef(
          name = "ACTIONS",
          sortable = FALSE,
          filterable = FALSE,
          width = 120,
          align = "center",
          cell = function(value, index) {
            row <- tbl_data[index, ]
            yt_u <- paste0("https://www.youtube.com/results?search_query=", URLencode(paste(row$Home, "vs", row$Away, format(row$Date, "%Y"), "highlights")))

            tags$a(href = yt_u, target = "_blank", style = "text-decoration: none;",
              div(style = paste0("background:", pl_neon, "; color:", pl_purple, "; padding:6px 14px; border-radius:6px; font-weight:700; font-size:0.8rem; display: inline-block;"), "▶ WATCH")
            )
          }
        )
      ),
      searchable = TRUE,
      pagination = FALSE,
      height = "600px",
      striped = TRUE,
      highlight = TRUE
    )
  })
}

shinyApp(ui, server)