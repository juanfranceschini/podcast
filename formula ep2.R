library(tidyverse)
library(tidytuesdayR)
library(scales)
library(lubridate)
library(beeswarm)
library(ggthemr)
library(ggfx)
library(ggrepel)
library(ggtext)
library(here)
library(glue)

library(extrafont)
font_import()
loadfonts(device = "win")

ggthemr('chalk', spacing = 1, type = 'outer', layout = "minimal")
ggthemr_reset()
library(showtext)
font_add_google("Roboto")
font_add_google("Acme")  ## name that will be used in R

font_add_google("Gochi Hand", "gochi")
font_add_google("Cabin Sketch")
showtext_opts(dpi = 160)

options("device" = "windows")

ggthemr('chalk', spacing = 0.9, type = 'outer', layout = 'clean')
ggthemr_reset()

## Automatically use showtext to render text
showtext_auto()

font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(theme_bw(base_size = 6, base_family = "Acme"))

get_png <- function(filename) {
    grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("logo.png")
t <- grid::roundrectGrob()


movies %>% as.tibble()
?boxoffice

extrafont::loadfonts(device = "win", quiet = TRUE)


# Custom theme
theme_owen <- function () { 
    theme_minimal(base_size=11, base_family="Consolas") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'midnightblue', color = "midnightblue")
        )
}

# Turn off scientific notation 
options(scipen=999)

movies <- boxoffice::top_grossing(type = 'worldwide', ranks = 1:5000)

movies = movies %>% as_tibble()
movies %>% filter(str_detect(movie, 'Harry Potter')) %>% 
    filter(!str_detect(movie,'Marathon')) %>% 
    ggplot(aes(year_released, total_box_office)) +
    geom_point()+
    geom_line()


movies %>% filter(str_detect(movie, 'John Wick')) %>% 
    #filter(!str_detect(movie,'Marathon')) %>% 
    mutate(movie = str_remove(movie, ' — Parabellum')) %>% 
    ggplot(aes(year_released, total_box_office, label = movie)) +
    coord_cartesian(clip = "off") +
    with_outer_glow(geom_line(size = 2, color = 'white'), colour = 'pink', sigma = 4)+
    with_outer_glow(geom_point(size = 8, color = 'white'), colour = 'pink', sigma = 4)+
    with_outer_glow(geom_text_repel(size = 5, fontface = 'bold', color = 'white', 
                                    force = 1,
                                    nudge_y = 20000000,
                                    nudge_x = 0.75,
                                    hjust = 0,
                                    vjust = 1,
                                    min.segment.length = Inf),
                                    colour = 'pink', sigma = 8)+
    scale_y_continuous(label = unit_format(unit = "MM", scale = 1e-6))+
    expand_limits(y = 380000000, x = 2020) +
    annotation_custom(l, xmin = 2019, xmax = 2020, ymin = -50000000, ymax = -10000)+
    labs(x=NULL, y = NULL)+
    theme(#axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15, face = 'bold', family = 'Acme'),
        axis.text.y = element_text(size = 15, face = 'bold', family = 'Acme'),
        axis.title.y = element_text(size = 10, face = 'bold', family = 'Acme'),
        axis.title.x = element_text(size = 10, face = 'bold', family = 'Acme'),
        legend.position = "none",
        plot.background = element_rect(fill = "#000033"),
        panel.background = element_rect(fill = "#000033"),
        #panel.grid = element_blank(),
        plot.title = element_text(size=15, color = "#525252", family = 'Acme'),
        plot.subtitle = element_text(size=15, color = "#525252", family = 'Acme'),
        plot.caption = element_text(size=10, color = "black", family = 'Rockwell'),
        plot.margin = unit(c(1, 1, 7, 1), "lines"))

movies %>% filter(str_detect(movie, 'Fast|Furious')) %>% 
    filter(!str_detect(movie,'Faster|Times|Crayon')) %>% 
    ggplot(aes(year_released, total_box_office, label = movie)) +
    geom_point()+
    geom_line(size = 2, color = 'orange')+
    geom_text(fontface = 'bold', , vjust = 0, nudge_x = 1)+
    scale_y_continuous(label = comma)

movies %>% filter(str_detect(movie, 'Pirates of the Caribbean')) %>% 
    filter(!str_detect(movie,'Faster|Times|Crayon')) %>% 
    mutate(movie = str_remove(movie, 'Pirates of the Caribbean: ')) %>% 
    ggplot(aes(year_released, total_box_office, label = movie)) +
    geom_point()+
    geom_line(size = 2, color = 'orange')+
    geom_text(fontface = 'bold')+
    scale_y_continuous(label = comma)

movies %>% filter(str_detect(movie, 'Harry Potter')) %>% 
    filter(!str_detect(movie,'Marathon')) %>% 
    mutate(movie = str_remove(movie, 'Harry Potter and ')) %>%
    ggplot(aes(year_released, total_box_office, label = movie)) +
    geom_point()+
    geom_line(size = 2, color = 'orange')+
    geom_text_repel(fontface = 'bold', color ='white')+
    scale_y_continuous(label = comma)+
    scale_x_continuous(breaks= pretty_breaks())




patchwork::wrap_plots((m1 + m2) / (m3 + m4))


movies %>% filter(str_detect(movie, 'Romeo|Juliet'))

nolan = movies %>% filter(rank %in% c(46,114,377,1490,1588,3332)) 

movies %>% mutate(mix = international_box_office/total_box_office) %>% 
    filter(!is.na(american_box_office),
           american_box_office > 10000000,
           year_released == 1996) %>% 
    slice_max(n = 10, mix)


nolan %>% 
    ggplot(aes(year_released, total_box_office, label = movie)) +
    coord_cartesian(clip = "off") +
    with_outer_glow(geom_line(size = 2, color = 'white'), colour = '#1FFF0F', sigma = 10)+
    with_outer_glow(geom_point(size = 4, color = 'white'), colour = '#1FFF0F', sigma = 10)+
    with_outer_glow(geom_text_repel(size = 2.75, fontface = 'bold', color =  ifelse(nolan$rank == 114, '#1FFF0F', 'white'), 
                                    force = 1,
                                    nudge_y = ifelse(nolan$rank == 46, +10000000,-30000000),
                                    nudge_x = 0.3,
                                    hjust = 0,
                                    vjust = 1,
                                    min.segment.length = Inf,
                                    family = 'Calibri',
                                    ),
                    colour =  'white', sigma = 1
                    )+
    
    scale_y_continuous(label = unit_format(unit = "MM", scale = 1e-6))+
    scale_x_continuous(breaks= pretty_breaks(8))+
    labs(x=NULL, y = NULL,
         title = "Christopher Nolan planificó el <b style='color:#1FFF0F'>golpe</b>",
         subtitle = 'Inception se estrena dos años luego del éxito que obtuvo Nolan con \"The Dark Knight\", que recaudó mil millones de dólares en 2008.',
         caption = 'LA FORMULA podcast')+
    theme(#axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 12), size = 18, 
                                   face = 'bold', family = 'Acme', angle = 25, color = 'white'),
        axis.text.y = element_text(size = 18, face = 'bold', family = 'Acme', color = 'white'),
        axis.title.y = element_text(size = 10, face = 'bold', family = 'Acme', color = 'white'),
        axis.title.x = element_text(size = 10, face = 'bold', family = 'Acme', color = 'white'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#333333"),
        panel.background = element_rect(fill = "#333333"),
        plot.title = element_markdown(size=50, color = "white", family = 'Cabin Sketch', lineheight = 1.1),
        plot.subtitle = element_text(size=15, color = "white", family = 'Acme', face = 'bold'),
        plot.caption = element_text(size=15, color = "white", family = 'Cabin Sketch'),
        plot.margin = unit(c(.2, .2, .2, .2), "cm"),
        panel.border = element_blank(),
        panel.grid = element_blank())

ggsave(here("inception.png"), width = 7, height = 4, dpi = 300)

movies %>% filter(year_released == 1996) %>% 
    slice_max(n=25, total_box_office) %>% 
    mutate(movie = glue('{movie} ({round(total_box_office/1000000,0)}MM)')) %>% 
    mutate(movie = fct_reorder(movie,total_box_office)) %>% 
    ggplot(aes(x=movie, y = total_box_office)) +
    geom_col(fill = '#1FFF0F') +
    scale_y_continuous(label = unit_format(unit = "MM", scale = 1e-6))+
    coord_flip() +
    theme(legend.position = 'none') +
    gghighlight::gghighlight(rank == 1187) +
    labs(title = 'Recaudacion de las 25 peliculas mas taquilleras en 1996',
         x = NULL,
         y = NULL) +
    theme(text=element_text(size=16,  family="Acme")) 


tmdb = tmdb_5000_movies %>% select(budget, original_title, release_date, revenue, runtime, vote_average)

tmdb %>% filter(year(release_date) == 1996,
                budget > 0) %>%
    #slice_max(n=25, budget) %>% 
    filter(budget > 1000000) %>% 
    mutate(margin = (revenue/budget)) %>% 
    mutate(original_title = str_trunc(original_title, 25)) %>% 
    slice_max(n=10, margin) %>% 
    mutate(original_title = fct_reorder(original_title, margin)) %>% 
    ggplot(aes(x=original_title, y = margin, fill = original_title)) +
    geom_col(fill = '#1FFF0F') +
    scale_y_continuous(label = number, breaks = pretty_breaks()) +
    expand_limits(y = c(0,1)) +
    geom_hline(yintercept = 10, size = 2, lty=2, alpha = 0.5)+
    coord_flip() +
    theme(legend.position = 'none') +
    gghighlight::gghighlight(original_title == 'Romeo + Juliet') +
    labs(title = 'Margen de ganancia de peliculas con presupuesto mayor a $1MM en 1996',
         x = NULL,
         y = NULL) +
    theme(text=element_text(size=16,  family="Acme"))

tmdb %>% filter(year(release_date) == 1996,
                budget > 0) %>%
    slice_max(n=100, revenue) %>% 
    mutate(margin = (revenue-budget)/revenue) %>%
    mutate(original_title = str_trunc(original_title, 30)) %>% 
    mutate(name = glue('{original_title} ({runtime})')) %>% 
    slice_max(n=25, revenue) %>% 
    mutate(name = fct_reorder(name, runtime)) %>% 
    ggplot(aes(x=name, y = runtime)) +
    geom_col(fill = '#1FFF0F') +
    scale_y_continuous(label = comma) +
    coord_flip() +
    theme(legend.position = 'none') +
    gghighlight::gghighlight(original_title == 'Romeo + Juliet' ) +
    labs(title = 'Duracion de las peliculas mas taquilleras en 1996',
         x = NULL,
         y = NULL) +
    theme(text=element_text(size=16,  family="Acme"))

tmdb %>% filter(year(release_date) == 1996,
                revenue > 100000000) %>%
    #slice_max(n=100, revenue) %>% 
    mutate(margin = (revenue-budget)/revenue) %>%
    mutate(original_title = str_trunc(original_title, 30)) %>% 
    mutate(name = glue('{original_title} ({vote_average})')) %>% 
    #slice_max(n=25, vote_average) %>% 
    mutate(name = fct_reorder(name, vote_average)) %>% 
    ggplot(aes(x=name, y = vote_average)) +
    geom_col(fill = '#1FFF0F') +
    scale_y_continuous(label = comma) +
    coord_flip() +
    theme(legend.position = 'none') +
    gghighlight::gghighlight(original_title == 'Romeo + Juliet' ) +
    labs(title = 'Duracion de las peliculas mas taquilleras en 1996',
         x = NULL,
         y = NULL) +
    theme(text=element_text(size=16,  family="Acme"))

tmdb %>% filter(year(release_date) == 1996,
                budget > 0) %>%
    slice_max(n=100, revenue) %>% 
    mutate(margin = (revenue-budget)/revenue) %>%
    mutate(original_title = str_trunc(original_title, 30)) %>% 
    mutate(name = glue('{original_title} ({vote_average})')) %>% 
    #slice_max(n=25, revenue) %>% 
    ggplot(aes(x=name, y = vote_average, size = revenue, label = original_title)) +
    geom_point() +
    scale_y_continuous(label = comma, breaks = pretty_breaks()) +
    labs(title = 'Calificacion de las peliculas mas taquilleras en 1996',
         x = NULL,
         y = NULL) +
    theme(text=element_text(size=16,  family="Acme"),
          axis.text.x = element_blank())+
    geom_label_repel()
    #gghighlight::gghighlight(original_title == 'Romeo + Juliet' )


tmdb %>% filter(year(release_date) == 2010,
                budget > 0) %>%
    slice_max(n=30, revenue) %>% 
    mutate(margin = (revenue-budget)/revenue) %>% 
    slice_max(n=15, margin) %>% 
    mutate(original_title = fct_reorder(original_title, margin)) %>% 
    ggplot() +
    geom_link(aes(x = original_title, y = 0, 
                  xend = original_title, yend = margin, 
                  color = original_title, size = stat(index))) +
    geom_point(aes(x = original_title, y = margin - 0.0075, color = original_title), 
               shape = 21,
               fill = "white",
               size = 6.5, 
               stroke = 1.5) +
    scale_y_continuous(label = percent) +
    coord_flip() +
    theme(legend.position = 'none') +
    #gghighlight::gghighlight(original_title == 'Inception') +
    labs(title = 'Margen de ganancia de las peliculas mas taquilleras en 2010')

tmdb %>% 
    filter(revenue > 10000000,
           year(release_date) > 1993,
           year(release_date) < 2015) %>% 
    group_by(year = year(release_date)) %>%
    summarise(length = mean(runtime)) %>%
    ungroup() %>% 
    ggplot(aes(x=year, y = length, label = round(length,1)))+
    geom_point(size = 3, alpha = 0.3)+
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 7),color = '#1FFF0F', size = 2.5, se = FALSE)+
    #geom_line(color = '#1FFF0F', size = 2.5) +
    geom_text_repel(color = 'white', fontface = "bold", size = 3)+
    scale_y_continuous(label = number_format(accuracy = NULL), breaks = pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    theme(legend.position = 'none') +
    labs(title = 'Duracion promedio de peliculas desde 1994 hasta 2014',
         subtitle = 'Solo incluye peliculas con al menos $10MM de recaudacion mundial',
         x = NULL,
         y = NULL) +
    theme(text=element_text(size=16,  family="Acme"),
          axis.text.y = element_blank())
