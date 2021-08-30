
# Load libraries ----------------------------------------------------------


library(tidyverse)
library(rvest)
library(mlbstatsR)
library(ggtext)
library(here)
library(gt)

# Data Wrangler -----------------------------------------------------------

teams <- get_reference_team_mlb()

teams_standings <- get_reference_team_mlb(stats = "pitching") %>%
  select(tm, w, l, ra = r)

df <- teams %>%
  select(tm, r, h, x2b, x3b, hr) %>%
  arrange(desc(hr)) %>%
  left_join(teams_standings) %>%
  filter(!(tm %in% c("Tm", "", "League Average"))) %>%
  mutate_at(c("h", "l", "x2b", "x3b", "w", "hr", "r", "ra"), as.numeric)

logos <- get_png_logos() %>%
  select(tm = full_name, logo = logoscoreboard)

df <- left_join(df, logos)

# exitoso pivot longer para hacer un mapa de color pero al final por la diferencia de los valores decido hacer un gt table
# df <- df %>%
#   pivot_longer(
#     cols =h:l,
#     names_to = "play",
#     values_to = "n"
#   )


#  Función para juntar Equipo logo y record -------------------------------



combine_word <- function(tm, w, l, logo) {
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;horizontal-align:left;font-size:12px'>{tm}</span></div>
        <div style='font-size:12px;line-height:18px;vertical-align:middle;horizontal-align:left'><span style ='font-weight:bold;color:grey;font-size:10px'><img src='{logo}'
    style='width:20px; height:20px;vertical-align:middle;horizontal-align:left'> {w} - {l} Record </span></div>"
  )
}


# gt table ----------------------------------------------------------------



df %>%
  arrange(desc(w),
          l) %>%
  mutate(
    combo = combine_word(tm, w, l, logo),
    combo = map(combo, gt::html)
  ) %>%
  select(combo, r, ra, h, x2b, x3b, hr) %>%
  gt() %>%
  cols_label(
    combo = gt::html("<span style='font-weight:bold;font-size:12px'>EQUIPO</span>"),
    r = gt::html("<span style='font-weight:bold;font-size:12px'>RUNS<sup style='font-weight:bold;font-size:6px'>1</sup></span>"),
    ra = gt::html("<span style='font-weight:bold;font-size:12px'>RUNS<sup style='font-weight:bold;font-size:6px'>2</sup></span>"),
    h = gt::html("<span style='font-weight:bold;font-size:12px'>HITS</span>"),
    hr = gt::html("<span style='font-weight:bold;font-size:12px'>HRUN</span>"),
    x2b = gt::html("<span style='font-weight:bold;font-size:12px'>2B</span>"),
    x3b = gt::html("<span style='font-weight:bold;font-size:12.7px'>3B</span>"),
  ) %>%
  tab_header(
    title = md("<img src='https://www.mlbstatic.com/team-logos/league-on-dark/1.svg' style='height:50px;'>"),
    subtitle = md(paste0("Pequeño Resumen por Equipos  | A ", format(Sys.Date(), format = "%d %B, %Y")))
  ) %>%
  tab_options(
    data_row.padding = px(4),
  ) %>%
  cols_align(
    align = "left",
    columns = c(combo)
  ) %>%
  cols_align(
    align = "center",
    columns = c(r:hr)
  ) %>%
  data_color(
    columns = c(r),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::sPBIGn",
        direction = 1
      ) %>% as.character(),
      domain = c(min(df$r), max(df$r))
    ))  %>%
  data_color(
    columns = c(ra),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::sPBIGn",
        direction = 1
      ) %>% as.character(),
      domain = c(min(df$ra), max(df$ra))
    )) %>%

  data_color(
    columns = c(h),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::sPBIGn",
        direction = 1
      ) %>% as.character(),
      domain = c(min(df$h), max(df$h))
    )
  ) %>%
  data_color(
    columns = c(hr),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::sPBIGn",
        direction = 1
      ) %>% as.character(),
      domain = c(min(df$hr), max(df$hr))
    )
  ) %>%
  data_color(
    columns = c(x2b),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::sPBIGn",
        direction = 1
      ) %>% as.character(),
      domain = c(min(df$x2b), max(df$x2b))
    )
  ) %>%
  data_color(
    columns = c(x3b),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::sPBIGn",
        direction = 1
      ) %>% as.character(),
      domain = c(min(df$x3b), max(df$x3b))
    )
  ) %>%
  cols_width(
    c(h) ~ px(35),
    c(hr) ~ px(35),
    c(x2b) ~ px(35),
    c(x3b) ~ px(35)
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.background.color = "#f4f4f4",
    column_labels.font.size = 12,
    table.font.size = 9,
    heading.title.font.size = 22.652,
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = 12,
    table.font.names = "Chivo",
    table.font.color = "black",
    data_row.padding = px(1),
    footnotes.font.size = 10,
    source_notes.font.size = 10,
    footnotes.padding = px(1)
  ) %>%
  tab_source_note(
    source_note = md("<div><sup style='font-weight:bold;font-size:6px'>1</sup><i>Carreras Realizadas<i></div>
                        <div><sup style='font-weight:bold;font-size:6px'>2</sup><i>Carreras Permitidas<i></div>
    <div><b>Grafica por</b> : <i>\n Ivo Villanueva @elcheff<i>
                       <div><b>Datos por</b> : \n<i>mlbstatsR y @baseball_ref<i>")
  ) %>%
  gtsave("jonrones.html")

# 30 de Agosto 2021 -----------------------------------------------------
# Ivo Villanueva ----------------------------------------------------------


  gtsave("jonrones.html")

# 30 de Agosto 2021 -----------------------------------------------------
# Ivo Villanueva ----------------------------------------------------------

