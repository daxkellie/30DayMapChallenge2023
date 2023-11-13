# South america / Choropleth
# A quick map of bird counts
# Date: 13 November, 2023

library(tidyverse)
library(galah) # remotes::install_github("AtlasOfLivingAustralia/galah-R")
library(here)
library(sf)
library(rnaturalearth)
library(rmapshaper)
# library(pilot) # remotes::install_github("olihawkins/pilot")
library(ggtext)

# Get outline of asian countries
s_america <- ne_countries(scale = "medium", continent = 'south america', returnclass = "sf")

# Get outline of cambodia
brazil <- s_america |>
  filter(name == "Brazil")


# Bioregion shapefiles
# source: https://www.codegeo.com.br/2013/04/shapefiles-do-brasil-para-download.html
states <- st_read(here("data",
                           "estados_2010",
                           "estados_2010.shp")) |>
  ms_simplify(keep = 0.1) |> 
  st_transform(crs = st_crs("WGS84")) |> 
  st_make_valid()

# Set coordinate projection
states <- ecoregions |>
  st_transform(crs = st_crs(4326))

brazil <- brazil |>
  st_transform(crs = st_crs(4326))

# Get brazil data
# turns out you need a registered email
# galah_config(email = "your-email-here", atlas = "brazil")

counts <- galah_call() |>
  identify("aves") |>
  group_by(cl10386) |>
  count() |>
  collect()

# fix names
# setdiff(counts$cl10386, ecoregions$nome)
setdiff(states$nome, counts$cl10386)

ecoregions |>
  filter(str_detect(nome, "Pi"))

counts <- counts |>
  mutate(cl10386 = case_when(
    cl10386 == "Sao Paulo" ~ "São Paulo",
    cl10386 == "Parana" ~ "Paraná", 
    cl10386 == "Para" ~ "Pará", 
    cl10386 == "Paraiba" ~ "Paraíba",  
    cl10386 == "Goias" ~ "Goiás",   
    cl10386 == "Ceara" ~ "Ceará",    
    cl10386 == "Espirito Santo" ~ "Espírito Santo",     
    cl10386 == "Rondonia" ~ "Rondônia",     
    cl10386 == "Maranhao" ~ "Maranhão",      
    cl10386 == "Amapa" ~ "Amapá",      
    cl10386 == "Piaui" ~ "Piauí",      
    TRUE ~ cl10386)
  )


state_counts <- states |>
  left_join(counts, by = join_by(nome == cl10386)) |>
  filter(!is.na(count))

# Get counts per km^2
state_counts_km <- state_counts |>
  rowwise() |>
  mutate(area_km2 = as.numeric(st_area(geometry))/1000,
         counts_km2 = count/area_km2) |>
  replace_na(list(counts_km2 = 0))


# make galah palette
# Colours inspired by this bird: https://en.wikipedia.org/wiki/Hoatzin

library(showtext)
# add font
font_add_google("Dosis", "dosis")
font <- "dosis"

# turn on showtext
showtext_auto()

# Map
ggplot() +
  geom_sf(data = state_counts_km,
          mapping = aes(fill = count),
          colour = "#85b1e3",
          linewidth = 0.3) +
  scale_fill_stepsn(
    name = "Number of<br>observations<br> per km squared",
    labels = c("0.001", "0.01", ".1"),
    trans = "log10", # log-transform
    colours = hoatzin,
    na.value = "grey10",
    guide = guide_colorsteps(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "left",
      draw.ulim = TRUE,
      draw.llim = TRUE,
    )
  ) + 
  geom_sf(data = brazil,
          fill = NA,
          colour = "#85b1e3",
          linewidth = 1) +
    labs(
      title = "Brazil",
      subtitle = "Bird observations by state",
      caption = "<br>Source: Sistema de Informação sobre a Biodiversidade Brasileira | Downloaded with {**galah**}<br>
      Dataviz: Dax Kellie"
    ) +
  theme_void() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(family = font, size = 38, hjust = 0.5),
    plot.subtitle = element_text(family = font, size = 18, hjust = 0.5),
    plot.caption = ggtext::element_markdown(family = font, hjust = 0.5, colour = "#7e2d06", size = 10),
    legend.title = ggtext::element_markdown(hjust = 0.5),
    plot.background = element_rect(fill = "white", colour = "white"),
    # panel.background = element_rect(fill = "#EDF6CB", colour = "#EDF6CB"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )


showtext_opts(dpi = 320) # Fixes text for saving in 320 dpi
ggsave(here("plots", "brazil_green.png"), height = 7, width = 7, dpi = 320)
