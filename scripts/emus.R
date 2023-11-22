# Emu observations in Australia
# Date: 22 November, 2023

library(galah)
library(tidyverse)
library(sf)
library(ozmaps)
library(ggpointdensity)
library(MetBrewer)
library(showtext)

galah_config(email = "dax.kellie@csiro.au")

# emu counts
count <- galah_call() |>
  identify("Dromaius novaehollandiae") |>
  galah_apply_profile(ALA) |> # set of data quality filters
  atlas_counts()

# download observations
emus <- galah_call() |>
  identify("Dromaius novaehollandiae") |>
  galah_apply_profile(ALA) |> # set of data quality filters
  select(decimalLatitude, decimalLongitude, scientificName) |>
  atlas_occurrences()

# get aus map
aus <- ozmap_country |>
  st_transform(crs = st_crs(4326))

# get font
font_add_google("Dosis")
font <- "Dosis"

showtext_auto()

# MAP
ggplot() +
  geom_sf(data = aus,
          fill = "white",
          colour = "#405548",
          linewidth = 1.03)+
  geom_pointdensity(data = emus, 
                    aes(x = decimalLongitude, y = decimalLatitude),
                    alpha = 0.5) +
  MetBrewer::scale_color_met_c("Hokusai3") +
  guides(colour = guide_colourbar(title = "Number of overlapping\nobservations")) +
  labs(title = "Emu observations in Australia",
       subtitle = glue::glue("N = {count$count[1] |> scales::comma()}")) +
  theme_void() + 
  theme(
    text = element_text(family = font),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 26),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", colour = NA)
  )


# save
showtext_opts(dpi = 320)

ggsave(here::here("plots", "emus.png"),
       height = 8, width = 8, unit = "in",
       dpi = 320)
  
  
  
