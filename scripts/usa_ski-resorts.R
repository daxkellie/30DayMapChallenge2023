# USA ski resorts
# Date: 11 November, 2023

library(tidyverse)
library(here)
library(glue)
library(sf)
library(tigris)
library(showtext)

# read in ski resort data
ski_resorts <- read_csv(here("data", "ski_resort_stats.csv"))

ski_resorts <- ski_resorts |>
  select(lat, lon, resort_name, state, vertical, green_percent, blue_percent, black_percent)

# convert coordinates to sf geometry
ski_resorts_sf <- ski_resorts |>
  drop_na(lat) |>
  st_as_sf(coords = c("lon", "lat"))

# set projection
ski_resorts_sf <- ski_resorts_sf |>
  sf::st_set_crs(4326)

# ski_resorts |> 
#   filter(is.na(vertical))


# get usa map
usa_states <- states()

# filter to main USA
usa_states_main <- usa_states |> 
  filter(!STUSPS %in% c('MP', 'VI', 'PR', 'GU', 'AS')) |> 
  filter(!STUSPS %in% c("AK", "HI", "DC"))

# get alaska too for an inset later
alaska <- usa_states |>
  filter(NAME == "Alaska")

# set projection
usa_states_main <- usa_states_main |>
  st_transform(crs = st_crs(4326))

# highest vertical
vert <- ski_resorts_sf |> 
  slice_max(vertical, n = 3)

# create label, add to data frame
n_vert <- vert$vertical |> scales::comma()

vert <- vert |>
  mutate(
vert_label = glue("
                   {vert$resort_name}
                   {n_vert} ft
                   "))



# ------ This wasn't used in the end ------------
# highest proportion of black runs
black <- ski_resorts_sf |> 
  slice_max(black_percent, n = 1)

# highest proportion of green runs
green <- ski_resorts_sf |> 
  slice_max(green_percent, n = 1)

# -----------------------------------------------

# add font
font_add_google("Dosis", "dosis")
font <- "dosis"

# turn on showtext
showtext_auto()
showtext_opts(dpi = 320)



## MAKE MAPS ------------------------------------

main <- ggplot() +
  geom_sf(data = usa_states_main, 
          fill = "#F8EADE",
          colour = "#DB995A",) + 
  geom_sf(data = ski_resorts_sf,
          aes(size = vertical),
          colour = "#4394C7",
          fill = "#D8F1FD",
          alpha = 0.8,
          shape = 24) +
  geom_sf(data = vert,
          inherit.aes = FALSE,
          aes(size = vertical),
          colour = "#875988",
          fill = "#875988",
          alpha = 0.8,
          linewidth = 2,
          shape = 24) +
  geom_sf_text(data = vert,
               inherit.aes = FALSE,
               aes(label = vert_label, 
                   geometry = geometry),
                colour = "#624063",
                hjust = 0.5,
               size = 2.2,
               lineheight = 1.0,
               family = "dosis",
               fontface = "bold",
               nudge_x = c(4.5, 5, 3.2), nudge_y = c(-0.5, 0.025, 0.7)) +
  coord_sf(xlim = c(-125, -67),
           ylim = c(22, 52)) + 
  theme_void() + 
  labs(
    title = "Ski resorts with the biggest vertical drop",
    caption = "Data visualisation: Dax Kellie\nSource: Ewiseman 2017 ski_resort_stats.csv\n"
       ) +
  guides(size = guide_legend(title = "vertical (ft)", 
                             override.aes = list(colour = "#4394C7",
                                                 fill = "#D8F1FD")
                             )) +
  theme(
    plot.title = element_text(family = font,
                              colour = "#4394C7",
                              hjust = 0.5, 
                              vjust = 0.5,
                              size = 27),
    legend.position = "bottom",
    panel.background = element_rect(fill = '#F4DFCD', colour = NA),
    plot.background = element_rect(fill = '#F4DFCD', colour = NA),
    plot.margin = unit(c(1,1,0,1), "cm"),
    plot.caption = element_text(family = font,
                                colour = "#DB995A",
                                hjust = 1,
                                margin = margin(t = 1, r = -0.8, unit = "cm"))
  )


alaska_map <- ggplot() +
  geom_sf(data = alaska, 
          fill = "#F8EADE",
          colour = "#DB995A",) + 
  geom_sf(data = ski_resorts_sf |> filter(state == "Alaska"),
          aes(size = vertical),
          colour = "#4394C7",
          fill = "#D8F1FD",
          alpha = 0.8,
          shape = 24) +
  theme_void() +
  coord_sf(xlim = c(-180, -130)) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = '#F4DFCD', colour = "#DB995A"),
    plot.background = element_rect(fill = '#F4DFCD', colour = NA),
  )

# combine main + inset
library(cowplot)
combined_map <- ggdraw(main) +
  draw_plot(alaska_map, 
            x = 0.03, y = -0.16, 
            width =0.23, height = 0.9)

combined_map + 
  panel_border(color = "brown", remove = FALSE) +
  theme(plot.background = element_blank())


ggsave(here::here("plots", "usa_ski-resorts.png"),
       height = 5.75, width = 6.9, dpi = 320)
