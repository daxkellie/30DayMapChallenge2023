# Day 6: Cambodia, Cardamom National Park
# Date: 5 November, 2023

library(tidyverse)
library(sf)
library(rnaturalearth)
library(here)
library(rmapshaper)
library(elevatr)
library(ggridges)
library(showtext)
library(ggtext)

# Get outline of asian countries
asia <- ne_countries(scale = "medium", continent = 'asia', returnclass = "sf")

# Get outline of cambodia
cambodia <- ne_countries(scale = "medium", continent = 'asia', returnclass = "sf") |>
  filter(name == "Cambodia")

ggplot() +
  geom_sf(data = asia) +
  geom_sf(data = cambodia,
          fill = "green")

# get national protected areas
protected_areas <- st_read(here("data",
                               "npa_bci",
                               "npa_bci.shp"),
                          quiet = TRUE) |>
  ms_simplify(keep = 0.1) |>
  # st_transform(crs = st_crs("WGS84")) |>
  st_make_valid()

# Filter to only protected areas with "Card" in the name
cardamom <- protected_areas |> 
  filter(stringr::str_detect(name, "Card")) |>
  st_transform(crs = st_crs(4326))


ggplot() + 
  geom_sf(data = cambodia) +
  geom_sf(data = cardamom)

cambodia


# get elevation data for cambodia
# Thanks many times over to Ryan Hart for your elevation ggridges code!
# Source: https://github.com/curatedmess/30DayMapChallenge/blob/main/2022/11042022/green_mountains.R
cambodia_elev <- get_elev_raster(locations = cambodia, 
                                 z = 11, 
                                 clip = "locations", 
                                 neg_to_na = "TRUE")
# note: this takes like 10 minutes

# reduce the number of rows
reduced <- raster::aggregate(cambodia_elev, fact = 30)

## crop and mask elevation to only where cardamom national park is
elev_crop <- raster::crop(reduced, st_bbox(cardamom))
elev_mask <- raster::mask(elev_crop, cardamom)

# points <- data.frame( raster::rasterToPoints( reduced ) )
points_cardamom <- data.frame( raster::rasterToPoints( elev_mask ) )

# create a data frame to allow for plotting
df_cardy <- as.data.frame(points_cardamom, xy = TRUE) |> 
  rename_with(.cols = 3, ~"elevation") |>
  drop_na(elevation) 

# check
ggplot() + 
  geom_density_ridges(data = df_cardy, aes(x = x, y = y, group = y, height = elevation), 
                      stat = "identity", size = 0.1, scale = 20, fill = NA)


## Make map ----------------------------------------------------------------

# add font 
font_add_google(name = "Abel", family = "Abel")
font_add_google(name = "Noto Sans", family = "Noto Sans")
font <- "Abel"

# turn on showtext
showtext_auto()
showtext_opts(dpi = 320)

# inset map
plot_small <- ggplot() +
  geom_sf(data = asia,
          fill = "#266940",
          linewidth = 0.6,
          colour = "#102D1B") +
  geom_sf(data = cambodia,
          colour = "#FC4622",
          linewidth = 0.5,
          fill = "#08B2E3") + 
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#102D1B", colour = NA),
    plot.background = element_rect(colour = NA)
  )

# Main Cambodia map
main_plot <- ggplot() + 
  geom_sf(data = cambodia,
          fill = NA,
          colour = "#08B2E3",
          linewidth = 0.5) +
  geom_sf(data = cardamom,
          fill = "#63C788",
          colour = NA) +
  geom_density_ridges(data = df_cardy, aes(x = x, y = y, group = y, height = elevation),
                      stat = "identity", size = 0.21, scale = 8, fill = NA, colour = "#205A36") +
  coord_sf(ylim = c(9.8, 15.2),
           xlim = c(97.3, 107.5)) +
  annotate(geom = "text", y = 13, x = 97.8, 
           label = "Cambodia", 
           hjust = "left", family = font, fontface = "bold", size = 27, 
           color = "#08B2E3") +
  annotate(geom = "text", y = 12.05, x = 99.5, 
           label = "Cardamom \nNational Park", 
           lineheight = .7,
           hjust = "middle", family = font, size = 17, color = "#63C788") +
  ggtext::geom_textbox(aes(x = 97.4,
                           y = 10.12),
                       label = "The mountains of Cardamom National Park are home  
                       to some of the most diverse, endangered species in the world,
                       including the Malay sun bear, Pileated gibbon, Clouded
                       leopard, Great hornbill, Asian elephant & Sunda pangolin.",
                       hjust = 0, halign = 0.5,
                       vjust = 0,
                       family = "Abel",
                       colour = "white",
                       box.colour = NA,
                       fill = NA,
                       lineheight = 1.4,
                       width = unit(23, "lines"),
                       size = 5,
                       fontface = "bold") +
  theme_void() +
  labs(caption = "Data visualisation: Dax Kellie <br>Source: Open Development Cambodia <br>Elevation data from {elevatr} package <br>") +
  theme(
    panel.background = element_rect(fill = '#102D1B', colour = NA),
    plot.background = element_rect(fill = '#102D1B', colour = NA),
    text = element_text(family = "Noto Sans"),
    plot.caption = ggtext::element_textbox_simple(family = "Noto Sans",
                                                  colour = "grey80",
                                                  halign = 1,
                                                  margin = margin(t = 0, r = 0.5, unit = "cm"))
  )

main_plot

# Thanks Cara Thompson for your lovely ggtext code!
# Source: https://github.com/cararthompson/30DayChartChallenge2023/blob/main/scripts/day06_owid.R


# combine main + inset
library(cowplot)
combined_map <- ggdraw(main_plot) +
  draw_plot(plot_small, 
            x = 0.05, y = 0.68, 
            width =0.3, height = 0.25)

combined_map + 
  panel_border(color = NA, remove = TRUE) +
  theme(plot.background = element_blank())


ggsave(here::here("plots", "cambodia.png"), height = 7.5, width = 12, dpi = 320)


