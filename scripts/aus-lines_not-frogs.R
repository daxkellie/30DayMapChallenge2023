# Day 2: Lines (but actually, Day 4: Bad map)
# Title: Frogs? (in the end, no, just lines)

# NOTE: This is an incredibly innefficient way to get to the final 
# dataviz. The uniqueness of line-length is, I think, from the variation 
# in frog observation locations. However, I downloaded A LOT of data for it 
# to, in the end, be flattened into a line. The same could be achieved by 
# using less data but equally varied across Australia.

# remotes::install_github("AtlasOfLivingAustralia/galah@dev-version-2-0-mw")
library(galah)
library(tidyverse)
library(here)
library(ozmaps)
library(sf)
library(ggridges)
library(tictoc)
library(beepr)
library(showtext)


# taxonomic info
frog_taxa <- search_taxa(tibble(order = "anura", class = "amphibia"))

# frog counts
galah_call() |>
  identify(frog_taxa) |>
  galah_apply_profile(ALA) |>
  atlas_counts()



# get australia map
aus <- ozmap_country |>
  st_transform(crs = st_crs(4326))

# create grid
oz_grid <- st_make_grid(aus,
                        what = "polygons",
                        cellsize = 0.5,
                        square = TRUE,
                        flat_topped = TRUE)

# subset to grid cells that are within land
keep_hexes <- st_intersects(oz_grid, aus)
keep_hexes <- keep_hexes |> 
  as.data.frame() |> 
  pull(row.id)
oz_grid <- oz_grid[keep_hexes]

ggplot() +
  geom_sf(data = oz_grid)


# convert frog points to sf
frogs_sf <- frogs |> 
  drop_na(decimalLongitude, decimalLatitude) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
           crs = st_crs("WGS84"))

# convert aus square grid to 
oz_grid_tibble <- oz_grid |> 
  as_tibble() |>
  mutate(id = row_number())

# get frog counts for each square in the grid

# set email
galah_config(email = "dax.kellie@csiro.au")

# function to send a query for each polygon and return counts of frogs
get_counts <- function(polygon){
  
  frog_taxa <- search_taxa(tibble(order = "anura", 
                                  class = "amphibia"))
  
  # convert to wkt
  wkt_string <- st_as_text(oz_grid[[polygon]])
  
  # get counts
  result <- galah_call() |>
    galah_geolocate(wkt_string) |>
    galah_identify(frog_taxa) |>
    galah_filter(decimalLongitude > 110) |>
    galah_apply_profile(ALA) |>
    atlas_counts(limit = NULL)
  
  # light formatting to catch errors
  if(is.null(result)){
    tibble(count = NA, id = polygon)
  }else{
    result$id <- polygon
    result
  }
}

# download number of species for each polygon
tic()
counts_list <- map(seq_along(oz_grid_tibble$geometry), get_counts)
toc(); beepr::beep(2)
# this took 32 minutes, there are a lot of squares to query


# bind lists to data frame
counts_df <- counts_list |> bind_rows()


# merge polygons & counts
frog_counts_joined <- oz_grid_tibble |>
  left_join(counts_df, join_by(id == id))


# extract centre
frog_counts_centre <- frog_counts_joined |> 
  mutate(centre = st_centroid(geometry))

# get xy of centre
frog_coords <- frog_counts_centre |>
  select(count, centre, id) |>
  mutate(longitude = st_coordinates(centre)[,1],
         latitude = st_coordinates(centre)[,2])


## MAKE A MAP

# add font
font_add_google("Dosis", "dosis")
font <- "dosis"

# turn on showtext
showtext_auto()
showtext_opts(dpi = 320)


# Map (but it is really just a fancy way to make lines lol)
ggplot() +
  geom_density_ridges(data = frog_coords,
                      aes(x = longitude,
                          y = latitude,
                          group = longitude,
                          height = count),
                      colour = "#EBEBD3",
                      scale = 100,
                      stat = "identity",) +
  geom_density_ridges(data = frog_coords,
                      aes(x = longitude,
                          y = latitude,
                          group = latitude,
                          height = count),
                      colour = "#F6DB79",
                      scale = 1,
                      stat = "identity") +
  annotate(geom = "text", y = -35, x = 157, 
           label = "Australia", 
           hjust = "left", family = font, fontface = "bold", size = 12, 
           color = "#F6DB79") +
  annotate(geom = "text", y = -37, x = 158.5, 
           label = "but it's just lines", 
           hjust = "left", family = font, size = 5, color = "#F2F2F2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = '#223222', colour = NULL)
    )

ggsave(here("plots", "aus_lines.png"), height = 7, width = 10, dpi = 320)
