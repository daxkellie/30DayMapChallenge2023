
# packages
library(galah)
library(tidyverse)
library(ggtext)
library(here)
library(ozmaps)
library(sf)
library(showtext)
library(terra)
library(tidyterra)


library(tictoc)
library(beepr)

# frog counts
galah_call() |>
  filter(year == 1970) |>
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
    galah_filter(year == 1970,
                 decimalLongitude > 110) |>
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
# this took ~20 minutes, there are a lot of squares to query


# bind lists to data frame
counts_df <- counts_list |> bind_rows()

# merge polygons & counts
counts_joined <- oz_grid_tibble |>
  left_join(counts_df, join_by(id == id))


# extract centre
counts_centre <- counts_joined |> 
  mutate(centre = st_centroid(geometry))

# get xy of centre
obs_coords <- counts_centre |>
  select(count, centre, id) |>
  mutate(longitude = st_coordinates(centre)[,1],
         latitude = st_coordinates(centre)[,2]) |>
  rename(geometry = centre)


ggplot(obs_coords)+
  geom_sf(aes(geometry = geometry)) 


# subset to grid cells that are within land
keep_hexes <- st_intersection(obs_coords$geometry, aus)
keep_hexes <- keep_hexes |> 
  as.data.frame() |> 
  pull(row.id)
point_grid <- obs_coords |>
  filter(id %in% keep_hexes)

# Keep only points inside metropolitan France
point_grid <- point_grid |>
  mutate(dens=case_when(
    count<0.01~"A",
    count<~0.1~"B",
    count<1~"C",
    count<10~"D",
    count<100~"E",
    TRUE~"F"
  ))

# map
ggplot() +
  geom_sf(
    data= point_grid,
    mapping= aes(size = dens, 
                 geometry = geometry),
    colour = pilot::pilot_color("brown")
  ) +
  geom_sf(
    data = aus,
    colour = "grey60",
    fill = NA
  ) + 
  scale_size_manual(
    values = c(0.5,1,1.5,2,2.5, 3),
    label=c("<20 inhabitants","20 to 69","70 to 139","140 to 199","≥200 inhabitants")
  )
