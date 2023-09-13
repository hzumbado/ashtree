# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(spatstat)
library(ggspatial)
library(terra)
library(tidyverse)

# paths -------------------------------------------------------------------

datap <- file.path('data/processed')

# data --------------------------------------------------------------------

source("scripts/rasterdf.R")

ashtrees <-
  read_csv(paste0(datap, '/ash_california.csv'))

ashtrees_sf <-
  ashtrees %>%
  st_as_sf(
    coords = c(
      longitude = 'x',
      latitude = 'y'),
    #remove = FALSE,
    crs = 4326)

california <-
  read_sf('shapefiles/california_4326.gpkg', quiet = T) %>%
  st_transform(crs = st_crs(ashtrees_sf))

ashtrees_sf <-
  ashtrees_sf %>%
  st_filter(california)

xy <-
  ashtrees_sf %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(x = 'X', y = 'Y')

ashtrees_sf <-
  ashtrees_sf %>%
  bind_cols(xy) %>%
  select(species, x, y, everything())


# random points -----------------------------------------------------------

set.seed(1)

rp <- spatSample(
  vect(california),
  10000,
  method = "random") %>%
  st_as_sf() %>%
  st_coordinates(rp) %>%
  as_tibble() %>%
  rename(x = 'X',
         y = 'Y') %>%
  mutate(ash = 0) %>%
  st_as_sf(coords =
             c('x', 'y'),
           crs = 4326,
           remove = F) %>%
  select(ash, everything())

ash_sf2 <-
  ashtrees_sf %>%
  mutate(ash = 1) %>%
  select(ash, x, y) %>%
  bind_rows(rp)

california %>%
  tm_shape() +
  tm_graticules(lines = F) +
  tm_polygons() +
  tm_shape(
    ashtrees_sf) +
  tm_dots(title = 'Species',
    col = 'name',
    palette = 'Set2',
    size = 0.1,
    shape = 21) +
  tm_scale_bar(
    position = c('left', 'bottom')) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = c("right", "top"))

boundary_utm <- st_transform(california, 32610)

ash_utm <- st_transform(ash_sf2, 32610)

ash_ppp <- as.ppp(ash_utm)

Window(ash_ppp) <- as.owin(boundary_utm)

ash_im <-
  Smooth.ppp(
    ash_ppp,
    sigma = 10000,
    eps = c(1000, 1000))

ash_grid <- rast(ash_im)

crs(ash_grid) <-"epsg:32610"

plot(ash_grid)

a <-
  tm_shape(ash_grid, is.master = T) +
  tm_raster(
    legend.show = FALSE,
    title = 'Ashtree density',
    style = 'cont',
    #palette = c('lightyellow', 'darkgreen' )
     palette = 'Reds',
    ) +
  tm_shape(california) %>% +
  tm_borders(lwd = 1, col = 'gray20') +
  tm_shape(ashtrees_sf) +
  tm_dots(
    title = 'Species',
    labels = c(
    'Foothill Ash',
    'Oregon Ash',
    'Single-leaf Ash',
    'Velvet Ash'),
    col = 'name',
    palette = 'Accent',
    size = 0.08,
    shape = 22) +
  tm_scale_bar(
    position = c('left', 'bottom'),
    breaks = c(0, 100, 200)) +
  tm_credits(
    'Hotspots of native ash (Fraxinus spp.)
    based on occurrence records available
    at calflora.org',
    size = 1,
    position = c('right','top'),
    just = 'center') +
  tm_layout(
    frame = TRUE,
    legend.position = c(0.7, 0.55),
    legend.text.size = 1,
    legend.title.size = 1.2,
    legend.title.fontface = 'bold')

a

tmap_save(
  a,
  'output/figures/ash_density.jpg',
  height = 7,
  width = 8,
  dpi = 300)

ash_df <- rasterdf(ash_grid)


ggplot(data = ash_df) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = value)) +
  scale_fill_gradient(name = "Subalpine Fir",
                      low = "lightyellow",
                      high = "darkgreen",
                      na.value = NA) +
  geom_sf(data = boundary_utm,
          fill = NA) +
  annotation_scale(location = 'bl') +
  coord_sf(expand = F) +
  theme_void()






ggsave('p1.png', width = 7, height = 8)

