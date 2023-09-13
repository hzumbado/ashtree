
# setup ------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

# paths -------------------------------------------------------------------

data <- file.path('data/raw')
datap <- file.path('data/processed')

# data --------------------------------------------------------------------

ashtrees <-
  list(
    oregon_ash = read_csv(paste0(data, '/oregon_ashtree.csv')) %>%
  mutate(
    Species = 'Fraxinus latifolia',
         'Common Name' = 'Oregon ash'),

velvet_ash = read_csv(paste0(data, '/velvet_ashtree.csv')) %>%
  mutate(
    Species = 'Fraxinus velutina',
         'Common Name' = 'Velvet ash'),

foothill_ash = read_csv(paste0(data, '/foothill_ashtree.csv')) %>%
  mutate(
    Species = 'Fraxinus dipetala',
         'Common Name' = 'Foothill ash'),

single_ash = read_csv(paste0(data, '/single_ashtree.csv')) %>%
  mutate(
    Species = 'Fraxinus anomala',
    'Common Name' = 'Single-leaf ash')) %>%
  map(~.x %>%
  set_names(names(.) %>% tolower()) %>%
  select(
    species,
    x = longitude,
    y = latitude,
    date,
    name = 'common name',
    accuracy = 'accuracy: square meters',
    source,
    quality = 'location quality')) %>%

  list2env(.GlobalEnv)

# dataset  ----------------------------------------------------------------

ash_dataset <-
  bind_rows(
  oregon_ash,
  velvet_ash,
  foothill_ash,
  single_ash)

ash_dataset %>%
  write_csv(paste0(datap, '/ash_california.csv'))
