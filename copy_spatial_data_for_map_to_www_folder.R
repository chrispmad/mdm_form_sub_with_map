library(sf)
library(tidyverse)
library(bcdata)

# Point data - from shapefile
points = sf::read_sf("data/all_sp_records_September-05-2024.shp")

points_simple = points |> 
  dplyr::select(Species = Species,
                Date,
                Record_type = Rcrd_ty)

# Point data - from geodatabase
# points_gdb_inc = sf::st_read("data/2.1 SpatialData_Mesocarnivores_11Oct.gdb/",
#                          layer = "IncidentalObs_Mesocarns_11Oct") |> 
#   st_as_sf() |> 
#   dplyr::select(Species = SCIENTIFIC_NAME,
#                 Date = OBSERVATION_DATE) |> 
#   dplyr::mutate(Record_type = "incidental")  |> 
#   dplyr::rename(geometry = GEOMETRY) |> 
#   sf::st_transform(4326)

# points_gdb_survey = sf::st_read("data/2.1 SpatialData_Mesocarnivores_11Oct.gdb/",
#                              layer = "SurveylObs_Mesocarns_11Oct") |> 
#   st_as_sf() |> 
#   dplyr::select(Species = SCIENTIFIC_NAME, 
#                 Date = SURVEY_START_DATE) |> 
#   dplyr::mutate(Record_type = "survey") |> 
#   dplyr::rename(geometry = GEOMETRY) |> 
#   sf::st_transform(4326)
# 
# # Combine point data.
# all_points = dplyr::bind_rows(
#   points_gdb_inc,
#   points_gdb_survey,
#   points_simple
# )
# 
# all_points = sf::st_make_valid(all_points)

all_points= sf::st_make_valid(points_simple)

# Combine some different species names into bigger categories.
all_points = all_points |> 
  dplyr::mutate(Species = dplyr::case_when(
    Species == "Neovison vison" ~ 'Neogale vison',
    Species == "Martes pennanti" ~ "Pekania pennanti",
    Species == "Lontra canadensis pacifica" ~ "Lontra canadensis",
    Species == "Mustela erminea" ~ "Mustela richardsonii",
    Species %in% c("Gulo gulo luscus","Gulo gulo vancouverensis") ~ "Gulo gulo",
    Species %in% c("Mustela frenata","Mustela frenata altifrontalis") ~ 'Neogale frenata',
    Species == 'Procyon lotor pacificus' ~ 'Procyon lotor',
    Species == "Taxidea taxus jeffersonii" ~ 'Taxidea taxus',
    T ~ Species
  ))

# make {sf} grid
bc = bcmaps::bc_bound() |> 
  dplyr::summarise() |> 
  sf::st_transform(4326)

bc_3km_grid = st_make_grid(
  bc,
  # n = c(20,20),
  n = c(100,100),
  square = F
) |> 
  st_intersection(
    bc
  ) |> 
  sf::st_as_sf() |> 
  dplyr::mutate(cell_id = dplyr::row_number()) |> 
  dplyr::filter(as.numeric(st_area(x)) > 10)

# st_area(bc_3km_grid[1,]$x)/30003447

# ggplot() + geom_sf(data = bc_3km_grid)

# bc_3km_grid = sf::st_make_valid(bc_3km_grid)

all_points_w_grid = sf::st_join(all_points, bc_3km_grid)

point_count_by_grid = all_points_w_grid |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(Species, cell_id) |> 
  dplyr::summarise(number_records = n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(Species = stringr::str_replace_all(Species," ","_")) |> 
  pivot_wider(names_from = Species, values_from = number_records)

bc_3km_grid = bc_3km_grid |> 
  dplyr::left_join(point_count_by_grid)

saveRDS(bc_3km_grid, "app/www/bc_3km_grid.rds")
# saveRDS(all_points, "app/www/point_data.rds")

rm(list = ls())
gc()

# IUCN rangemap polygons
iucn_ranges = list.files("data/IUCN Distribution-Range_BC/",
           pattern = '.shp$',
           full.names = T) |> 
  lapply(\(x) sf::read_sf(x))

iucn_ranges = dplyr::bind_rows(iucn_ranges)

# Correct one scientific name.
iucn_ranges = iucn_ranges |> 
  dplyr::mutate(SCI_NAME = dplyr::case_when(
    SCI_NAME == "Martes pennanti" ~ "Pekania pennanti",
    T ~ SCI_NAME
  ))

# Clip to BC
bc = bcmaps::bc_bound() |> dplyr::summarise() |> sf::st_transform(4326)

# iucn_ranges = sf::st_intersection(iucn_ranges,bc)

iucn_ranges_v = terra::vect(iucn_ranges)

# Aggregate polygons based on species ID.
iucn_ranges_v = terra::aggregate(iucn_ranges_v, by = "SCI_NAME")

iucn_ranges_v = terra::crop(iucn_ranges_v, terra::vect(bc)) |> 
  st_as_sf()

# unique_sci_names = unique(iucn_ranges$SCI_NAME)

# i = 14

# ggplot() + 
#   geom_sf(data = bc) + 
#   geom_sf(data = iucn_ranges |> dplyr::filter(SCI_NAME == unique_sci_names[i]), fill = 'blue') +
#   geom_sf(data = test |> dplyr::filter(SCI_NAME == unique_sci_names[i]), fill = 'red', alpha = 0.8) + 
#   labs(title = unique_sci_names[i])

# sf::st_is_valid(test)

# iucn_ranges = st_make_valid(iucn_ranges)

iucn_ranges = sf::st_as_sf(iucn_ranges_v)

iucn_ranges_s = rmapshaper::ms_simplify(iucn_ranges)

# sf::st_is_valid(iucn_ranges_s)

# # Combine by species
# iucn_ranges = iucn_ranges |> 
#   dplyr::group_by(SCI_NAME) |> 
#   dplyr::summarise()

saveRDS(iucn_ranges_s, "app/www/IUCN_ranges.rds")

parks_in_bc = bcdc_query_geodata("bc-parks-ecological-reserves-and-protected-areas") |>
  collect()

parks_in_bc_s = sf::st_simplify(parks_in_bc, dTolerance = 50)

saveRDS(parks_in_bc_s,"app/www/parks_in_bc.rds")

