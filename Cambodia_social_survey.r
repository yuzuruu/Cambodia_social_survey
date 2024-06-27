####################################################################
# Social survey in Cambodia
# Original 25th. June 2024
# Revised 
# by Yuzuru Utsunomiya, Ph. D.
# 
####################################################################
# 
# ----- load.library -----
library(tidyverse)
library(sf)
library(osmdata)
library(ggmap)
library(ggsn)
# 
# ----- read.data -----
# shapefiles (province)
kratie_map_province_01 <- 
  sf::read_sf("../Cambodia_ncp/KHM_adm/KHM_adm1.shp") %>% 
  dplyr::filter(NAME_1 == "Krâchéh") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) 
# shapefiles (district)
kratie_map_province_02 <- 
  sf::read_sf("../Cambodia_ncp/KHM_adm/KHM_adm2.shp") %>% 
  dplyr::filter(NAME_1 == "Krâchéh") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) 
# shapefiles (subdistrict)
kratie_map_province_03 <- 
  sf::read_sf("../Cambodia_ncp/KHM_adm/KHM_adm3.shp") %>% 
  dplyr::filter(NAME_1 == "Krâchéh") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) 

# make a boundary box for 
kratie_bbox <- 
  sf::read_sf("../Cambodia_ncp/KHM_adm/KHM_adm1.shp") %>% 
  dplyr::filter(NAME_1 == "Krâchéh") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) %>% 
  sf::st_transform(4326) %>% 
  sf::st_bbox()
# obtain information from Open Street Map
# street (small road)
# streets <- 
#   kratie_bbox %>%
#   osmdata::opq() %>%
#   osmdata::add_osm_feature(
#     key = "highway",
#     value = c(
#       # "footway", 
#       # "service", 
#       "track"
#       # "residential", 
#       # "living_street",
#       # "unclassified"
#     )
#   ) %>%
#   osmdata::osmdata_sf()
# save the results
# The OSM often ignore our request. 
# Save the data whenever it works.
# readr::write_rds(streets, "streets.rds")
# load the data
# streets <- readr::read_rds("streets.rds")
# road
# road <- 
#   kratie_bbox %>%
#   osmdata::opq() %>%
#   osmdata::add_osm_feature(
#     key = "highway",
#     value = c(
#       "motorway", 
#       "motorway_junction",
#       "motorway_link",
#       "primary", 
#       "primary_link",
#       "secondary", 
#       "secondary_link",
#       "tertiary",
#       "tertiary_link",
#       "trunk",
#       "trunk_link"
#     )
#   ) %>%
#   osmdata::osmdata_sf()
# save
# readr::write_rds(road, "road.rds")
road <- readr::read_rds("road.rds")
# combine the roads altogether
ways <- 
  road$osm_lines %>% 
  dplyr::mutate(id = rownames(road$osm_lines)) %>% 
  dplyr::select(id, geometry) %>% 
  # # If we need smaller ways, run the followings.
  # dplyr::bind_rows(
  #   ., 
  #   streets$osm_lines %>% dplyr::mutate(id = rownames(streets$osm_lines)) %>% dplyr::select(id, geometry)) %>% 
  sf::st_union()
# remove ways not in the Kratie province
inside_ways <- sf::st_intersection(ways, kratie_map_province_03) %>% sf::st_union()
# 
inside_ways <- sf::st_transform(inside_ways, crs = 4326)
# read building footprint data
# NOTE
# 
object_Cambodia_all <- 
  readr::read_rds(
    "object_Cambodia_all.rds"
  )
# select a subset including Kratie province
object_Cambodia_kratie <- 
  object_Cambodia_all %>% 
  dplyr::filter(province_name == "Krâchéh")
# provide CRS (WGS84 = 4326)
sf::st_transform(object_Cambodia_kratie$centroid, 4326)
sf::st_transform(object_Cambodia_all$geometry, crs = 4326)
# calculate distance bewteen centroid of building and road
object_Cambodia_kratie <- 
  object_Cambodia_kratie %>%
  as_tibble() %>% 
  dplyr::mutate(
    distance = sf::st_distance(centroid, inside_ways)
  )
# 
# ----- draw.figure -----
# Read the Google API key
# NOTE
# THIS KEY NEEDS SECURE MANAGEMENT.
source("map_key.r")
# Fetch the map
kratie  <-  
  ggmap::get_map(
    location = c(
      left = as.numeric(kratie_bbox$xmin), 
      bottom = as.numeric(kratie_bbox$ymin), 
      right = as.numeric(kratie_bbox$xmax), 
      top = as.numeric(kratie_bbox$ymax)), 
    zoom = 9,
    maptype = "hybrid",
    source = "google"
    )
# print the map
ggmap::ggmap(kratie)
# draw a overlaid map
kratie_multilayer_map <- 
  # ggplot() +
  ggmap::ggmap(kratie) +
  # geom_sf() +
  # district-level administrative boundaries of Kratie province
  geom_sf(
    data = kratie_map_province_01,
    inherit.aes = FALSE,
    color = NA,
    fill = "white",
    size = 0.1,
    linetype = "dotted",
    alpha = 0.25
  ) +
  geom_sf(
    data = inside_ways,
    inherit.aes = FALSE,
    color = "orange",
    fill = "orange",
    size = 0.2,
    alpha = 1.0
  ) +
  # commune-level administrative boundaries of Kratie province
  geom_sf(
    data = kratie_map_province_03,
    inherit.aes = FALSE,
    color = "black",
    fill = NA,
    size = 0.5,
    linetype = "dotted",
    alpha = 1.0
  ) +
  geom_sf(
    data = kratie_map_province_02,
    inherit.aes = FALSE,
    color = "black",
    fill = NA,
    size = 2.0,
    alpha = 1.0
    ) +
  # province-level administrative boundaries
  geom_sf(
    data = kratie_map_province_01,
    inherit.aes = FALSE,
    fill = NA,
    color = "white",
    size = 20.0,
    alpha = 1.0
  ) +
  geom_sf(
    data = object_Cambodia_kratie,
    aes(geometry = geometry), 
    inherit.aes = FALSE,
    fill = NA,
    color = "white",
    size = 1.0,
    alpha = 1.0
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    caption = "\U00a9 OpenStreetMap contributors"
  ) +
  theme_classic()
# print
kratie_multilayer_map
# save
ggsave(
  "kratie_multilayer_map.pdf",
  plot = kratie_multilayer_map,
  width = 500,
  height = 500,
  units = "mm"
)

