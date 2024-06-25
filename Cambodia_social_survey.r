library(tidyverse)
library(sf)
library(osmdata)
library(ggmap)

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


streets <- 
  kratie_bbox %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      # "footway", 
      # "service", 
      "track"
      # "residential", 
      # "living_street",
      # "unclassified"
    )
  ) %>%
  osmdata::osmdata_sf()
streets <- readr::read_rds("streets.rds")


# road
road <- 
  kratie_bbox %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "motorway", 
      "motorway_junction",
      "motorway_link",
      "primary", 
      "primary_link",
      "secondary", 
      "secondary_link",
      "tertiary",
      "tertiary_link",
      "trunk",
      "trunk_link"
    )
  ) %>%
  osmdata::osmdata_sf()
road
road <- readr::read_rds("road.rds")

ways <- 
  road$osm_lines %>% dplyr::mutate(id = rownames(road$osm_lines)) %>% dplyr::select(id, geometry) %>% 
  dplyr::bind_rows(., streets$osm_lines %>% dplyr::mutate(id = rownames(streets$osm_lines)) %>% dplyr::select(id, geometry))

inside_ways <- sf::st_intersection(ways, kratie_map_province_03)


source("map_key.r")
# Fetch the map
kratie  <-  
  ggmap::get_map(
    location = c(left = as.numeric(kratie_bbox$xmin), bottom = as.numeric(kratie_bbox$ymin), right = as.numeric(kratie_bbox$xmax), top = as.numeric(kratie_bbox$ymax)), 
    zoom = 9,
    source = "google"
    )
ggmap::ggmap(kratie)



kratie_multilayer_map <- 
  # ggplot() +
  ggmap::ggmap(kratie) +
  # geom_sf() +
  # district-level administrative boundaries of Kratie province
  geom_sf(
    data = kratie_map_province,
    inherit.aes = FALSE,
    color = "black",
    fill = "grey50",
    size = 0.1,
    linetype = "dotted",
    alpha = 0.4
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
  theme_classic()

#   # paint provinces other than Kratie in white
#   geom_sf(
#     data = adm_01_ex_target,
#     inherit.aes = FALSE,
#     color = NA,
#     fill = "white",
#     size = 20.0,
#     alpha = 1.5
#   ) +
#   # province-level administrative boundaries
#   geom_sf(
#     data = adm_01_target,
#     inherit.aes = FALSE,
#     fill = NA,
#     color = "black",
#     size = 20.0,
#     alpha = 1.0
#   ) +
#   labs(
#     x = "Longitude",
#     y = "Latitude",
#     caption = "\U00a9 OpenStreetMap contributors"
#   ) +
#   # geom_text(
#   #   data = adm_02_target,
#   #   aes(
#   #     x = center_x,
#   #     y = center_y,
#   #     label = NAME_2,
#   #     # adjust font size when necessary
#   #     size = 1
#   #   ),
#   #   show.legend = FALSE,
#   #   family = "Times"
# # )+
# geom_text(
#   data = adm_02_target,
#   aes(
#     x = center_x,
#     y = center_y,
#     label = NAME_2,
#     # adjust font size when necessary
#     size = 1
#   ),
#   show.legend = FALSE,
#   check_overlap = FALSE,
#   family = "sans"
# )+
#   # fix boundary box
#   coord_sf(xlim = c(105.5, 107.0),
#            ylim = c(12.0, 14.0),
#            expand = TRUE
#   ) +
#   theme_classic() +
#   theme(
#     plot.background = element_rect(fill = NA)
#   ) +
#   ggsn::scalebar(
#     x.min = 105.6,
#     x.max = 106.0,
#     y.min =12.0,
#     y.max =12.1,
#     dist = 20, 
#     dist_unit = "km",
#     st.dist = 0.3, 
#     st.size = 1, 
#     height= 0.3, 
#     transform = TRUE
#   ) +
#   # north arrow
#   ggsn::north(
#     x.min = 105.6,
#     x.max = 105.7,
#     y.min =12.05,
#     y.max = 12.15,
#     symbol = 8,
#     scale = 1
#   )

ways <- 
  road$osm_lines %>% dplyr::mutate(id = rownames(road$osm_lines)) %>% dplyr::select(id, geometry) %>% 
  dplyr::bind_rows(., streets$osm_lines %>% dplyr::mutate(id = rownames(streets$osm_lines)) %>% dplyr::select(id, geometry))

 



