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
# library(ggsn)
library(cmdstanr)
library(furrr)
library(future)
future::plan(multisession, workers = 16)

# 
# ----- read.data -----
# shapefiles (province)
kratie_map_province_01 <- 
  sf::read_sf("./KHM_adm/gadm41_KHM_1.shp") %>% 
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
  sf::read_sf("./KHM_adm/gadm41_KHM_2.shp") %>% 
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
  sf::read_sf("./KHM_adm/gadm41_KHM_3.shp") %>% 
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
  sf::read_sf("./KHM_adm/gadm41_KHM_1.shp") %>% 
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

# # ----- stan.area -----
# # Classify the distribution of the area using latent class analysis
# # References
# # https://qiita.com/ando_roid/items/8e7142f8c87e5e0f44b0
# # NOTE
# # This process needs long computation period.
# # Save the results and comment out when not in use.
# # 1. area
# model <- cmdstanr::cmdstan_model("latent_classification.stan")
# dataset_02 <- 
#   list(
#     K = 2, 
#     y = log(object_Cambodia_kratie$area),
#     N = nrow(object_Cambodia_kratie
#              )
#     )
# # fit the model with stan
# fit_02_area <- 
#   model$sample(
#     data = dataset_02,
#     seed = 123,
#     chains = 4,
#     parallel_chains = 4,
#     refresh = 100
#     )
# fit_02_area$save_object(file = "fit_02_area.rds")
# rm(fit_02_area)
# gc()
# 
# dataset_03 <- 
#   list(
#     K = 3, 
#     y = log(object_Cambodia_kratie$area),
#     N = nrow(object_Cambodia_kratie
#     )
#   )
# # fit the model with stan
# fit_03_area <- 
#   model$sample(
#     data = dataset_03,
#     seed = 123,
#     chains = 4,
#     parallel_chains = 4,
#     refresh = 100
#   )
# fit_03_area$save_object(file = "fit_03_area.rds")
# rm(fit_03_area)
# gc()
# # 2. distance from road
# model <- cmdstanr::cmdstan_model("latent_classification.stan")
# # assumed N. of distribution: 2
# dataset_02 <- 
#   list(
#     K = 2, 
#     y = log(as.numeric(object_Cambodia_kratie$distance)),
#     N = nrow(object_Cambodia_kratie
#     )
#   )
# # fit the model with stan
# fit_02_distance <- 
#   model$sample(
#     data = dataset_02,
#     seed = 123,
#     chains = 4,
#     parallel_chains = 4,
#     refresh = 100
#   )
# fit_02_distance$save_object(file = "fit_02_distance.rds")
# rm(fit_02_distance)
# gc()
# # assumed N. of distribution: 3
# dataset_03 <- 
#   list(
#     K = 3, 
#     y = log(as.numeric(object_Cambodia_kratie$distance)),
#     N = nrow(object_Cambodia_kratie
#     )
#   )
# # fit the model with stan
# fit_03_distance <- 
#   model$sample(
#     data = dataset_03,
#     seed = 123,
#     chains = 4,
#     parallel_chains = 4,
#     refresh = 100
#   )
# fit_03_distance$save_object(file = "fit_03_distance.rds")
# rm(fit_03_distance)
# gc()
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

# ----- grid.map -----
wgseqproj <- "EPSG:4087"
kratie_map <- 
  sf::read_sf("./KHM_adm/gadm41_KHM_1.shp") %>% 
  dplyr::filter(NAME_1 == "Krâchéh")  %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(wgseqproj)
kratie_point <- 
  object_Cambodia_all %>% 
  dplyr::filter(province_name == "Krâchéh") %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(wgseqproj)






kratie_grid <-
  kratie_map %>%
  st_make_grid(
    .,
    cellsize = 100,
    square = TRUE,
    what = "polygons",
    crs = wgseqproj
    )  %>%
  st_intersection(kratie_map) %>%
  st_as_sf() 


# kratie_grid_map <- 
#   ggmap::ggmap(kratie) +
#   # geom_sf() +
#   # district-level administrative boundaries of Kratie province
#   geom_sf(
#     data = kratie_map_province_01,
#     inherit.aes = FALSE,
#     color = NA,
#     fill = "white",
#     size = 0.1,
#     linetype = "dotted",
#     alpha = 0.25
#   ) +
#   geom_sf(
#   data = kratie_grid,
#   inherit.aes = FALSE,
#   fill = NA,
#   color = "white",
#   size = 20.0,
#   alpha = 1.0
# ) +
#   geom_sf(
#     data = kratie_point,
#     inherit.aes = FALSE,
#     fill = NA,
#     color = "yellow",
#     size = 1.0,
#     alpha = 1.0
#   ) +
#   theme_classic()
# 
# 
# ggsave(
#   "kratie_grid_map.pdf",
#   plot = kratie_grid_map,
#   width = 600,
#   height = 600,
#   units = "mm"
# )



wgs84 <- "EPSG:4326"
st_geometry(kratie_grid) <- "geometry"
kratie_grid_4326 <- 
  kratie_grid %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(wgs84) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    area = sf::st_area(geometry),
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    lon = st_coordinates(centroid)[,1],
    # y
    lat = st_coordinates(centroid)[,2],
    id_grid = c(1:nrow(.))
  ) %>% 
  dplyr::select(-centroid) 

# kratie_grid_number_map <-
#   ggplot(kratie_grid_4326) +
#   geom_sf() +
#   geom_sf_text(
#     aes(label = id),
#     size = 2
#     )

object_Cambodia_kratie <- 
  object_Cambodia_all %>% 
  dplyr::filter(province_name == "Krâchéh") %>% 
  dplyr::mutate(
    lon = st_coordinates(centroid)[,1],
    # y
    lat = st_coordinates(centroid)[,2],
    id_kratie = c(1:nrow(.))
  ) %>% 
  dplyr::select(-geometry) %>% 
  st_as_sf()



###############################################

object_Cambodia_sample <-
  object_Cambodia_kratie %>%
  dplyr::select(-geometry) %>% 
  sf::st_drop_geometry()
# load shapefiles to obtain administrative information

object_grid_index <- st_intersects(object_Cambodia_kratie, kratie_grid_4326, sparse = T) %>%  as.numeric()

###############################################


kratie_grid_4326



object_Cambodia_sample <-
  kratie_grid_4326 %>%
  sf::st_drop_geometry()
# load shapefiles to obtain administrative information
shp_Cambodia <- 
  sf::st_read(
    "./KHM_adm/gadm41_KHM_3.shp", 
    options = "ENCODING=UTF-8", 
    stringsAsFactors=FALSE
  ) %>% 
  dplyr::mutate_if(
    is.character, 
    enc2utf8
  ) %>% 
  dplyr::filter(NAME_1 = "Krâchéh")
# function to detect addresses
find_city <- 
  function(sp_polygon = df, lon = lon, lat = lat){
    # find a polygon containing a certain pair of lon / lat
    which.row <- 
      sf::st_contains(
        sp_polygon, 
        sf::st_point(
          c(
            lon, 
            lat
          )
        ), 
        sparse = FALSE
      ) %>%  
      grep(TRUE, .)
    # If not, leave a warning message
    # -> deliver values ("hoge") to avoid malfunction.
    # We will remove NA by the values later. 
    if (identical(which.row, integer(0)) == TRUE) {
      # Original code provides the following message.
      # message("指定した座標がポリゴンに含まれません")
      geos <- 
        data.frame(
          ID_1 = "hoge", 
          NAME_1 = "hoge",
          ID_2 = "hoge", 
          NAME_2 = "hoge",
          ID_3 = "hoge", 
          NAME_3 = "hoge"
        )
    }
    # If exist, obtain information of coordinates
    else {
      geos <- 
        sp_polygon[which.row, ] %>%
        # transform from factor to character
        dplyr::mutate_if(
          is.factor, 
          as.character
        ) %>% 
        # obtain necessary part of the shapefile
        dplyr::mutate_at(
          # dplyr::vars(NAME_1, NAME_2, NAME_3), 
          dplyr::vars(NAME_1), 
          dplyr::funs(
            dplyr::if_else(
              # Is it NA?
              condition = is.na(.),
              # if NA, return blank
              true = "", 
              # if not, use it
              false = .
            )
          )
        )
      # make a dataset of administrative boundaries
      # Names and IDs are obtained from shapefiles
      res <- 
        tibble::data_frame(
          province_code = geos$ID_1,
          district_code = geos$ID_2,
          subdistrict_code = geos$ID_3,
          province_name = geos$NAME_1,
          district_name = geos$NAME_2,
          subdistrict_name = geos$NAME_3
        )
      # for inspecting function movement
      # print(res)
      # return results
      return(res)
    }
  }
set.seed(123)
# split the building footprint data into manageable units
# n = 500 per group
object_Cambodia_sample_group <- 
  object_Cambodia_sample %>% 
  dplyr::mutate(
    group = rep(paste0("group",sample(c(1:500000), replace = FALSE)),each = 500)[c(1:nrow(.))]
  ) 
# N. of group = 7654 in total
group_level <-
  levels(
    factor(
      object_Cambodia_sample_group$group
    )
  )
# obtain address by group and object individually
object_Cambodia_sample_group_address <-
  for(i in 1:length(group_level)){
    target <- filter(
      object_Cambodia_sample_group,
      group == group_level[i]
    )
    target_address <-
      target %>%
      dplyr::mutate(
        area_info = furrr::future_map2_dfr(
          .x = lon,
          .y = lat,
          ~ try(
            find_city(
              sp_polygon = shp_Cambodia,
              lon = .x,
              lat = .y
            )
          )
        )
      ) %>%
      tibble()
    # save the computation results
    write_excel_csv(
      # fix target column
      bind_cols(
        id = target_address$id,
        province_code = target_address$area_info$province_code,
        province_name = target_address$area_info$province_name,
        district_code = target_address$area_info$district_code,
        district_name = target_address$area_info$district_name,
        subdistrict_code = target_address$area_info$subdistrict_code,
        subdistrict_name = target_address$area_info$subdistrict_name
      ) %>%
        # remove rows containing NA
        na.omit(),
      file = paste0("target_address_Cambodia/",target_address$group[1], ".csv")
    )
    # for larger data, enable the gc() function
    gc()
    gc()
  }






