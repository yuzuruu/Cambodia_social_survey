####################################################################
# Social survey in Cambodia
# Original 25th. June 2024
# Revised 
# by Yuzuru Utsunomiya, Ph. D.
# 
####################################################################
# 
# ----- read.library -----
library(tidyverse)
library(sf)
library(osmdata)
library(ggmap)
library(ggsci)
library(cmdstanr)
library(furrr)
library(future)
future::plan(multisession, workers = 8)
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
# set crs
# for making quadrat
# Using the crs, we can make the quadrat without overlapping.
wgseqproj <- "EPSG:4087"
# for general use
# After making grids, use the crs below for convenient use.
wgs84 <- "EPSG:4326"
# read building footprints with addresses in Cambodia
# NOTE
# It consumes long reading times.
# USE the completed data entitled "target_location.rds" instead.
object_Cambodia_all <- readr::read_rds("object_Cambodia_all.rds")
# make a subset focusing on Kratie province
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
  st_as_sf() %>% 
  sf::st_transform(wgseqproj) 
# Kratie
kratie_map <- 
  sf::read_sf("./KHM_adm/gadm41_KHM_1.shp") %>% 
  dplyr::filter(NAME_1 == "Krâchéh")  %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(wgseqproj)
kratie_point <- 
  object_Cambodia_all %>% 
  dplyr::filter(province_name == "Krâchéh") %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(wgseqproj) %>% 
  dplyr::tibble()
# grid data settings
# quadrat size in m (unit: metre)
cellsize <- 
  data.frame(
    cellsize = c(100, 200, 500, 1000, 2000, 5000)
  ) %>% 
  dplyr::mutate(
    index = factor(order(cellsize))
  ) %>% 
  dplyr::tibble()
# N. of sample
n <- 100
set.seed(123)
# make grid data
# NOTE
# This process needs long computation period.
# Use saved data.
# Comment out when not in use.
target_location <- 
  cellsize %>% 
  group_by(index) %>% 
  nest() %>% 
  # make grids in accordance with provided size
  dplyr::mutate(
    grid = furrr::future_map(
      data,
      ~
        sf::st_make_grid(
          kratie_map,
          cellsize = .$cellsize[1],
          square = TRUE,
          what = "polygons",
          crs = wgseqproj
        )  %>%
        sf::st_intersection(kratie_map) %>%
        sf::st_as_sf() %>%
        sf::st_transform(4326) %>% 
        dplyr::mutate(
          # First, we obtain the gravity
          area = sf::st_area(.),
          centroid = sf::st_centroid(x),
          lon = st_coordinates(centroid)[,1],
          lat = st_coordinates(centroid)[,2],
          id_grid = c(1:nrow(.))
        ) %>% 
        dplyr::select(-centroid) ,
      .options = furrr_options(seed = 123)
    )
  ) %>% 
  dplyr::mutate(
    obtain_location = furrr::future_map(
      grid,
      ~
        sf::st_intersects(
          object_Cambodia_kratie %>% 
            sf::st_transform(4326),
          .
        ) %>% 
        as.numeric() %>% 
        dplyr::tibble(location = .) ,
      .options = furrr_options(seed = 123)
    )
  ) %>% 
  dplyr::mutate(
    obtain_location_complete = furrr::future_map(
      obtain_location,
      ~
        dplyr::left_join(
          .,
          grid,
          by = c("location" = "id_grid"),
          copy = TRUE,
          keep = TRUE
        ) %>% 
        tidyr::complete(
          id_grid = tidyr::full_seq(id_grid, period = 1)
        ) %>% 
        dplyr::mutate(
          id_grid = factor(id_grid),
          area = as.numeric(area)),
      .options = furrr_options(seed = 123)
    ) 
  ) %>% 
  dplyr::mutate(
    summary = furrr::future_map(
      obtain_location_complete,
      ~
        dplyr::tibble(.) %>% 
        dplyr::group_by(.$id_grid) %>% 
        dplyr::summarise(
          n_building = sum(!is.na(area)),
          mean_area_building  = mean(area, na.rm = TRUE),
          median_area_building  = median(area, na.rm = TRUE)
        ) %>%
        # probability to be in a quadrat
        # Vital value for spatial sampling
        dplyr::mutate(
          p = n_building/sum(n_building)
        ) %>% 
        # replace NA into 0
        dplyr::mutate(
          across(contains("area"), \(x)replace_na(x,0))
        ) ,
      .options = furrr_options(seed = 123)
    )
  )
# save the results
readr::write_rds(
  target_location, 
  "target_location.rds"
)
# 
# ----- obtain.100.targets -----
for(i in 1:5){
  target_location_100 <- 
    target_location %>% 
    dplyr::mutate(
      # sample 100 quadrats in accordance with spatial distribution of the
      # building footprints and bind the results with existing point data
      sample = furrr::future_map(
        summary,
        ~
          dplyr::tibble(
            target_id = sample(
              nrow(.), 
              size = n, 
              replace = TRUE, 
              prob = .$p
            )
          ) %>% 
          dplyr::left_join(
            .,
            grid,
            by = c("target_id" = "id_grid"),
            copy = TRUE
          ) %>%
          sf::st_as_sf() %>%
          sf::st_centroid() %>% 
          sf::st_as_sf()
      )
    ) %>%
    dplyr::mutate(
      disance_duration = furrr::future_map(
        sample,
        ~
          # obtain shortes route, distance, and duration via OSRM
          # https://project-osrm.org/
          osrm::osrmTrip(
            loc = data.frame(lon = .$lon, lat = .$lat),
            # by car
            osrm.profile = "car"
          ) %>% 
          # pick the minimum moving distance and duration
          dplyr::tibble(
            distance = .[[1]]$summary$distance,
            duration = .[[1]]$summary$duration
          ),
        .options = furrr_options(seed = 123)
      )
    )
  distance_duration <- 
    dplyr::bind_rows(target_location_100$disance_duration) %>%
    dplyr::select(2:3) %>%
    dplyr::mutate(
      index = rep(cellsize$index, times = 1)
    )
  readr::write_excel_csv(
    distance_duration, 
    paste0("./distance_duration/distance_duation_",i,".csv")
  )
}

# st_geometry(hoge$sample[[1]]) <- "geometry"
# st_geometry(hoge$sample[[2]]) <- "geometry"
# st_geometry(hoge$sample[[3]]) <- "geometry"

