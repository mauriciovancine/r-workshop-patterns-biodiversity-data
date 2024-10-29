#' ---
#' title: analyzing spatial patterns in biodiversity data 
#' author: mauricio vancine
#' date: 30/10/2024
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(CoordinateCleaner)
library(rnaturalearth)
library(spData)
library(sf)
library(terra)
library(tmap)

# options
tmap_options(check.and.fix = TRUE)
sf::sf_use_s2(FALSE)

# import ----------------------------------------------------------------

# occurrences
occ <- readr::read_csv("02_results/01_occurrences_raw/01_occ_raw_splink_spocc.csv") %>% 
    tibble::rowid_to_column(var = "id")
occ

# information
nrow(occ)

# date filter -------------------------------------------------------------

# temporal

# fauna
occ_filter_temporal <- occ %>% 
    dplyr::mutate(temporal_filter = ifelse(date >= 1970 & date <= lubridate::year(lubridate::today()), TRUE, FALSE)) %>% 
    tidyr::replace_na(list(temporal_filter = FALSE))
occ_filter_temporal

# precision ---------------------------------------------------------------

# fauna
occ_filter_temporal_precision <- occ_filter_temporal %>% 
    dplyr::mutate(precision_filter = ifelse(longitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3 &
                                                latitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3, 
                                            TRUE, FALSE))
occ_filter_temporal_precision

# bias filter -------------------------------------------------------------

# bias
occ_filter_temporal_precision_bias <- CoordinateCleaner::clean_coordinates(
    x = occ_filter_temporal_precision, 
    species = "species",
    lon = "longitude", 
    lat = "latitude",
    tests = c("capitals", # radius around capitals
              "centroids", # radius around country and province centroids
              "duplicates", # records from one species with identical coordinates
              "equal", # equal coordinates
              "gbif", # radius around GBIF headquarters
              "institutions", # radius around biodiversity institutions
              # "outliers", # remove outliers
              "seas", # in the sea
              "urban", # within urban area
              "validity", # outside reference coordinate system
              "zeros" # plain zeros and lat = lon
    ),
    capitals_rad = 2000,
    centroids_rad = 2000,
    centroids_detail = "both",
    inst_rad = 100,
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 1000,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid") %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(.cen = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .cen),
                  .summary = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .summary))
occ_filter_temporal_precision_bias

# filter ------------------------------------------------------------------

# fauna
occ_cleaned <- occ_filter_temporal_precision_bias %>% 
    dplyr::filter(temporal_filter == TRUE,
                  precision_filter == TRUE,
                  .summary == TRUE) %>% 
    dplyr::select(-id) %>% 
    tibble::rowid_to_column(var = "id")
occ_cleaned

# maps --------------------------------------------------------------------

# vector
occ_filter_temporal_precision_bias_v <- occ_filter_temporal_precision_bias %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_filter_temporal_precision_bias_v

occ_cleaned_v <- occ_cleaned %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_cleaned_v

# map
tm_shape(li, bbox = occ_filter_temporal_precision_bias_v) +
    tm_polygons() +
    tm_shape(occ_filter_temporal_precision_bias_v) +
    tm_bubbles(size = .4, fill = "red") +
    tm_shape(occ_cleaned_v) +
    tm_bubbles(size = .4, fill = "forestgreen")

# manual filter
occ_cleaned <- occ_cleaned %>% 
    dplyr::filter(latitude < 0)
occ_cleaned

occ_cleaned_v <- occ_cleaned %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_cleaned_v

# map
tm_shape(li, bbox = occ_filter_temporal_precision_bias_v) +
    tm_polygons() +
    tm_shape(occ_filter_temporal_precision_bias_v) +
    tm_bubbles(size = .4, fill = "red") +
    tm_shape(occ_cleaned_v) +
    tm_bubbles(size = .4, fill = "forestgreen")

# export ------------------------------------------------------------------

# export
readr::write_csv(occ_filter_temporal_precision_bias, "02_results/02_occurrences_cleaned/occ_filters.csv")
readr::write_csv(occ_cleaned, "02_results/02_occurrences_cleaned/occ_cleaned.csv")

# end ---------------------------------------------------------------------