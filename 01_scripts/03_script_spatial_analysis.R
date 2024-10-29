#' ---
#' title: analyzing spatial patterns in biodiversity data 
#' author: mauricio vancine
#' date: 30/10/2024
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)
library(tmap)

# options
tmap_options(check.and.fix = TRUE)
sf::sf_use_s2(FALSE)

# import ----------------------------------------------------------------

# occ
occ_cleaned <- readr::read_csv("02_results/02_occurrences_cleaned/occ_cleaned.csv")
occ_cleaned

# date filter -------------------------------------------------------------

# temporal

# fauna
occ_fauna_filter_date <- occ_fauna %>% 
    dplyr::mutate(date_filter = ifelse(year >= 1970 & year <= lubridate::year(lubridate::today()) | is.na(year), TRUE, FALSE)) %>% 
    tidyr::replace_na(list(date_filter = FALSE))
occ_fauna_filter_date

# precision ---------------------------------------------------------------

# fauna
occ_fauna_filter_date_precision <- occ_fauna_filter_date %>% 
    dplyr::mutate(precision_filter = ifelse(longitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3 &
                                                latitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3, 
                                            TRUE, FALSE)) %>% 
    dplyr::mutate(precision_filter_date = ifelse(date_filter, TRUE, ifelse(is.na(year) & precision_filter, TRUE, FALSE)))
occ_fauna_filter_date_precision

# america filter ------------------------------------------------------

# vector
am <- sf::st_read("01_data/02_variables/01_raw/01_limits/america.shp") %>% 
    dplyr::mutate(fid = 1) %>% 
    dplyr::select(fid)
am
plot(am$geometry, col = "gray")

# fauna
occ_fauna_filter_date_precision_america <- occ_fauna_filter_date_precision %>% 
    dplyr::mutate(lon = longitude, lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    sf::st_join(am) %>% 
    dplyr::mutate(america_filter = ifelse(is.na(fid), FALSE, TRUE)) %>% 
    dplyr::select(-fid) %>% 
    sf::st_drop_geometry()
occ_fauna_filter_date_precision_america

# iucn filter ------------------------------------------------------------

# import
iucn_files <- dir("01_data/01_occurrences/01_raw/02_fauna/iucn_birlife", pattern = ".shp", full.names = TRUE)
iucn_files

# filter
occ_fauna_filter_date_precision_america_iucn <- NULL
for(i in sort(unique(occ_fauna$species))){
    
    # iucn species filter
    if(!i %in% c("Troglodytes musculus", "Elaenia obscura", "Elaenia chilensis", 
                 "Callithrix jacchus", "Tityra cayana", "Eira barbara", "Nasua nasua", 
                 "Myiodynastes maculatus", "Ramphastos vitellinus")){
        
        iucn_i <- sf::st_read(grep(i, iucn_files, value = TRUE)) %>%
            dplyr::mutate(fid = 1) %>%
            dplyr::select(fid)
        
        # species filter
        occ_fauna_filter_date_precision_america_i <- occ_fauna_filter_date_precision_america %>%
            dplyr::filter(species == i)
        
        # spatial iucn filter
        occ_fauna_filter_date_precision_america_iucn_i <- occ_fauna_filter_date_precision_america_i %>%
            dplyr::mutate(lon = longitude, lat = latitude) %>%
            sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
            sf::st_join(iucn_i, quiet = TRUE) %>%
            dplyr::mutate(iucn_filter = ifelse(is.na(fid), FALSE, TRUE)) %>%
            dplyr::select(-fid) %>%
            sf::st_drop_geometry()
        
    }else{
        
        # species filter
        occ_fauna_filter_date_precision_america_i <- occ_fauna_filter_date_precision_america %>%
            dplyr::filter(species == i)
        
        # spatial iucn filter
        occ_fauna_filter_date_precision_america_iucn_i <- occ_fauna_filter_date_precision_america_i %>%
            dplyr::mutate(iucn_filter = TRUE)
        
    }
    
    # bind
    occ_fauna_filter_date_precision_america_iucn <- dplyr::bind_rows(
        occ_fauna_filter_date_precision_america_iucn,
        occ_fauna_filter_date_precision_america_iucn_i)
    
}
occ_fauna_filter_date_precision_america_iucn

# bias filter -------------------------------------------------------------

# bias
occ_fauna_filter_date_precision_america_iucn_bias <- CoordinateCleaner::clean_coordinates(
    x = occ_fauna_filter_date_precision_america_iucn, 
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
occ_fauna_filter_date_precision_america_iucn_bias

# spatial filter -----------------------------------------------------

# raster
raster_5km <- geodata::worldclim_global(var = "prec", res = 2.5, 
                                        path = "01_data/02_variables/01_raw/02_climate/") 
raster_5km

raster_5km_am <- raster_5km %>% 
    .[[1]] %>% 
    terra::crop(am) %>% 
    terra::mask(am)
raster_5km_am

plot(raster_5km_am)

raster_5km_am_id <- raster_5km_am
raster_5km_am_id[!is.na(raster_5km_am_id)] <- 1:ncell(raster_5km_am_id[!is.na(raster_5km_am_id)])
names(raster_5km_am_id) <- "id"
plot(raster_5km_am_id)

# spatial distance filter ----
occ_fauna_filter_date_precision_america_iucn_bias_spatial_values <- terra::extract(
    x = raster_5km_am_id, 
    y = occ_fauna_filter_date_precision_america_iucn_bias[, c("longitude", "latitude")], 
    ID = FALSE, cells = TRUE) %>% 
    tibble::as_tibble() %>% 
    dplyr::select(2)
occ_fauna_filter_date_precision_america_iucn_bias_spatial_values

occ_fauna_filter_date_precision_america_iucn_bias_spatial <- occ_fauna_filter_date_precision_america_iucn_bias %>%
    dplyr::mutate(spatial_cells = occ_fauna_filter_date_precision_america_iucn_bias_spatial_values) %>% 
    dplyr::distinct(species, spatial_cells, .keep_all = TRUE) %>% 
    dplyr::mutate(spatial_filter = TRUE) %>% 
    dplyr::select(c(id, spatial_filter))
occ_fauna_filter_date_precision_america_iucn_bias_spatial

occ_fauna_filter_date_precision_america_iucn_bias_spatial <- occ_fauna_filter_date_precision_america_iucn_bias %>% 
    dplyr::left_join(occ_fauna_filter_date_precision_america_iucn_bias_spatial) %>% 
    dplyr::mutate(spatial_filter = ifelse(is.na(spatial_filter), FALSE, TRUE))
occ_fauna_filter_date_precision_america_iucn_bias_spatial

# filter ------------------------------------------------------------------

# fauna
occ_fauna_cleaned <- occ_fauna_filter_date_precision_america_iucn_bias_spatial %>% 
    dplyr::filter(precision_filter_date == TRUE,
                  america_filter == TRUE,
                  iucn_filter == TRUE, 
                  .summary == TRUE,
                  spatial_filter == TRUE) %>% 
    dplyr::select(-id) %>% 
    tibble::rowid_to_column(var = "id")
occ_fauna_cleaned

# export ------------------------------------------------------------------

readr::write_csv(occ_fauna_filter_date_precision_america_iucn_bias_spatial, "01_data/01_occurrences/02_clean/occ_fauna_filters.csv")
readr::write_csv(occ_fauna_cleaned, "01_data/01_occurrences/02_clean/occ_cleaned_fauna.csv")

# end ---------------------------------------------------------------------