#' ---
#' title: analyzing spatial patterns in biodiversity data 
#' author: mauricio vancine
#' date: 30/10/2024
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(jsonlite)
library(lubridate)
library(spocc)
library(rnaturalearth)
library(spData)
library(sf)
library(tmap)

# options
options(timeout = 3e5)
sf::sf_use_s2(FALSE)
tmap_options(check.and.fix = TRUE)

# create directories
dir.create(path = "02_results")
dir.create(path = "02_results/01_occurrences_raw")

# import data -------------------------------------------------------------

# import south america limite
li <- spData::world
li

tm_shape(li) +
    tm_polygons()

# species list
species_list <- c("Chrysocyon brachyurus")
species_list

# download fauna --------------------------------------------------------

# occ
for(i in species_list){
    
    # species
    print(i)
    
    ## specieslink ----
    
    # information
    print("splink")
    
    # download
    occ_splink <- jsonlite::fromJSON(
            paste0("https://specieslink.net/ws/1.0/search?scientificname=", 
                   tolower(gsub(" ", "+", i)), 
                   "&apikey=aXGEJtnQW12sPuSyKMX7&offset=0&limit=50000"))$features$properties
        
    # conditional without data
    if(length(occ_splink) == 0){
            
            occ_splink_data <- tibble::tibble(species_searched = i,
                                              species = NA,
                                              longitude = NA,
                                              latitude = NA,
                                              source = "specieslink",
                                              date = NA,
                                              key = NA)
            
        # conditional with data and year
        } else{
            
            if ("yearcollected" %in% colnames(occ_splink)) {
                occ_splink_data <- occ_splink %>% 
                    tidyr::drop_na(decimallongitude, decimallatitude) %>% 
                    dplyr::mutate(species_searched = i,
                                  species = scientificname,
                                  longitude = as.numeric(decimallongitude),
                                  latitude = as.numeric(decimallatitude),
                                  source = "specieslink",
                                  date = as.numeric(yearcollected),
                                  key = as.character(catalognumber)) %>% 
                    dplyr::select(species_searched, species, longitude, latitude, source, date, key)
                
            } else {
                occ_splink_data <- occ_splink %>% 
                    tidyr::drop_na(decimallongitude, decimallatitude) %>% 
                    dplyr::mutate(species_searched = i,
                                  species = scientificname,
                                  longitude = as.numeric(decimallongitude),
                                  latitude = as.numeric(decimallatitude),
                                  source = "specieslink",
                                  date = NA,
                                  key = as.character(catalognumber)) %>% 
                    dplyr::select(species_searched, species, longitude, latitude, source, date, key)
            }
        }

    
    # spocc ----
    # information
    print("spocc")
    
    # download
        occ_spocc <- spocc::occ(query = i, 
                                from = c("gbif", "vertnet", "idigbio", "ecoengine"),
                                has_coords = TRUE,
                                limit = 2e4,
                                throw_warnings = FALSE)
        
        # data
        occ_spocc_data <- spocc::occ2df(occ_spocc)
        
        # conditional without data
        if(nrow(occ_spocc_data) == 0){
            
            occ_spocc_data <- tibble::tibble(species_searched = i,
                                             species = NA,
                                             longitude = NA,
                                             latitude = NA,
                                             source = "spocc",
                                             date = NA,
                                             key = NA)
            
            # conditional without year  
        } else if(!"date" %in% colnames(occ_spocc_data)){
            
            occ_spocc_data <- occ_spocc_data %>% 
                dplyr::mutate(species_searched = i, .before = 1) %>% 
                dplyr::mutate(species = name, 
                              longitude = as.numeric(longitude),
                              latitude = as.numeric(latitude),
                              source = prov, 
                              date = NA,
                              key = as.character(key)) %>% 
                dplyr::select(species_searched, species, longitude, latitude, prov, date, key)
            
            # conditional with data and year
        } else{
            
            occ_spocc_data <- occ_spocc_data %>% 
                dplyr::mutate(species_searched = i, .before = 1) %>% 
                dplyr::mutate(species = name, 
                              longitude = as.numeric(longitude),
                              latitude = as.numeric(latitude),
                              source = prov, 
                              date = lubridate::year(occ_spocc_data$date),
                              key = as.character(key)) %>% 
                dplyr::select(species_searched, species, longitude, latitude, source, date, key)
            
        }
        

    # combine data ----
    occ_data <- dplyr::bind_rows(occ_splink_data, occ_spocc_data)
    
    # export 
    readr::write_csv(occ_data, 
                     paste0("02_results/01_occurrences_raw/occ_raw_splink_spocc_", sub(" ", "_", tolower(i)), ".csv"))
    
}
occ_data

# integrated --------------------------------------------------------------

# import
occ_data_fauna <- dir(path = "02_results/01_occurrences_raw/", pattern = ".csv", full.names = TRUE)%>% 
    purrr::map_dfr(readr::read_csv, col_types = "ccddcdc")
occ_data_fauna

# vector
occ_data_fauna_v <- occ_data_fauna %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    dplyr::filter(lon > -180 & lon < 180) %>% 
    dplyr::filter(lat > -90 & lat < 90) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_fauna_v

# map
tm_shape(li) +
    tm_polygons() +
    tm_shape(occ_data_fauna_v) +
    tm_bubbles(size = .2, 
               col = "species_searched", 
               col.scale = tm_scale_categorical(values = "viridis"),
               col.legend = tm_legend_hide())

# export
readr::write_csv(occ_data_fauna, "02_results/01_occurrences_raw/01_occ_raw_splink_spocc.csv")

# end ---------------------------------------------------------------------