#' ---
#' title: analyzing spatial patterns in biodiversity data 
#' author: mauricio vancine
#' date: 30/10/2024
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(geobr)
library(sf)
library(terra)
library(SpatialKDE)
library(geodata)
library(usdm)
library(dismo)
library(marginaleffects)
library(tmap)

# options
sf::sf_use_s2(FALSE)
tmap_options(check.and.fix = TRUE)

# import ----------------------------------------------------------------

# occ
occ <- readr::read_csv("02_results/02_occurrences_cleaned/occ_cleaned.csv")
occ

occ_v <- occ %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_v

# brazil
brazil <- geobr::read_country()
brazil

# map
tm_shape(brazil) +
    tm_polygons() +
    tm_shape(occ_v) +
    tm_dots() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# sao paulo state
sp_state <- geobr::read_state(code_state = 35, year = 2020) %>% 
    sf::st_transform(4326)
sp_state

tm_shape(sp_state) +
    tm_polygons() +
    tm_shape(occ_v) +
    tm_dots() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# spatial filter ----------------------------------------------------------
occ_v_sp <- occ_v[sp_state,]
occ_v_sp

tm_shape(sp_state) +
    tm_polygons() +
    tm_shape(occ_v_sp) +
    tm_dots() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# projection -------------------------------------------------------------

# projection
occ_v_sp_proj <- sf::st_transform(occ_v_sp, crs = 5880) # SIRGAS2000/Brazil Polyconic - https://epsg.io/5880
occ_v_sp_proj

sp_state_proj <- sf::st_transform(sp_state, crs = 5880)
sp_state_proj

tm_shape(sp_state_proj) +
    tm_polygons() +
    tm_shape(occ_v_sp_proj) +
    tm_dots() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# grids -------------------------------------------------------------------

# create
grid <- sf::st_make_grid(x = sp_state_proj, cellsize = 50000) %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(areakm2 = sf::st_area(.)/1e4) %>% 
    tibble::rowid_to_column("id_grid")
grid

tm_shape(sp_state_proj, bbox = grid) +
    tm_polygons() +
    tm_shape(grid) +
    tm_borders() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# spatial select
grid_sel <- grid[sp_state_proj,]
grid_sel

tm_shape(sp_state_proj, bbox = grid_sel) +
    tm_polygons() +
    tm_shape(grid_sel) +
    tm_borders() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))
    
tm_shape(sp_state_proj, bbox = grid_sel) +
    tm_polygons() +
    tm_shape(grid_sel) +
    tm_borders() +
    tm_shape(occ_v_sp_proj) +
    tm_dots() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90)) 

# count points by grid
grid_sel_join <- sf::st_join(grid_sel, occ_v_sp_proj, left = FALSE)
grid_sel_join

grid_sel_occ_count <- grid_sel_join %>% 
    dplyr::group_by(id_grid) %>% 
    dplyr::summarise(n_occ = n())
grid_sel_occ_count

map_grid <- tm_shape(grid_sel) +
    tm_polygons() +
    tm_shape(grid_sel_occ_count) +
    tm_polygons(fill = "n_occ",
                fill.scale = tm_scale_continuous(values = "-spectral"),
                fill_alpha = .7,
                fill.legend = tm_legend(title = "Number of \n occurrences", 
                                        position = tm_pos_in("left", "bottom"),
                                        reverse = TRUE)) +
    tm_text("n_occ", col = "black", shadow = TRUE) +
    tm_shape(sp_state_proj) +
    tm_borders(col = "black", lwd = 2) +
    tm_shape(occ_v_sp_proj) +
    tm_dots() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))  +
    tm_compass(size = 3, position = c("right", "top")) +
    tm_scalebar(text.size = .8)
map_grid

# export
tmap::tmap_save(map_grid, "02_results/03_spatial_analysis/map_grid.png", 
                width = 25, height = 20, units = "cm", dpi = 300)

# hexagons ----------------------------------------------------------------

# create
hex <- sf::st_make_grid(x = sp_state_proj, cellsize = 50000, square = FALSE) %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(areakm2 = sf::st_area(.)/1e4) %>% 
    tibble::rowid_to_column("id_grid")
hex

tm_shape(sp_state_proj, bbox = grid) +
    tm_polygons() +
    tm_shape(hex) +
    tm_borders() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# spatial select
hex_sel <- hex[sp_state_proj,]
hex_sel

tm_shape(sp_state_proj, bbox = hex_sel) +
    tm_polygons() +
    tm_shape(hex_sel) +
    tm_borders() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

tm_shape(sp_state_proj, bbox = hex_sel) +
    tm_polygons() +
    tm_shape(hex_sel) +
    tm_borders() +
    tm_shape(occ_v_sp_proj) +
    tm_dots() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# count points by grid
hex_sel_join <- sf::st_join(hex_sel, occ_v_sp_proj, left = FALSE)
hex_sel_join

hex_sel_occ_count <- hex_sel_join %>% 
    dplyr::group_by(id_grid) %>% 
    dplyr::summarise(n_occ = n())
hex_sel_occ_count

map_hex <- tm_shape(hex_sel) +
    tm_polygons() +
    tm_shape(hex_sel_occ_count) +
    tm_polygons(fill = "n_occ",
                fill.scale = tm_scale_continuous(values = "-spectral"),
                fill_alpha = .7,
                fill.legend = tm_legend(title = "Number of \n occurrences", 
                                        position = tm_pos_in("left", "bottom"),
                                        reverse = TRUE)) +
    tm_text("n_occ", col = "black", shadow = TRUE) +
    tm_shape(sp_state_proj) +
    tm_borders(col = "black", lwd = 2) +
    tm_shape(occ_v_sp_proj) +
    tm_dots() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90)) +
    tm_compass(size = 3, position = c("right", "top")) +
    tm_scalebar(text.size = .8)
map_hex

# export
tmap::tmap_save(map_hex, "02_results/03_spatial_analysis/map_hex.png", 
                width = 25, height = 20, units = "cm", dpi = 300)

# convex hull -----------------------------------------------------------

# create
convex_hull <- occ_v_sp_proj %>% 
    sf::st_union() %>% 
    sf::st_convex_hull() %>% 
    sf::st_as_sf()
convex_hull

# map
map_convex_hull <- tm_shape(sp_state_proj) +
    tm_polygons(col = "black", lwd = 2) +
    tm_shape(convex_hull, bbox = sp_state_proj) +
    tm_fill(col = "red", alpha = ) +
    tm_borders(col = "red", lwd = 3) +
    tm_shape(occ_v_sp_proj) +
    tm_bubbles(fill = "black", size = .5) +
    tm_grid(lines = FALSE, labels.rot = c(0, 90)) +
    tm_compass(size = 3, position = c("right", "top")) +
    tm_scalebar(text.size = .8)
map_convex_hull

# export
tmap::tmap_save(map_convex_hull, "02_results/03_spatial_analysis/map_convex_hull.png", 
                width = 25, height = 20, units = "cm", dpi = 300)

# kernel ------------------------------------------------------------------

# raster
sp_state_proj_raster <- SpatialKDE::create_raster(
    geometry = sp_state_proj,
    cell_size = 5000)
sp_state_proj_raster

# kernel density estimation - kde
# functions: https://en.wikipedia.org/wiki/Kernel_(statistics)#Kernel_functions_in_common_use
kde <- SpatialKDE::kde(
    points = occ_v_sp_proj,
    band_width = 50000,  
    kernel = "quartic", # "quartic", "uniform", "triweight", "epanechnikov", "triangular"
    grid = sp_state_proj_raster) %>% 
    terra::rast() %>% 
    terra::crop(sp_state_proj, mask = TRUE)
kde

tm_shape(kde) +
    tm_raster(col.legend = tm_legend_hide())

# map
map_kernel <- tm_shape(kde, bbox = hex_sel) +
    tm_raster(col = "layer",
              col.scale = tm_scale_continuous(values = "-spectral"),
              col.legend = tm_legend(title = "Kernel density", 
                                     position = tm_pos_in("left", "bottom"),
                                     reverse = TRUE)) +
    tm_shape(sp_state_proj) +
    tm_borders(col = "black", lwd = 2) +
    tm_shape(occ_v_sp_proj) + 
    tm_bubbles(col = "black", fill = NA, size = .3) +
    tm_grid(lines = FALSE, labels.rot = c(0, 90)) +
    tm_compass(size = 3, position = c("right", "top")) +  
    tm_scalebar(text.size = .8)
map_kernel

# export
tmap_save(map_kernel, "02_results/03_spatial_analysis/map_kernel.png",
          width = 25, height = 20, units = "cm", dpi = 300)

# sdm ---------------------------------------------------------------------

# download variables
var <- geodata::worldclim_global(var = "bio", res = 10, path = "02_results/03_spatial_analysis/")
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
var

tm_shape(var[[1]]) +
    tm_raster(col.legend = tm_legend_hide())

# adjust
var_sp <- terra::crop(var, sp_state, mask = TRUE)
var_sp

tm_shape(var_sp[[1]]) +
    tm_raster(col.legend = tm_legend_hide())

# correlation
var_sp_vif <- usdm::vifstep(var_sp, th = 2)
var_sp_vif

var_sp_sel <- usdm::exclude(var_sp, var_sp_vif)
var_sp_sel

tm_shape(var_sp_sel) +
    tm_raster(col.legend = tm_legend_hide())

# pseudo-absence
pseud <- terra::spatSample(terra::vect(sp_state), nrow(occ_v))
pseud

tm_shape(sp_state) +
    tm_polygons() +
    tm_shape(pseud) +
    tm_dots(fill = "red") +
    tm_shape(occ_v_sp) +
    tm_dots()

# prepare data
sdm_data <- dismo::prepareData(x = var_sp_sel,
                           p = occ_v_sp[, 4:5],
                           b = as.data.frame(geom(pseud)[, 3:4]))
head(sdm_data)

# fit model
sdm_glm_model <- glm(pb ~ bio05 + bio07 + bio18 + bio19, data = sdm_data[, -2], family = binomial)
sdm_glm_model
summary(sdm_glm_model)

# plot models
marginaleffects::plot_predictions(sdm_glm_model, condition = "bio05") + theme_classic() # Max Temperature of Warmest Month 
marginaleffects::plot_predictions(sdm_glm_model, condition = "bio07") + theme_classic() # Temperature Annual Range (BIO5-BIO6)
marginaleffects::plot_predictions(sdm_glm_model, condition = "bio18") + theme_classic() # Precipitation of Warmest Quarter
marginaleffects::plot_predictions(sdm_glm_model, condition = "bio19") + theme_classic() # Precipitation of Coldest Quarter

# predict
sdm_pred <- terra::predict(var_sp_sel, sdm_glm_model, type = "response")
sdm_pred

# threshold
sdm_pred_thr <- sdm_pred >= 0.1

# maps
map_sdm <- tm_shape(sdm_pred, bbox = hex_sel) +
    tm_raster(col = "lyr1",
              col.scale = tm_scale_continuous(values = "-spectral"),
              col.legend = tm_legend(title = "Suitability", 
                                     position = tm_pos_in("left", "bottom"),
                                     reverse = TRUE)) +
    tm_shape(sp_state_proj) +
    tm_borders(col = "black", lwd = 2) +
    tm_shape(occ_v_sp_proj) + 
    tm_bubbles(col = "black", fill = NA, size = .3) +
    tm_grid(lines = FALSE, labels.rot = c(0, 90)) +
    tm_compass(size = 3, position = c("right", "top")) +  
    tm_scalebar(text.size = .8)
map_sdm

map_sdm_thr <- tm_shape(sdm_pred_thr, bbox = hex_sel) +
    tm_raster(col = "lyr1",
              col.scale = tm_scale_categorical(values = c("gray", "darkred")),
              col.legend = tm_legend(title = "Suitability", 
                                     position = tm_pos_in("left", "bottom"),
                                     reverse = TRUE)) +
    tm_shape(sp_state_proj) +
    tm_borders(col = "gray30", lwd = 1) +
    tm_shape(occ_v_sp_proj) + 
    tm_dots(size = .3) +
    tm_grid(lines = FALSE, labels.rot = c(0, 90)) +
    tm_compass(size = 3, position = c("right", "top")) +  
    tm_scalebar(text.size = .8)
map_sdm_thr

# export
tmap_save(map_sdm, "02_results/03_spatial_analysis/map_sdm.png",
          width = 25, height = 20, units = "cm", dpi = 300)

tmap_save(map_sdm_thr, "02_results/03_spatial_analysis/map_sdm_thr.png",
          width = 25, height = 20, units = "cm", dpi = 300)

# end ---------------------------------------------------------------------