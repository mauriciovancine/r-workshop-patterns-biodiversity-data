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
occ <- readr::read_csv("02_results/02_occurrences_cleaned/occ_cleaned.csv")
occ

occ_v <- 


# sao paulo state
sp_state <- geobr::read_state(code_state = 35, year = 2020)
sp_state

tm_shape(sp_state) +
    tm_polygons() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# projected
sp_state_proj <- sf::st_transform(sp_state, crs = 5880)
sp_state_proj

tm_shape(sp_state_proj) +
    tm_polygons() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90))

# grids -------------------------------------------------------------------

# grid

# create
grid <- sf::st_make_grid(x = sp_state_proj, cellsize = 50000)
grid

tm_shape(sp_state_proj, bbox = grid) +
    tm_polygons() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90)) +
    tm_shape(grid) +
    tm_borders()

# spatial select
grid_sel <- grid[sp_state_proj,]
grid_sel

tm_shape(sp_state_proj, bbox = grid_sel) +
    tm_polygons() +
    tm_grid(lines = FALSE, labels.rot = c(0, 90)) +
    tm_shape(grid_sel) +
    tm_borders()

# hexagons ----------------------------------------------------------------




# kernel ------------------------------------------------------------------




# sdm ---------------------------------------------------------------------




# end ---------------------------------------------------------------------