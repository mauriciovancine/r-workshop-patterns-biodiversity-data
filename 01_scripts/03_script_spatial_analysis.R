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

# grids -------------------------------------------------------------------




# hexagons ----------------------------------------------------------------



# kernel ------------------------------------------------------------------



# sdm ---------------------------------------------------------------------




# end ---------------------------------------------------------------------