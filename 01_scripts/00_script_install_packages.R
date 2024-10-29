#' ----
#' title: analyzing spatial patterns in biodiversity data 
#' author: mauricio vancine
#' date: 30/10/2024
#' ----

# prepare r -------------------------------------------------------------

# packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(sf)) install.packages("sf")
if(!require(terra)) install.packages("terra")
if(!require(spocc)) install.packages("spocc")
if(!require(jsonlite)) install.packages("jsonlite")
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner")
if(!require(geobr)) install.packages("geobr")
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(remotes)) install.packages("remotes")
if(!require(tmap)) install_github("r-tmap/tmap")
if(!require(dismo)) install.packages("dismo")

# end ---------------------------------------------------------------------
