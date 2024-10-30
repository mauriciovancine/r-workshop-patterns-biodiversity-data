#' ----
#' title: analyzing spatial patterns in biodiversity data 
#' author: mauricio vancine
#' date: 30/10/2024
#' ----

# prepare r -------------------------------------------------------------

# install packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(sf)) install.packages("sf")
if(!require(terra)) install.packages("terra")
if(!require(jsonlite)) install.packages("jsonlite")
if(!require(spocc)) install.packages("spocc")
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner")
if(!require(spData)) install.packages("spData")
if(!require(geobr)) install.packages("geobr")
if(!require(remotes)) install.packages("remotes")
if(!require(tmap)) install_github("r-tmap/tmap")
if(!require(SpatialKDE)) install.packages("SpatialKDE")
if(!require(geodata)) install.packages("geodata")
if(!require(usdm)) install.packages("usdm")
if(!require(dismo)) install.packages("dismo")
if(!require(marginaleffects)) install.packages("marginaleffects")

# end ---------------------------------------------------------------------
