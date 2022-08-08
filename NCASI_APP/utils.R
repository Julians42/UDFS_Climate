# `Project Climate` Packages and Source Files --------
# UDFS: Maxwell Vanlandschoot and Julian Schmitt -----
# Summer 2022 ----------------------------------------

# packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(leaflet)
library(plotly)
library(sp)
library(sf)
library(leaflet.extras)
library(gt)
library(data.table)  
library(here)

# set root directory
rootdir <- paste(here(), "/", sep ="")
# rootdir <- "/Users/julianschmitt/Documents/Research/FIA_22/climate/"
#rootdir <- "/Users/maxwell/climate/"

# Data loading functions
source(paste(rootdir, "NCASI_APP/data_utils.R", sep = ""))

# Plotting functions
source(paste(rootdir, "NCASI_APP/plot_utils.R", sep = ""))
  
