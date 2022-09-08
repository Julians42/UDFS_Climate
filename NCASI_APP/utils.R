# `Project Climate` Packages and Source Files --------
# UDFS: Maxwell Vanlandschoot and Julian Schmitt -----
# Summer 2022 ----------------------------------------

# packages
package_list <- c("shiny",
                  "shinydashboard",
                  "shinycssloaders",
                  "tidyverse",
                  "leaflet",
                  "plotly",
                  "sp",
                  "leaflet.extras",
                  "gt",
                  "data.table",
                  "here")

# install any packages not already installed
uninstalled_packages <- package_list[!(package_list %in% installed.packages()[, "Package"])]
if(length(uninstalled_packages)) install.packages(uninstalled_packages)

# load packages 
lapply(package_list, require, character.only = TRUE)

# library(shiny)
# library(shinydashboard)
# library(shinycssloaders)
# library(tidyverse)
# library(leaflet)
# library(plotly)
# library(sp)
# library(sf)
# library(leaflet.extras)
# library(gt)
# library(data.table)  
# library(here)

# set root directory
rootdir <- paste(here(), "/", sep ="")

# Data loading functions
source(paste(rootdir, "NCASI_APP/data_utils.R", sep = ""))

# Plotting functions
source(paste(rootdir, "NCASI_APP/plot_utils.R", sep = ""))
  
