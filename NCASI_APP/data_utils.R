# `Project Climate` Data Processing Functions --------
# UDFS: Maxwell Vanlandschoot and Julian Schmitt -----
# Summer 2022 ----------------------------------------


# load data files ------------------------------------
# load metadata crucial to file pathing and tree data
meta <- readRDS(paste(rootdir, "data/RCP26/meta.RData", sep=""))
tree_data <- fread(paste(rootdir, "data/leaflet_dat/tree_dat.csv", sep=""))
species_codes <- fread(paste(rootdir, "data/leaflet_dat/species_codes.csv", sep="")) %>% select(c(COMMON_NAME, SPECIES_CODE))
hist_clim_x_tree_dat <- data.table(read.table(paste(rootdir, "data/historical_climate_tree_data_v3.csv", sep = ""), 
                                              sep=",", header=TRUE))
# rename to match columns of other datasets
hist_clim_x_tree_dat <- hist_clim_x_tree_dat %>% dplyr::rename(mean_temp = meantmp, 
                                                        min_temp = mintmp,
                                                        max_temp = maxtmp) 

# process data @MAXWELL - explain why were doing this
RCP_60_trim <- fread(paste(rootdir, "data/leaflet_dat/RCP_60_trim.csv", sep="")) %>%
  mutate(lat1 = LAT - 0.0625,
         lon1 = LON + 0.0625,
         lat2 = LAT + 0.0625,
         lon2 = LON - 0.0625) %>%
  group_by(YR) %>%
  mutate(yr_meantmp = mean(meantmp),
         yr_mintmp = mean(yr_mintmp),
         yr_maxtmp = mean(yr_maxtmp),
         yr_precip = mean(yr_precip),
         yr_rng = mean(yr_rng)) %>%
  ungroup()  

# polygon data for basemap 
multipoly <- RCP_60_trim %>% dplyr::select(LON, LAT) %>% dplyr::distinct()
multipoly <- SpatialPointsDataFrame(multipoly[,c("LON","LAT")],multipoly)


# (New) Functions to load data -----------------------
load_cmip5_from_coord <- function(lat, lon, rcp) {
  #' Loads CMIP5 File from lat/lon pair for specific RCP
  
  # find closest datapoint
  pick_lat <- meta$LAT[which.min(abs(meta$LAT-lat))]
  pick_lon <- meta$LON[which.min(abs(meta$LON-lon))]
  
  # get filepath for location
  fpath <- paste(rootdir, "data/RCP", rcp, "/LAT_", pick_lat, "_LON_", pick_lon, "_.csv", sep="")
  
  # load and return datafile
  cmip5_dat <- tryCatch(fread(fpath, sep=",", header=TRUE),
                error = function(e)
                  print("No CMIP5 Climate data available for that location"))
  
  # add column with RCP scenario
  cmip5_dat$RCP <- rcp
  
  return(data.table(cmip5_dat))
}

# function to load all RCPs for all locations --------
load_cmip5_from_df_all_rcp <- function(coord_df, rcp_pathways = c(26, 45, 60, 85)) {
  #' Loads CMIP5 data for all selected points and RCPs 
  
  # Load dataframes one-by-one into the list
  dat_list <- list()
  
  # loop through selected points
  for (i in 1:dim(coord_df)[1]) {
    for (j in 1:length(rcp_pathways)) {
      # append file
      index <- (i-1)*(length(rcp_pathways))+j
      dat_list[[index]] <- load_cmip5_from_coord(lat = coord_df[i, ]$LAT, 
                                               lon = coord_df[i, ]$LON, 
                                               rcp = rcp_pathways[j])
    }
  }
  # join data sets and aggregate
  dat_joined <- rbindlist(dat_list) %>% 
    # monthly averages - @MAXWELL, does this summarize actually change anything except renaming?
    dplyr::rename(mean_temp = meantmp,
              min_temp = mintmp,
              max_temp = maxtmp,
              precip = precip) %>%
    # add yearly averages by scenario
    group_by(YR, RCP) %>%
    mutate(yr_mean_temp = mean(mean_temp, na.rm = T),
           yr_min_temp = mean(min_temp, na.rm = T),
           yr_max_temp = mean(max_temp, na.rm = T),
           yr_precip = mean(precip, na.rm = T))
  
}




# Functions to use loaded data -----------------------
t_series_CMIP5 <- function(scen, var, lat, lon, l_quantile = 0.05, h_quantile=0.95) {
  # load data
  lon <- 360 + lon
  df <- load_data(lat, lon, scen)
  
  # select location and aggregate by year, convert to deg C
  t_series <- df  %>% 
    group_by(year, MODEL) %>% 
    summarise(an_mean = mean( {{ var }} ))
  
  
  temp_series <- t_series %>% ungroup() %>% group_by(year) %>% 
    summarize("quantile" = list(round(quantile(an_mean, c(l_quantile, h_quantile)), 2)), 
              "ens_mean" = round(mean(an_mean), 2)) %>% 
    unnest_wider(quantile) %>% 
    dplyr::rename(LB = "5%", UB = "95%")
  
  return(temp_series)
}
  