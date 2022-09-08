# `Project Climate` Data Processing Functions --------
# UDFS: Maxwell Vanlandschoot and Julian Schmitt -----
# Summer 2022 ----------------------------------------

# load data files ------------------------------------
# climate data aggregated to the yearly level with uncertainty
clim_dat <- fread(paste(rootdir, "data/yearly_ci_all.csv", sep = ""))

# climate data aggregated to the monthly level without uncertainty
clim_mon <- fread(paste(rootdir, "data/clim_dat_monthly.csv", sep = ""))

# species codes and common names
species_codes <- fread(paste(rootdir, "data/species_codes.csv", sep = "")) %>% select(c(COMMON_NAME, SPECIES_CODE))

# acreage information without 
hist_clim_x_tree_dat <- fread(paste(rootdir, "data/historical_climate_tree_data_cleaned.csv", sep = ""), sep=",", header=TRUE)

# data for map shading
background_map <- fread(paste(rootdir, "data/background_heatmap.csv", sep = ""))
multipoly <- background_map %>% dplyr::select(LON, LAT) %>% dplyr::distinct()
multipoly <- SpatialPointsDataFrame(multipoly[,c("LON","LAT")],multipoly)


#  Functions to load data ----------------------------
sel_CMIP5_dat <- function(locations) {
  
  # find closest location with available data
  LATS <- clim_dat$LAT
  LONS <- clim_dat$LON
  
  # iterate through selected points to find nearest data
  locs <- data.frame(LAT = c(), LON = c())
  for (i in 1:dim(locations)[1]){
    close_lat <- LATS[which.min(abs(LATS-locations[i,]$LAT))]
    close_lon <- LONS[which.min(abs(LONS-locations[i,]$LON))]
    locs <- rbind(locs, c(close_lat, close_lon))
  }
  locs <- locs %>% rename(LAT = 1, LON = 2)
  
  
  # select data corresponding to these points
  dat_all_locs <- subset(clim_dat, (LAT %in% locs$LAT) & (LON %in% locs$LON)) %>% drop_na()
  dat_all_locs <- dat_all_locs %>% select(c(-X, -LAT, -LON))
  
  # summarize across all selected locations
  dat_combi <- dat_all_locs %>% group_by(YR, RCP) %>% 
    summarize(mean_temp = mean(meantmp), mean_temp_LB = mean(meantmp.1), mean_temp_UB = mean(meantmp.2),
              max_temp = mean(maxtmp), max_temp_LB = mean(maxtmp.1), max_temp_UB = mean(maxtmp.2),
              min_temp = mean(mintmp), min_temp_LB = mean(mintmp.1), min_temp_UB = mean(mintmp.2),
              precip = mean(precip), precip_LB = mean(precip.1), precip_UB = mean(precip.2)) %>%
    ungroup()
  return(dat_combi)
}

sel_CMIP5_mon <- function(locations) {
  
  # find closest location with available data
  LATS <- clim_mon$LAT
  LONS <- clim_mon$LON
  # iterate through selected points and find corresponding data in dataframe
  locs <- data.frame(LAT = c(), LON = c())
  for (i in 1:dim(locations)[1]){
    close_lat <- LATS[which.min(abs(LATS-locations[i,]$LAT))]
    close_lon <- LONS[which.min(abs(LONS-locations[i,]$LON))]
    locs <- rbind(locs, c(close_lat, close_lon))
  }
  locs <- locs %>% rename(LAT = 1, LON = 2)
  # select data corresponding to these points
  dat_all_locs <- subset(clim_mon, (LAT %in% locs$LAT) & (LON %in% locs$LON)) %>% drop_na()
  dat_all_locs <- dat_all_locs %>% select(c(-V1, -LAT, -LON))

  # summarize across all selected locations
  dat_combi <- dat_all_locs %>% group_by(YR, RCP, MON) %>% 
    summarize(mean_temp = mean(meantmp),
              max_temp = mean(maxtmp), 
              min_temp = mean(mintmp), 
              precip = mean(precip)) %>% 
    ungroup() 
  return(dat_combi)
}


# Summary Statistics First Page ----------------------
# Currently is not used
summary_stats<- function(dat, year1, year2, RCP){
  
  yr1 <- dat %>%
    drop_na() %>% 
    dplyr::filter(YR == year1 &
             RCP == RCP)
  
  yr2 <- dat %>%
    drop_na() %>% 
    dplyr::filter(YR == year2 &
             RCP == RCP)

  df <- data.frame(Metric = c("Yearly Mean Temp",
                              "Yearly Average Max Temp",
                              "Yearly Average Min Temp",
                              "Annual Precipitation"),
                   Unit = c("°C", "°C", "°C", "mm/yr"),
                   Year1 = c(round(mean(yr1$mean_temp, na.rm = T), 2),
                             round(mean(yr1$max_temp, na.rm = T), 2),
                             round(mean(yr1$min_temp, na.rm = T), 2),
                             round(mean(yr1$precip, na.rm = T)*365, 2)),
                   Year2 = c(round(mean(yr2$mean_temp, na.rm = T), 2),
                             round(mean(yr2$max_temp, na.rm = T), 2),
                             round(mean(yr2$min_temp, na.rm = T), 2),
                             round(mean(yr2$precip, na.rm = T)*365, 2))) %>%
    mutate(Difference = Year2 - Year1,
           Difference = round(Difference, 2))

  colnames(df)[3] <- paste(year1)
  colnames(df)[4] <- paste(year2)
  df %>%
    DT::datatable(options = list(dom = 't'))
  # data.frame(df)
}
  