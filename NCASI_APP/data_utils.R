# `Project Climate` Data Processing Functions --------
# UDFS: Maxwell Vanlandschoot and Julian Schmitt -----
# Summer 2022 ----------------------------------------

# slim data files 
clim_dat <- read.csv("~/Documents/Research/FIA_22/UFDS_Climate/data/yearly_ci_all.csv")
clim_dat <- clim_dat[-1, ]
clim_dat[] <- lapply(clim_dat, function(x) as.numeric(as.character(x)))
#clim_dat <- clim_dat %>% drop_na() %>% dplyr::mutate_if(is.character, as.numeric())


# load data files ------------------------------------
# load metadata crucial to file pathing and tree data
#meta <- readRDS(paste(rootdir, "data/meta.RData", sep=""))
tree_data <- fread(paste(rootdir, "data/tree_dat.csv", sep=""))
species_codes <- fread(paste(rootdir, "data/species_codes.csv", sep="")) %>% select(c(COMMON_NAME, SPECIES_CODE))
hist_clim_x_tree_dat <- data.table(read.table(paste(rootdir, "data/historical_climate_tree_data.csv", sep = ""), 
                                              sep=",", header=TRUE))
# rename to match columns of other datasets
hist_clim_x_tree_dat <- hist_clim_x_tree_dat %>% dplyr::rename(mean_temp = meantmp, 
                                                        min_temp = mintmp,
                                                        max_temp = maxtmp) 

# process data @MAXWELL - explain why were doing this
RCP_60_trim <- fread(paste(rootdir, "data/background_heatmap.csv", sep="")) %>%
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

sel_CMIP5_dat <- function(locations) {
  
  # find closest location with available data
  LATS <- clim_dat$LAT
  LONS <- clim_dat$LON
  # iterate through selected points 
  locs <- data.frame(LAT = c(), LON = c())
  for (i in 1:dim(locations)[1]){
    close_lat <- LATS[which.min(abs(LATS-locations[i,]$LAT))]
    close_lon <- LONS[which.min(abs(LONS-locations[i,]$LON))]
    locs <- rbind(locs, c(close_lat, close_lon))
  }
  locs <- locs %>% rename(LAT = 1, LON = 2)
  print(locs)
  # select data corresponding to these points
  dat_all_locs <- subset(clim_dat, (LAT %in% locs$LAT) & (LON %in% locs$LON)) %>% drop_na()
  dat_all_locs <- dat_all_locs %>% select(c(-X, -LAT, -LON))
  #dat_avg <-  dat_all_locs %>% group_by(RCP, YR) 
  #return(dat_all_locs)
  # agg out yr and rcp
  dat_combi <- dat_all_locs %>% group_by(YR, RCP) %>% 
    #summarize(meantmp = mean(meantmp))
    summarize(meantmp = mean(meantmp), meantmp_LB = mean(meantmp.1), meantmp_UB = mean(meantmp.2),
              maxtmp = mean(maxtmp), maxtmp_LB = mean(maxtmp.1), maxtmp_UB = mean(maxtmp.2),
              mintmp = mean(mintmp), mintmp_LB = mean(mintmp.1), mintmp_UB = mean(mintmp.2),
              precip = mean(precip), precip_LB = mean(precip.1), precip_UB = mean(precip.2))
  return(dat_combi)
}
# z <- sel_CMIP5_dat(data.frame(LAT = c(43.9375), LON = c(-70.6875))) 
# z




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



# Summary Statistics First Page ----------------------
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
                   Year1 = c(round(mean(yr1$meantmp, na.rm = T), 2),
                             round(mean(yr1$maxtmp, na.rm = T), 2),
                             round(mean(yr1$mintmp, na.rm = T), 2),
                             round(mean(yr1$precip, na.rm = T)*365, 2)),
                   Year2 = c(round(mean(yr2$meantmp, na.rm = T), 2),
                             round(mean(yr2$maxtmp, na.rm = T), 2),
                             round(mean(yr2$mintmp, na.rm = T), 2),
                             round(mean(yr2$precip, na.rm = T)*365, 2))) %>%
    mutate(Difference = Year2 - Year1,
           Difference = round(Difference, 2))

  colnames(df)[3] <- paste(year1)
  colnames(df)[4] <- paste(year2)
  df %>%
    DT::datatable(options = list(dom = 't'))
}
  