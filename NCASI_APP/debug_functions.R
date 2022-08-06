# Debug Shiny app Functions 


# load dataset 
library(tidyverse)
# rootdir <- "/Users/julianschmitt/Documents/Research/FIA_22/climate/"
rootdir <- "/Users/maxwell/climate/"
full_dat <- fread(paste(rootdir, "data/dummy_app_data.csv", sep=""))

    

# line functionality
plt_clim_trajectory_CMIP5 <- function(full_dat, var, metric, unit, ci_level = 90) {
  
  # get quantiles from CI_level
  l_quantile = (100-ci_level)/2/100
  h_quantile = 1 - l_quantile
  
  # load data
  scens <- c(26, 45, 60,85)
  fill_cols <-c('rgba(48, 90, 219, 0.7)', 'rgba(98, 170, 88, 0.7)', 'rgba(255, 255, 0, 0.7)', 'rgba(255, 0, 0, 0.7)')
  
  CI_dat <- full_dat %>% group_by(YR, MODEL, RCP) %>% 
    summarize(annual_mean = mean( {{ var }}, na.rm = TRUE)) %>% 
    ungroup() %>%
    group_by(YR, RCP) %>%
    summarize("quantile" = list(round(quantile(annual_mean, c(l_quantile, h_quantile), na.rm = T), 2)),
              "ens_mean" = round(mean(annual_mean, na.rm=T), 2)) %>%
    unnest_wider(quantile) %>%
    rename(LB = "5%", UB = "95%") %>%
    ungroup()
  
  # iteratively add mean and confidence intervals to plotly object
  fig <- plot_ly()
  for(i in 1:length(scens)) {
    dat <- CI_dat %>% filter(RCP == scens[i])
    fig <- add_trace(fig, data = dat, 
                     x = ~YR, 
                     y = ~ens_mean, 
                     name = paste(scens[i], "Ensemble Mean"), 
                     type = 'scatter', 
                     mode = 'lines', 
                     line = list(color = fill_cols[i]), 
                     opacity = 1)
    fig <- fig %>% add_trace(data = dat, 
                             x = ~YR, 
                             y = ~LB, 
                             type = 'scatter', 
                             mode = 'lines',
                             fillcolor = fill_cols[i], 
                             opacity = 0.7, 
                             group = ~YR, 
                             line = list(color = 'transparent'),
                             showlegend = FALSE, 
                             name = paste(scens[i],'Lower 90% CI'))
    fig <- fig %>% add_trace(data = dat, 
                             x = ~YR, 
                             y = ~UB, 
                             type = 'scatter', 
                             mode = 'lines',
                             fill = 'tonexty', 
                             fillcolor = fill_cols[i], 
                             opacity = 0.7, 
                             group = ~year, 
                             line = list(color = 'transparent'),
                             showlegend = FALSE, 
                             name = paste(scens[i], 'Upper 90% CI'))
  }
  
  fig <- fig %>% layout(title = paste("CMIP5 Ensemble Annual Average ", metric, " with ", ci_level, "% CI", sep=""),
                        paper_bgcolor = 'rgb(255,255,255)', 
                        plot_bgcolor = 'rgb(229,229,229)',
                        xaxis = list(title = "Year",
                                     gridcolor = 'rgb(255,255,255)',
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     tickcolor = 'rgb(127,127,127)',
                                     ticks = 'outside',
                                     zeroline = FALSE),
                        yaxis = list(title = paste(metric, unit, sep=" "),
                                     gridcolor = 'rgb(255,255,255)',
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     tickcolor = 'rgb(127,127,127)',
                                     ticks = 'outside',
                                     zeroline = FALSE))
  return(fig)
}

plt_clim_trajectory_CMIP5(full_dat, "mean_temp", "Temperature", "°C", 90)


dim(full_dat)

l_quantile = 0.05
h_quantile = 0.95

CI_dat2 <- full_dat %>% group_by(YR, MODEL, RCP) %>% 
  summarize(annual_mean = mean(mean_temp, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by(YR, RCP) %>%
  summarize("quantile" = list(round(quantile(annual_mean, c(l_quantile, h_quantile), na.rm = T), 2)),
            "ens_mean" = round(mean(annual_mean, na.rm=T), 2)) %>%
  unnest_wider(quantile) %>%
  rename(LB = "5%", UB = "95%") %>%
  ungroup()



plt_month_trajectory <- function(full_dat, var, year1, year2) {
  #' Plots Monthly Trajectory for Selected Climate Variable 
  #' 
  #' Takes loaded app data and variable of interest, returning
  #' plot_ly object with togglable Year and RCP scenario curves
  
  # Subset data to years and variable of interest
  dat <- full_dat %>% filter((YR == year1) | (YR == year2)) %>% 
    select(YR, MON, RCP, MODEL, all_of(var))
  
  # rename variable of interest, summarize over ensemble members, and name months
  dat <- dat %>% rename(vari = var) %>% 
    group_by(YR, RCP, MON) %>% 
    summarize(month_mean = mean(vari, na.rm = T)) %>% 
    mutate(month = case_when(MON == 1 ~ "January",
                           MON == 2 ~ "February",
                           MON == 3 ~ "March",
                           MON == 4 ~ "April",
                           MON == 5 ~ "May",
                           MON == 6 ~ "June",
                           MON == 7 ~ "July",
                           MON == 8 ~ "August",
                           MON == 9 ~ "September",
                           MON == 10 ~ "October",
                           MON == 11 ~ "November",
                           MON == 12 ~ "December"))
  
  # add unique ID for year and RCP combination and plot over pairs
  dat$TRACE_ID <- str_c("Year: ", dat$YR, ", RCP: ", dat$RCP/10) # MAXWELL - would be nice to format 6 as "6.0"
  
  dat$month <- reorder(dat$month, dat$MON)
  
  # plot traces across unique ID 
  plt <- dat %>% group_by(TRACE_ID) %>% 
    plot_ly(x = ~month,
            y = ~month_mean, 
            type = "scatter", 
            color = ~TRACE_ID, 
            split = ~TRACE_ID,
            mode = "lines+markers", 
            visibility = 'legendonly') %>% 
    layout(xaxis = list(title = "Month"),
           yaxis = list(title = paste("Variable (EX TEMP)","(Degrees C)", sep = " ")),
           title = "Climatology by Month")
  return(plt)
}
plt_month_trajectory(full_dat, "max_temp", 2022, 2080)

var = "mean_temp"
t <- full_dat %>% filter((YR ==2080) | (YR == 2022)) %>% 
  select(YR, MON, RCP, MODEL, all_of(var))
t2 <- t %>% group_by(YR, MON, RCP) %>% 
  rename(vari = var) %>% 
  summarize(month_mean = mean(vari, na.rm = T))
t2

p <- plot_ly(type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "Month"),
         yaxis = list(title = paste("Variable (e.g. temp)","(Degrees C)", sep = " "))) #input$metrics

t2  %>% group_by(TRACE_ID) %>% 
  plot_ly(x = ~MON, 
          y = ~ month_mean, 
          type ="scatter", 
          color=~TRACE_ID, 
          split = ~TRACE_ID,
          mode = "lines+markers", 
          visibility = 'legendonly') %>% 
  layout(xaxis = list(title = "Month"),
         yaxis = list(title = paste("Variable (EX TEMP)","(Degrees C)", sep = " ")),
         title = "Climatology by Month")

library(stringr)
#t2$TRACE_ID <- do.call(paste0, t2[c("YR", "RCP")])
t2$TRACE_ID <- str_c("Year: ", t2$YR, ", RCP: ", t2$RCP/10)
t2








# debug heatmap --------------------------------------
# tree_data <- fread(paste(rootdir, "data/leaflet_dat/tree_dat.csv", sep = ""))
hist_clim_x_tree_dat <- data.table(read.table(paste(rootdir, "data/clim_tree_baseline.csv", sep = ""), 
                                          sep=",", header=TRUE))
hist_clim_x_tree_dat <- hist_clim_x_tree_dat %>% rename(mean_temp = meantmp, 
                                                        min_temp = mintmp,
                                                        max_temp = maxtmp)


# clim_dat <- full_dat %>% dplyr::select(RCP, YR, MON, MODEL, mean_temp, precip) %>% 
#   dplyr::filter(RCP == 85) %>% 
#   mutate(Decade = round(YR / 10) * 10) %>% 
#   group_by(RCP, Decade, MODEL) %>% 
#   summarize(mean_temp = mean(mean_temp, na.rm = T),
#             precip = mean(precip, na.rm = T))


plt_clim_x_tree_heatmap <- function(full_dat, tree_id, var_x, var_y, RCP, 
                                    hist_clim_x_tree_dat = hist_clim_x_tree_dat) {
  #' Creates Heatmap of Tree Historical Distribution and Projected Location Pathway
  #' 
  #' Takes climate data from selected location (`full_dat`) alongside a selected tree 
  #' species (`tree_id`) and historical climate and tree dataset
  #' 
  #' Creates a heatmap of where tree species live historically in temperature 
  #' and precipitation space across the US. Overlays with climate projections from the 
  #' location of interest
  
  # filter historical climate data to species of interest
  filt_clim_x_tree_dat <- hist_clim_x_tree_dat %>% filter(SPECIES_CODE == tree_id)
  
  # # Plot tree data baseline
  plt <- ggplot(filt_clim_x_tree_dat, aes(x=get(var_x), y=get(var_y))) +
    stat_density2d(aes(alpha=..level.., fill=..level.., weight=ACRES),  bins=10, geom="polygon")+
    scale_fill_gradient(low="blue", high="red")+
    scale_alpha(range = c(0.1, 0.9), guide="none") +
    geom_density2d(colour="black", bins=10, aes(weight=ACRES)) +
    labs(x="Mean Temperature", y="Mean Annual Precipitation", 
         title="Tree Density for Selected Species", fill="Density")
  
  # Load climate data from selected location, select RCP, and group to decadal averages
  clim_dat <- full_dat %>% dplyr::select(RCP, YR, MON, MODEL, var_x, var_y) %>% 
    dplyr::filter(RCP == RCP) %>% 
    mutate(Decade = round(YR / 10) * 10) %>% 
    group_by(RCP, Decade, MODEL) %>% 
    summarize(x_var = mean(get(var_x), na.rm = T),
              y_var = mean(get(var_y), na.rm = T))
  
  # add climate data to map from point of interest - across ensemble members
  plt <- plt + geom_line(data=clim_dat, 
              aes(x = x_var, y = y_var, color = `Decade`, group = MODEL, 
                  alpha=0.05, stroke=0.5), size=1.5)+
    scale_color_viridis_c(direction=-1)
  
  # calculate the ensemble means for point of interest
  mean_dat <- clim_dat %>% group_by(Decade) %>% 
    summarize(x_var = mean(x_var, na.rm=TRUE), 
              y_var = mean(y_var, na.rm=TRUE)) 

  # add ensemble mean by decade to map
  plt <- plt + geom_line(data = mean_dat, aes(x=x_var, y=y_var))
  
  # remove density fill legend and return plot
  plt <- plt + guides(fill="none")
  return(plt)
}
plt_clim_x_tree_heatmap(full_dat, 261, "mean_temp", "precip", 85,  hist_clim_x_tree_dat)




hist_clim_x_tree_dat %>% filter(SPECIES_CODE == 261)


full_dat %>% dplyr::select("mean_temp")


# ------------------------------------------------
plt_clim_trajectory_CMIP5 <- function(full_dat, var, metric, unit, ci_level = 90) {
  
  # get quantiles from CI_level
  l_quantile = (100-ci_level)/2/100
  h_quantile = 1 - l_quantile
  
  # load data
  scens <- c(26, 45, 60,85)
  grp = c("grp1", "grp2", "grp3", "grp4")
  fill_cols <-c('rgba(48, 90, 219, 0.7)', 'rgba(98, 170, 88, 0.7)', 'rgba(255, 255, 0, 0.7)', 'rgba(255, 0, 0, 0.7)')
  
  CI_dat <- full_dat %>% group_by(YR, MODEL, RCP) %>% 
    summarize(annual_mean = mean(get(var), na.rm = TRUE)) %>% 
    ungroup() %>%
    group_by(YR, RCP) %>%
    summarize("quantile" = list(round(quantile(annual_mean, c(l_quantile, h_quantile), na.rm = T), 2)),
              "ens_mean" = round(mean(annual_mean, na.rm=T), 2)) %>%
    unnest_wider(quantile) %>%
    rename(LB = "5%", UB = "95%") %>%
    ungroup()
  
  # iteratively add mean and confidence intervals to plotly object
  fig <- plot_ly()
  for(i in 1:length(scens)) {
    dat <- CI_dat %>% filter(RCP == scens[i])
    fig <- add_trace(fig, data = dat, 
                     x = ~YR, 
                     y = ~ens_mean, 
                     name = paste("RCP", as.integer(scens[i])/10, "Ensemble Mean"), 
                     type = 'scatter', 
                     mode = 'lines', 
                     line = list(color = fill_cols[i]), 
                     opacity = 1, 
                     legendgroup = grp[i])
    fig <- fig %>% add_trace(data = dat, 
                             x = ~YR, 
                             y = ~LB, 
                             type = 'scatter', 
                             mode = 'lines',
                             fillcolor = fill_cols[i], 
                             opacity = 0.7, 
                             group = ~YR, 
                             line = list(color = 'transparent'),
                             showlegend = FALSE, 
                             name = paste("RCP", as.integer(scens[i])/10, "Ensemble Mean"),
                             legendgroup = grp[i])
    fig <- fig %>% add_trace(data = dat, 
                             x = ~YR, 
                             y = ~UB, 
                             type = 'scatter', 
                             mode = 'lines',
                             fill = 'tonexty', 
                             fillcolor = fill_cols[i], 
                             opacity = 0.7, 
                             group = ~year, 
                             line = list(color = 'transparent'),
                             showlegend = FALSE, 
                             name = paste("RCP", as.integer(scens[i])/10, "Ensemble Mean"),
                             legendgroup = grp[i])
  }
  
  fig <- fig %>% layout(title = paste("CMIP5 Ensemble Annual Average ", metric, " with ", ci_level, "% CI", sep=""),
                        paper_bgcolor = 'rgb(255,255,255)', 
                        plot_bgcolor = 'rgb(229,229,229)',
                        xaxis = list(title = "Year",
                                     gridcolor = 'rgb(255,255,255)',
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     tickcolor = 'rgb(127,127,127)',
                                     ticks = 'outside',
                                     zeroline = FALSE),
                        yaxis = list(title = paste(metric, unit, sep=" "),
                                     gridcolor = 'rgb(255,255,255)',
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     tickcolor = 'rgb(127,127,127)',
                                     ticks = 'outside',
                                     zeroline = FALSE))
  return(fig)
}


plt_clim_trajectory_CMIP5(full_dat, "mean_temp", "Temperature", "Deg C")


full_dat



