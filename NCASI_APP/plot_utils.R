# `Project Climate` Plotting Functions ---------------
# UDFS: Maxwell Vanlandschoot and Julian Schmitt -----
# Summer 2022 ----------------------------------------



# Plot Mean T/P Conditions throughout 21st Century ---
plt_clim_trajectory_CMIP5 <- function(full_dat, var, year1, year2, toggle_var, ci_level = 90) {
  
  # get quantiles from CI_level
  metric <- NA
  unit <- "(°C)"

  # Static Parameters and colors  
  scens <- c(26, 45, 60,85)
  grp = c("grp1", "grp2", "grp3", "grp4")
  fill_cols <-c('rgba(48, 90, 219, 0.4)', 'rgba(98, 170, 88, 0.4)', 'rgba(255, 255, 0, 0.4)', 'rgba(255, 0, 0, 0.4)')
  fill_transp <-c('rgba(48, 90, 219, 0.4)', 'rgba(98, 170, 88, 0.4)', 'rgba(255, 255, 0, 0.4)', 'rgba(255, 0, 0, 0.4)')

  # Select year range, clean data, and group
  CI_dat <- full_dat %>% filter(YR >= year1 & YR <= year2)

  # Process Data to yearly level based on parameter of interest, set metric and unit params
  if (var %in% c("mean_temp")) {
    CI_dat <- CI_dat %>% select(YR, RCP, mean_temp, mean_temp_LB, mean_temp_UB) %>% 
      rename(ens_mean = "mean_temp", LB = "mean_temp_LB", UB = "mean_temp_UB") %>% ungroup()
    metric <- "Mean Temperature"
  } else if (var %in% c("precip")) {
    CI_dat <- CI_dat %>% select(YR, RCP, precip, precip_LB, precip_UB) %>% 
      rename(ens_mean = "precip", LB = "precip_LB", UB = "precip_UB") %>% ungroup() %>% 
      mutate(ens_mean = ens_mean*365.25, LB = LB*365.25, UB = UB*365.25)
    #CI_dat <- CI_dat %>% summarize(annual_mean = mean(get(var))*365.25) # convert from mm/day -> mm/yr
    metric <- "Mean Precipitation"
    unit <- "(mm/yr)" # update unit for precipitation
  } else if (var %in% c("max_temp")) {
    CI_dat <- CI_dat %>% select(YR, RCP, max_temp, max_temp_LB, max_temp_UB) %>% 
      rename(ens_mean = "max_temp", LB = "max_temp_LB", UB = "max_temp_UB") %>% ungroup()
    metric <- "Max Temperature"
  } else { # then var == min_temp so return annual minimum 
    CI_dat <- CI_dat %>% select(YR, RCP, min_temp, min_temp_LB, min_temp_UB) %>% 
      rename(ens_mean = "min_temp", LB = "min_temp_LB", UB = "min_temp_UB") %>% ungroup()
    metric <- "Min Temperature"
  }

  
  # iteratively add mean and confidence intervals to plotly object
  fig <- plot_ly()
  for(i in 1:length(scens)) {
    dat <- CI_dat %>% filter(RCP == scens[i])
    fig <- add_trace(fig, data = dat, 
                     x = ~YR, 
                     y = ~ens_mean, 
                     name = paste("RCP", as.integer(scens[i])/10, "Ensembles"), 
                     type = 'scatter', 
                     mode = 'lines', 
                     line = list(color = fill_cols[i]), 
                     opacity = 1, 
                     legendgroup = grp[i])
    if (toggle_var) {
      fig <- fig %>% add_trace(data = dat, 
                               x = ~YR, 
                               y = ~LB, 
                               type = 'scatter', 
                               mode = 'lines',
                               fillcolor = fill_transp[i], 
                               line = list(color = 'transparent'),
                               showlegend = FALSE, 
                               name = paste("RCP", as.integer(scens[i])/10, "Ensembles"),
                               legendgroup = grp[i])
      fig <- fig %>% add_trace(data = dat, 
                               x = ~YR, 
                               y = ~UB, 
                               type = 'scatter', 
                               mode = 'lines',
                               fill = 'tonexty', 
                               fillcolor = fill_transp[i], 
                               line = list(color = 'transparent'),
                               showlegend = FALSE, 
                               name = paste("RCP", as.integer(scens[i])/10, "Ensembles"),
                               legendgroup = grp[i])
    }

  }
  
  fig <- fig %>% layout(title = paste("CMIP5 Ensemble Annual ", metric, " with ", ci_level, "% CI", sep=""),
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
# ----------------------------------------------------
  
# Plot Variable seasonally ---------------------------
plt_month_trajectory <- function(full_dat, var, year1, year2) {
  #' Plots Monthly Trajectory for Selected Climate Variable 
  #' 
  #' Takes loaded app data and variable of interest, returning
  #' plot_ly object with togglable Year and RCP scenario curves
  
  # Subset data to years and variable of interest
  dat <- full_dat %>% filter((YR == year1) | (YR == year2)) %>% 
    select(YR, MON, RCP, all_of(var))
  
  # rename variable of interest, summarize over ensemble members, and name months
  dat <- dat %>% dplyr::rename(vari = var) %>% 
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
  dat$ALPHA <- ((dat$YR-2000-75)/2+75)/100

  dat$month <- reorder(dat$month, dat$MON)
  labels. <- data.frame(row.names = c("mean_temp", "max_temp", "min_temp", "precip"), 
                        val = c("Mean Temperature (°C)", "Max Temperature (°C)", 
                                "Min Temperature (°C)", "Daily Precipitation (mm/day)"))
  
  # plot traces across unique ID 
  plt <- dat %>% group_by(TRACE_ID) %>% 
    plot_ly(x = ~month,
            y = ~month_mean, 
            type = "scatter", 
            color = ~as.factor(RCP),
            #alpha = ~ALPHA, # Having trouble setting this
            split = ~TRACE_ID,
            mode = "lines+markers", 
            #visibility = 'legendonly', 
            legendgroup = ~RCP, 
            name = ~TRACE_ID) %>% 
    layout(xaxis = list(title = "Month"),
           yaxis = list(title = labels.[var,]),
           title = "Climatology by Month")
  return(plt)
}
# ----------------------------------------------------

# Plot Species Heatmap -------------------------------
plt_clim_x_tree_heatmap <- function(full_dat, tree_id, var_x, var_y, rcps_, var = TRUE,
                                    hist_clim_x_tree_da = hist_clim_x_tree_dat) {
  #' Creates Heatmap of Tree Historical Distribution and Projected Location Pathway
  #' 
  #' Takes climate data from selected location (`full_dat`) alongside a selected tree 
  #' species (`tree_id`) and historical climate and tree dataset
  #' 
  #' Creates a heatmap of where tree species live historically in temperature 
  #' and precipitation space across the US. Overlays with climate projections from the 
  #' location of interest
  
  # filter historical climate data to species of interest
  filt_clim_x_tree_dat <- hist_clim_x_tree_da %>% dplyr::filter(COMMON_NAME == tree_id) %>% 
    mutate(ACRES = round(ACRES/1000))
  
  # for KDE density plot we repeat rows based on number of 1000s of acres (mem limit constraint)
  weighted.df <- filt_clim_x_tree_dat[rep(seq_len(dim(filt_clim_x_tree_dat)[1]), filt_clim_x_tree_dat$ACRES)]
  
  # Load climate data from selected location, select RCP, and group to decadal averages
  mean_dat <- full_dat %>% dplyr::select(RCP, YR, var_x, var_y, 
                                         paste(var_x, "LB", sep = "_"), 
                                         paste(var_x, "UB", sep = "_"),
                                         paste(var_y, "LB", sep = "_"),
                                         paste(var_y, "UB", sep = "_")) %>% 
    drop_na() %>% 
    dplyr::filter(RCP == rcps_) %>% 
    mutate(Decade = round(YR / 10) * 10) %>% 
    group_by(Decade) %>% 
    summarize(x_var = mean(get(var_x)),
              y_var = mean(get(var_y)),
              xLB = mean(get(paste(var_x, "LB", sep = "_"))),
              xUB = mean(get(paste(var_x, "UB", sep = "_"))),
              yLB = mean(get(paste(var_y, "LB", sep = "_"))),
              yUB = mean(get(paste(var_y, "UB", sep = "_"))))
  
  # plot background density
  fig <- plot_ly(data = weighted.df, x = ~get(var_x), y = ~get(var_y)) %>% 
    add_trace(type = 'histogram2dcontour',
              showlegend = F,
              histnorm = "probability density",
              nbinsx = 50,
              nbinsy = 50,
              line = list(smoothing = 1.3),
              colorscale = "Greys",
              reversescale = TRUE,
              showscale = F,
              name = "Species Density")  
  
  if (var) {
    # generate a set of points as the convex hull 
    hull_dat <- rbind(mean_dat %>% select(Decade, xLB, yLB) %>% rename(x = 2, y = 3), 
                      mean_dat %>% select(Decade, xUB, yUB) %>% rename(x = 2, y = 3),
                      mean_dat %>% select(Decade, xLB, yUB) %>% rename(x = 2, y = 3),
                      mean_dat %>% select(Decade, xUB, yLB) %>% rename(x = 2, y = 3)) %>% 
      group_by(Decade) %>% 
      nest() %>% 
      mutate(
        hull = map(data, ~ with(.x, chull(x, y))),
        out = map2(data, hull, ~ .x[.y,,drop=FALSE])
      ) %>%
      select(-data) %>%
      unnest(cols = c(hull, out)) 
    # add variability
    
    #obnoxiously defined discrete colorscheme 
    colorScale <- data.frame(z = seq(2000, 2100, 10), col = c("#440154", "#472474", "#414387", 
                                                              "#355F8D", "#2A788E", "#21918C", 
                                                              "#24A883", "#44BE70","#7BD151", 
                                                              "#BCDE28", "#FDE725"))
    # add uncertainty for each decade
    for (D in seq(from = 2000, to = 2100, by = 10)){
      dd <- hull_dat %>% filter(Decade ==D) %>% group_by(Decade) %>% slice(chull(x, y))
      fig <- fig %>% add_trace(data = dd,
                               x = ~x,
                               y = ~y,
                               type = 'scatter',
                               mode = 'none',
                               fill = 'toself',
                               fillcolor = (colorScale %>% filter(z == D))$col,
                               # fillpattern = list(
                               #   fillmode = "overlay"
                               #   ),
                               opacity = 0.2,
                               showlegend = F
      )
    }
    
  } 
  # add decadal means
  fig <- fig %>% add_trace(data = mean_dat,
                           type = 'scatter',
                           x = ~x_var,
                           y = ~y_var,
                           #color = ~Decade,
                           mode = 'markers',
                           marker= list(
                             size = 20,
                             color = ~Decade,
                             alpha = 1,
                             colorbar = list(
                               title = "Decade"
                             ),
                             colorscale = "Viridis",
                             name = "Decade"
                           )
  )
  
  
  # remove density fill legend, update plot labels
  labels. <- data.frame(row.names = c("mean_temp", "max_temp", "min_temp", "precip"), 
                        val = c("Yearly Mean Temperature", "Yearly Max Temperature", "Yearly Min Temperature", 
                                "Avg Daily Precipitation"))
  
  # Label Axes
  fig <- fig %>% layout(
    title = "Tree Density for Selected Species",
    yaxis = list(title = labels.[var_y, ]),
    xaxis = list(title = labels.[var_x, ])
  )
  
  return(fig)
}