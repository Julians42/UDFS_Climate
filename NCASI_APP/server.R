# Server-side `Project Climate` App Implementation ---
# UDFS: Maxwell Vanlandschoot and Julian Schmitt -----
# Summer 2022 ----------------------------------------

  

# tdf <-  tibble(LAT = c(44.8125, 44.9375, 44.8125, 44.9375), LON = c(73.0625, 73.0625, 72.9375, 72.9375))

shinyServer(function(input, output, session) {

app_info <- modalDialog(
    tabsetPanel(
      tabPanel("About",
               h2("About this App", align = "center"),
               br(),
               h4("This demo app was created for NCASI through a collaboration of the 
                 Harvard University Department of Statistics and Reed College in the 
                 Undergraduate Forestry Data Science Lab (UFDS). The app is intended to 
                 help forestry professionals, conservationists, and data scientists understand
                 the potential impacts of climate change on our nations forests. The full 
                 app is available through NCASI.org.
                    ", align = "center"),
               br(),
               fluidRow(width = 12,
                        HTML('<img src="ncasi.png" width="30%">'),
                        HTML('<img src="veritas1.jpg" width="35%">'),
                        HTML('<img src="griffin.png" width="25%">')),
               fluidRow(width = 12,
                        column(width = 12,
                               
                        )
               ),
               
      ),
      tabPanel("Creators",
               h3("App Creators", align = "center"),
               fluidRow(width = 12,
                        column(width = 6,
                               HTML('<img src="maxwell1.png" width="100%">'),
                        ),
                        column(width = 6,
                               HTML('<img src="julian1.png" width="100%">'),
                        )
               ),
               fluidRow(width = 12,
                        column(width = 6,
                               h5(strong("Maxwell J.D. VanLandschoot"), align = "center"),
                               h6(align = "center",
                                  "Contact: ",
                                  a(href = "maxwelljdvl@gmail.com","maxwelljdvl@gmail.com")),
                               h6(align = "center",
                                  "Website: ",
                                  a(href = "mjdvl.com","mjdvl.com")),
                               p(style="text-align: justify; font-size = 25px",
                                 "Maxwell is a senior Economics major at Reed College and is an 
                                                      undergraduate Research Fellow at Harvard University. 
                                                      His research interests include: survey statistics, risk perception, 
                                                      and the application of statistics to law and policy. 
                                                      Beyond academia, Maxwell is active in the Reed Research Reactor 
                                                      and is an avid whitewater canoer.")
                               
                        ),
                        column(width = 6,
                               h5(strong("Julian Schmitt"), align = "center"),
                               h6(align = "center",
                                  "Contact: ",
                                  a(href = "jschmitt@college.harvard.edu","jschmitt@college.harvard.edu")),
                               h6(align = "center",
                                  "Website: ",
                                  a(href = "https://www.linkedin.com/in/julian-schmitt-a4450417a","link")),
                               p(style="text-align: justify; font-size = 25px",
                                 "Julian Schmitt is a senior at Harvard College concentrating in Applied Mathematics 
                                   with a focus in Earth and Planetary Sciences. His research interests include applications 
                                   of statistics and high performance computing to climate and climate risk assessment. 
                                   When he’s not studying the environment, Julian enjoys spending time training with the 
                                   Harvard Varsity Nordic Ski Team and playing trumpet in the Harvard Pops Orchestra.")
                               
                        )
               )
      ),
      tabPanel("Mentors",
               h3("Research Mentors", align = "center"),
               fluidRow(width = 12,
                        column(width = 6,
                               HTML('<img src="kelly1.jpg" width="100%">'),
                        ),
                        column(width = 6,
                               HTML('<img src="kate1.jpeg" width="100%">'),
                        )
               ),
               fluidRow(width = 12,
                        column(width = 6,
                               h5(strong("Dr. Kelly McConville"), align = "center"),
                               h6(align = "center",
                                  "Website: ",
                                  a(href = "https://mcconville.rbind.io/","https://mcconville.rbind.io/")),
                               p(style="text-align: justify; font-size = 25px",
                                 "Dr. Kelly McConville is a Survey Statistician and a Senior Lecturer on 
                                   Statistics at Harvard University.  Her work focuses on modern survey 
                                   estimation methods that combine multiple data sources through machine 
                                   learning tools.  She has been mentoring the Undergraduate Forestry Data 
                                   Science Program for four years now.  Outside of teaching and research, 
                                   she enjoys cooking up a perfect pot of beans.")
                               
                        ),
                        column(width = 6,
                               h5(strong("Dr. Kate Hu"), align = "center"),
                               h6(align = "center",
                                  "Website: ",
                                  a(href = "http://www.katehu.com/","http://www.katehu.com/")),
                               p(style="text-align: justify; font-size = 25px",
                                 "Dr. Kate Hu is currently a research fellow at Harvard School of Public Health, 
                                   investigating how to debias the inference due to unmeasured and mismeasured confounders, 
                                   with applications to evaluating health effects of climate change. Before returning to academia, 
                                   Dr. Kate Hu was the Head of Data Science at Aclima Inc.")
                               
                        )
               )
      ),
      tabPanel("Stakeholders",
               h3("NCASI Stakeholders", align = "center"),
               fluidRow(width = 12,
                        column(width = 6,
                               HTML('<img src="holly1.jpg" width="100%">'),
                        ),
                        column(width = 6,
                               HTML('<img src="steve1.jpg" width="100%">'),
                        )
               ),
               fluidRow(width = 12,
                        column(width = 6,
                               h5(strong("Dr. Holly L. Murno"), align = "center"),
                               h6(align = "center",
                                  "Website: ",
                                  a(href = "https://www.hollylynnmunro.com/","hollylynnmunro.com")),
                               p(style="text-align: justify; font-size = 25px",
                                 "Dr. Holly Murno is currently a Senior Research Scientist with the National Council 
                                   of Air and Stream Improvement (NCASI) and an Adjunct Assistant Professor at Warnell 
                                   School of Forestry and Natural Resources at the University of Georgia.")
                               
                        ),
                        column(width = 6,
                               h5(strong("Dr. Stephen Prisley"), align = "center"),
                               h6(align = "center",
                                  "Website: ",
                                  a(href = "https://www.linkedin.com/in/steve-prisley-8a769a13","link")),
                               p(style="text-align: justify; font-size = 25px",
                                 "Dr. Steve Prisley is a forest biometrician/economist with over 30 years of experience in the 
                                   forest industry, academia, and government. He managed forest resource information systems for 
                                   15 years for two integrated paper companies and was a professor of forest inventory and GIS at 
                                   Virginia Tech. He also developed and led the Center for Natural Resource Assessment and Decision 
                                   Support (CeNRADS) in the College of Natural Resources and Environment.")
                               
                        )
               )
      ),
      tabPanel("Data",
               h4(strong("Data Sources"), align = "center"),
               p(style="text-align: justify; font-size = 25px",
                 "Plot-level data on tree prevalance and climate was collated by Dr. Holly Murno. These data were further sourced
                   from the U.S. Forestry Inventory and Analysis Program (FIA) and from more than 200 climate models in the
                   Downscaled CMIP3 and CMIP5 Climate and Hydrology Projections. Data wrangling and analysis was conducted by Maxwell VanLandschoot
                   and Julian Schmitt and all relevant code is available on github at https://github.com/harvard-ufds/climate"
               )
               
      ),
      tabPanel("Disclaimer",
               h4(strong("Disclaimer"), align = "center"),
               p(style="text-align: justify; font-size = 25px",
                 "The statistics presented on this app are not official estimates of NCASI, 
                   rather they serve as illustrations of the potential effects of climate change
                   on our nation's forests. This demo app was prepared by Maxwell VanLandschoot and 
                   Julian Schmitt as a part of UFDS in the summer of 2022. The final app offered by NCASI
                   is a distinct product for which these creators take no responsibility. The any views or
                   results presented in this app are soley those of the creators and do not necessarily
                   represent the views of NCASI, the Harvard University Department of Statistics, or Reed College.
                   "
               )
               
      ),
      
    ),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "m"
  )
  
  showModal(app_info)

  output$sumtabtitle <- renderText({
    paste("Summary Table for",input$species,"Under RCP",as.numeric(input$rcps)/10,"from",input$year[1],"to",input$year[2],sep = " ")
  })
  
  output$linetabtitle <- renderText({
    paste("Climate Projections for Selected Plot(s) from",input$year[1],"to",input$year[2],sep = " ")
  })
  
  output$montabtitle <- renderText({
    paste("Monthly Climate Projections for",input$year[1],"and",input$year[2], "in Selected Plot(s)", sep = " ")
  })
  
  output$heattabtitle <- renderText({
    paste("Density Plot of",input$species,"Under RCP", as.numeric(input$rcps)/10, sep = " ") #,"from", input$year[1],"to", input$year[2]
  })
  
  observeEvent(input$map_shape_click, {
    if(input$viz == "home"){
      updateTabsetPanel(session, "viz",
                        selected = "p1"
      )
    }
  })
  
  observeEvent(input$map_draw_stop, {
    if(input$viz == "home"){
      updateTabsetPanel(session, "viz",
                        selected = "p1"
      )
    }
  })
  
  observeEvent(input$button1, {
    showModal(app_info)
  })
  
  observeEvent(input$button4, {
    showModal(app_info)
  })
  
  observeEvent(input$button2, {
    showModal(modalDialog(
      h3(strong("How to Use the App"), align = "center"),
      HTML('<img src = "tut.gif" width = "100%">'),
      easyClose = TRUE,
      footer = NULL,
      size = "m"
    ))
  })
  
  observeEvent(input$button3, {
    showModal(modalDialog(
      h3(strong("How to Use the App"), align = "center"),
      HTML('<img src = "tut.gif" width = "100%">'),
      easyClose = TRUE,
      footer = NULL,
      size = "m"
    ))
  })
  
  app_data <- reactive({

    if(is.null(input$map_draw_stop)){
      # extract latitude/longitude from user click
      lat <- input$map_shape_click[[3]]
      lon <- input$map_shape_click[[2]]
      df <- data.frame(LAT = c(lat), LON = c(lon))
      
      # load data for requested point 
      load_cmip5_from_df_all_rcp(df) 
      # temporarily load file for debugging purposes
      # fread(paste(rootdir, "data/dummy_app_data.csv", sep=""))

    }else{
        #use the draw_stop event to detect when users finished drawing
        req(input$map_draw_stop)
        print(input$map_draw_new_feature)
        
        #get the coordinates of the polygon
        polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
        
        #transform them to an sp Polygon
        drawn_polygon <- Polygon(do.call(rbind, lapply(polygon_coordinates,
                                                      function(x){c(x[[1]][1], x[[2]][1])})))
        
        #use over from the sp package to identify selected cities
        selected_plots <- multipoly %over% SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
        
        dat <- data.frame(multipoly[which(!is.na(selected_plots)),"LON"],
                          multipoly[which(!is.na(selected_plots)),"LAT"]) %>%
          dplyr::select(c(LON, LAT)) %>% tibble()
        
        # load data
        load_cmip5_from_df_all_rcp(dat)
        # fread(paste(rootdir, "data/dummy_app_data.csv", sep=""))
    }
  })
  
  output$table <- DT::renderDataTable({
    
    
    
    yr1 <- app_data() %>%
      filter(YR == input$year[1] &
               RCP == input$rcps)
    
    yr2 <- app_data() %>%
      filter(YR == input$year[2] &
               RCP == input$rcps)
    
    df <- data.frame(Metric = c("Yearly Mean Temp",
                                "Yearly Average Max Temp",
                                "Yearly Average Min Temp",
                                "Highest Yearly Temperature",
                                "Lowest Yearly Temperature",
                                "Annual Precipitation"),
                     Year1 = c(round(mean(yr1$yr_mean_temp, na.rm = T), 2),
                               round(mean(yr1$yr_max_temp, na.rm = T), 2),
                               round(mean(yr1$yr_min_temp, na.rm = T), 2),
                               round(max(yr1$max_temp, na.rm = T), 2),
                               round(min(yr1$min_temp, na.rm = T), 2),
                               round(mean(yr1$yr_precip, na.rm = T), 2)),
                     Year2 = c(round(mean(yr2$yr_mean_temp, na.rm = T), 2),
                               round(mean(yr2$yr_max_temp, na.rm = T), 2),
                               round(mean(yr2$yr_min_temp, na.rm = T), 2),
                               round(max(yr2$max_temp, na.rm = T), 2),
                               round(min(yr2$min_temp, na.rm = T), 2),
                               round(mean(yr2$yr_precip, na.rm = T), 2))) %>% 
      mutate(Difference = Year2 - Year1,
             Difference = round(Difference, 2))
    
    colnames(df)[2] <- paste(input$year[1])
    colnames(df)[3] <- paste(input$year[2])
    
    df %>%
      DT::datatable(options = list(dom = 't'))
    
  })
  
  output$lines <- renderPlotly({
    plt_clim_trajectory_CMIP5(app_data(), input$metrics_line, input$year[1], input$year[2], toggle_var = input$var_toggle_yearly)
  })
  
  output$curves <- renderPlotly({
    plt_month_trajectory(app_data(), input$metrics, input$year[1], input$year[2])
    
  })

  output$heatmap <- renderPlotly({
    plt_clim_x_tree_heatmap(app_data(), input$species, input$x_axis_metric, input$y_axis_metric, input$rcps, input$density_var, hist_clim_x_tree_dat)
  })
  
  output$julian_break <- renderTable({
    
    scens <- c(26, 45, 60,85)
    fill_cols <-c('rgba(48, 90, 219, 0.7)', 'rgba(98, 170, 88, 0.7)', 'rgba(255, 255, 0, 0.7)', 'rgba(255, 0, 0, 0.7)')
    
    metric <- input$metrics_line
    yr_metric <- paste("yr_", input$metrics_line, sep = "")
    
    dat <- app_data() %>% 
        dplyr::select(c(YR, RCP, !!metric, !!yr_metric))
    
    dat <- dat %>%
        group_by(YR, RCP) %>%
        summarize(UB = quantile(x = dat[3], probs = 0.95, na.rm = T),
                  LB = quantile(x = dat[3], probs = 0.05, na.rm = T),
                  ens_mean = round(x = dat[4], 2)) %>%
      unique()
  
    dat %>% head(10)
    
  })
  
  output$map <- renderLeaflet({
    tree_dat <- tree_data %>% 
      drop_na(COMMON_NAME) %>%
      filter(COMMON_NAME == input$species) %>%
      unique()
    
    dat_1 <- RCP_60_trim %>%
      filter(YR == input$year[1] &
               MON == 6)
    
    dat_1 <- dat_1 %>%
      mutate(poly_id = 1:nrow(dat_1))
    
    dat_join <- full_join(tree_dat, dat_1) %>%
      drop_na()
    
    leaf_pallet <- colorBin("viridis", domain = dat_join$ACRES)
    
    dat_join %>%
      distinct(ACRES, LAT, LON, meantmp, .keep_all= TRUE) %>%
      distinct(ACRES, .keep_all= TRUE) %>%
      distinct(meantmp, .keep_all= TRUE) %>%
      leaflet() %>%
      addTiles() %>%
      addRectangles(
        lng1 = dat_join$lon1,
        lng2 = dat_join$lon2,
        lat1 = dat_join$lat1,
        lat2 = dat_join$lat2,
        stroke = T,
        weight = 1,
        color = "grey",
        fill = T,
        fillColor = ~leaf_pallet(dat_join$ACRES),
        fillOpacity = 0.3,
        popup = paste0(
          "<b>", "Tree Information", "</b>",
          "<br>Tree Type: ", dat_join$COMMON_NAME,
          "<br>Acres of Tree: ", round(dat_join$ACRES, 1),
          "<br>",
          "<b>", "Plot Location", "</b>",
          "<br>Latitude: ", dat_join$LAT,
          "<br>Longitude: ", dat_join$LON
        ),  
      ) %>%
      addLegend("bottomright", 
                pal = leaf_pallet, 
                values = ~dat_join$ACRES,
                title = "Acres of selected tree",
                opacity = 1
      ) %>%
      addDrawToolbar(
        targetGroup='draw',
        polylineOptions = F,
        rectangleOptions = F,
        circleMarkerOptions = F,
        markerOptions = F,
        circleOptions = F,
        singleFeature = T) %>% 
      fitBounds(-72, 43, -70, 45)
  })


  output$lookup <- DT::renderDataTable({
    app_data() %>%
      select(-V1) %>%
      mutate(across(numeric(), round, 2)) %>%
      DT::datatable(options = list(pageLength = 6,
                                   lengthMenu = c(4, 6, 8)))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(app_data, con)
    }
  )
  
})
