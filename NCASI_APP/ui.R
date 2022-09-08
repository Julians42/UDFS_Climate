# UI-side `Project Climate` App Implementation -------
# UDFS: Maxwell Vanlandschoot and Julian Schmitt -----
# Summer 2022 ----------------------------------------

source("utils.R")
   
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(bottom = 20, left = 30, width = "20%", draggable = FALSE,
                fluidRow(width = 12,
                         div(style = "display:inline-block",
                             actionButton("button1", "",
                                          icon = icon("info-circle", style="font-size: 60px; opacity: 0.9; color: black"),
                                          style = "background: transparent; border: none; opacity: 0.5;")
                         ),
                         div(style = "display:inline-block",
                             actionButton("button2", "",
                                          icon = icon("question-circle", style="font-size: 60px; opacity: 0.9; color: black"),
                                          style = "background: transparent; border: none; opacity: 0.5;")
                         )
                )
  ),
  absolutePanel(top = 10, 
                left = 50, 
                width = "10%", 
                draggable = TRUE,
                h3(align = "center", strong("Options")),
                div(
                  style = "padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFFF;", 
                  draggable = T,
                  selectizeInput(inputId = "species",
                                 label = "Tree Species",
                                 choices = unique(species_codes$COMMON_NAME),
                                 selected = "Red Maple",
                                 multiple = F),
                  sliderInput("year", "Reference Year - Projected Year", 
                              value = c(2022, 2080), min = 2000, max = 2099, sep = ""),
                  selectizeInput(inputId = "rcps",
                                 label = "RCP",
                                 choices = list("2.6" = 26, 
                                                "4.5" = 45, 
                                                "6.0" = 60, 
                                                "8.5" = 85),
                                 selected = 60,
                                 multiple = F),
                  p(style="text-align: justify; font-size = 25px",
                    em("This app is for demo purposes only -- full version is available through NCASI.org")
                  )
                )
  ),
  absolutePanel(top = 10, 
                right = 20, 
                width = "40%", 
                height = "80%", 
                draggable = T, 
              div(
                style = "padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFFF;", 
                tabsetPanel(type = "tabs", id = "viz",
                            tabPanel(icon("question-circle"), value = "home",
                                     h3("Please Select Plot(s)", 
                                        align = "center"),
                                     h5("Use your cursor to click on a single rectangular plot or 
                                        click the pentagon in the top left corner to draw a shape 
                                        around several plots. Data for all plots inside of the drawn 
                                        shape will be averaged in visualizations and tables.", align = "center"),
                                     br(),
                                     div(style = "display:inline-block",
                                         actionButton("button3", "Click Me for More Help",
                                                      icon = icon("question-circle"),
                                                      style = "background-color: #7F7A8E; border-color: #60546D; opacity: 0.85; color: white; width: 100%"), 
                                         style = "float:right"),
                                     br(),
                                     div(style = "display:inline-block",
                                         actionButton("button4", "Click Me for Information About this App",
                                                      icon = icon("info-circle"),
                                                      style = "background-color: #7F7A8E; border-color: #60546D; opacity: 0.85; color: white; width: 100%"), 
                                         style = "float:right")
                            ),
                            tabPanel("Table", value = "p1",
                                     h3(textOutput("sumtabtitle"), align = "center"
                                     ),
                                     box(DT::dataTableOutput("table") %>% 
                                           withSpinner(type = 6, size = 1.5), 
                                           width = "40%",
                                           height = "80%",
                                           style='overflow-y: scroll;')
                            ),
                            tabPanel("Yearly",
                                     h3(textOutput("linetabtitle"), align = "center"
                                     ),
                                     fluidRow(width = 12,
                                              column(width = 6,
                                                     box(selectizeInput(inputId = "metrics_line",
                                                                        label = "Climate Metric",
                                                                        choices = list("Mean Temperature" = "mean_temp", 
                                                                                       "Minimum Temperature" = "min_temp", 
                                                                                       "Maximum Temperature" = "max_temp", 
                                                                                       "Annual Precipitation" = "precip"),
                                                                        selected = "mean_temp",
                                                                        multiple = F), 
                                                         width = "100%"),
                                                     
                                                     ),
                                              column(width = 6, 
                                                     box(radioButtons(inputId = "var_toggle_yearly",
                                                                        label = "Toggle Variability",
                                                                        choices = list("On" = TRUE,
                                                                                       "Off" = FALSE),
                                                                        selected = FALSE,
                                                                        inline = FALSE),
                                                         width = "100%"),
                                                     )
                                              )
                                     ,
                                     box(plotlyOutput("lines") %>% 
                                           withSpinner(type = 6, size = 1.5), 
                                         width = "40%")
                            ),
                            tabPanel("Monthly",
                                     h3(textOutput("montabtitle"), align = "center"
                                     ),
                                     box(selectizeInput(inputId = "metrics",
                                                        label = "Climate Metric",
                                                        list("Mean Temperature" = "mean_temp", 
                                                             "Maximum Temperature" = "max_temp", 
                                                             "Minimum Tempurature" = "min_temp", 
                                                             "Annual Precipitation" = "precip"),
                                                        selected = "mean_temp",
                                                        multiple = F), 
                                         width = "40%"),
                                     box(plotlyOutput("curves") %>% 
                                           withSpinner(type = 6, size = 1.5), 
                                         width = "40%")
                            ),
                            
                            tabPanel("Density",
                                     h3(textOutput("heattabtitle"), align = "center"
                                     ),
                                     fluidRow(width = 12,
                                              column(width = 1),
                                              column(width = 3,
                                                     box(radioButtons(inputId = "density_var",
                                                                      label = "Toggle Variability",
                                                                      list("On" = TRUE,
                                                                           "Off" = FALSE),
                                                                      selected = FALSE,
                                                                      inline = T), 
                                                         width = "100%")
                                              ),
                                              column(width = 4,
                                                     box(selectizeInput(inputId = "x_axis_metric",
                                                                        label = "X-axis Climate Variable",
                                                                        list("Mean Temperature" = "mean_temp", 
                                                                             "Maximum Temperature" = "max_temp", 
                                                                             "Minimum Tempurature" = "min_temp", 
                                                                             "Annual Precipitation" = "precip"),
                                                                        selected = "mean_temp",
                                                                        multiple = F),
                                                         width = "100%")),
                                              column(width = 4, 
                                                     box(selectizeInput(inputId = "y_axis_metric",
                                                                        label = "Y-axis Climate Variable",
                                                                        list("Mean Temperature" = "mean_temp", 
                                                                             "Maximum Temperature" = "max_temp", 
                                                                             "Minimum Tempurature" = "min_temp", 
                                                                             "Annual Precipitation" = "precip"),
                                                                        selected = "precip",
                                                                        multiple = F),
                                                         width = "100%"))),
                                     box(plotlyOutput("heatmap") %>% 
                                           withSpinner(type = 6, size = 1.5),
                                         width = "40%")
                            ),
                            
                            tabPanel("Lookup Table",
                                     box(DT::dataTableOutput("lookup"), 
                                                             width = "40%",
                                                             style='overflow-y: scroll;'),
                                     downloadLink('downloadData', 'Download .csv'),
                                     br()
                                     
                            )
                            
                )
  )
)
)    