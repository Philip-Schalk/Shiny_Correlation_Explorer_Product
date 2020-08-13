
rm(list = ls())

# Define UI
ui <- fluidPage(
  
  # img(src='tdreply_blue.png', align = "left", height = 50, width = 50),
  
  # App title ----
  # 
  titlePanel("Time Series Correlation Explorer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file", "Choose Excel-File from local Machine",
                multiple = FALSE,
                accept = c(".xlsx")),
      
      uiOutput("sheet"),
      uiOutput("date_variable"),
      checkboxInput("drop_dates", "Restrict Date Range", value = FALSE, width = "100%"),
      uiOutput("date_range"),
      
      uiOutput("dependent_variable"),
      uiOutput("independent_variables"),
      hr(),
      checkboxGroupInput("Transformations", "Transformations", 
                         choiceNames = c("Scale", "Use First Differences", "Detrend", "Lag", "Smooth", "Log-Transform"),
                         choiceValues = c("scale_values", "first_diffs", "detrend_values", "do_lag", "do_smooth", "do_log_transform")),
      
      uiOutput("lag_parameter"),
      uiOutput("smooth_options"),
      uiOutput("smooth_variables"),
      uiOutput("smooth_window"),
      uiOutput("log_transform"),
      
      checkboxInput("eval", "Evaluate Selection", value = TRUE, width = "200%"),
      titlePanel(img(src='tdreply_blue.jpg',  height = 150/3, width = 350/3)),
      downloadButton("Download", "Download")
      #selectInput("scale_values", "scale_values", choices = c(TRUE, FALSE), selected = FALSE)
    ),
    
    # Main panel for displaying outputs ----
    #titlePanel(img(src="VMA_Newsletter_Icon.png")),
    
    #, height = 45, width = 280)
    
    mainPanel(

      textOutput("info"),
      #plotlyOutput("timeseries_plot", height = "400px"),
      plotOutput("timeseries_plot", height = "400px"),
      hr(),
      plotOutput("correlations_with_y", height = "300px"),
      hr(),
      plotOutput("correlations_matrix_plot", height = "300px")
      
    )
  )
)