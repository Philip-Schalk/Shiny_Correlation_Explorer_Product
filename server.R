rm(list = ls())

server <- function(input, output) {
  
  output$info <- renderText({
    print(input$smooth_options)
  })
  
  output$sheet = renderUI({
    if(is.null(input$file)) return(NULL)
    all_sheets <- readxl::excel_sheets(input$file$datapath)
    
    selectizeInput(
      'sheet', 'Sheet', choices = all_sheets,
      options = list(
        placeholder = 'Choose Excel-Sheet you want to use',
        maxItems = 1
      ))
    
  })
  
  
  output$date_variable = renderUI({
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    all_vars <- colnames(read.xlsx(input$file$datapath, input$sheet, detectDates = TRUE, startRow = 1))
    
    selectizeInput(
      'date_variable', 'Date Variable', choices = all_vars,
      options = list(
        placeholder = 'Choose Date Variable',
        onInitialize = I('function() { this.setValue(""); }'),
        maxItems = 1
      ))
    
  })
  
  output$date_range <- renderUI({
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    if(!input$drop_dates) return(NULL)
    
    dateRangeInput("date_range", "Date Range", start = as.Date("2015-01-01"), end = Sys.Date())
  })
  
  output$dependent_variable = renderUI({
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    all_vars <- colnames(read.xlsx(input$file$datapath, input$sheet, detectDates = TRUE, startRow = 1))
    
    selectizeInput(
      'dependent_variable', 'Dependend Variable', choices = all_vars %>%  setdiff(input$date_variable),
      options = list(
        placeholder = 'Choose dependent Variables',
        onInitialize = I('function() { this.setValue(""); }'),
        maxItems = 1
      ))
    
  })
  
  
  
  output$independent_variables = renderUI({
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$dependent_variable)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    all_vars <- colnames(read.xlsx(input$file$datapath, input$sheet, detectDates = TRUE, startRow = 1))
    x_vars <- all_vars %>% setdiff(c(input$date_variable, input$dependent_variable))
    
    selectizeInput(
      'independent_variables', 'Independend Variables', choices = x_vars, multiple = TRUE, 
      options = list(
        placeholder = 'Choose independent Variables',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })
  
  output$lag_parameter <- renderUI({
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$dependent_variable)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    if(!"do_lag" %in% input$Transformations) return(NULL)
    
    sliderInput("lag_independent_variables", label = "Lag independent Variables", 
                min = 0, max = 5,step = 1,value = 0, width = "50%")
  })
  
  output$smooth_options <- renderUI({
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$dependent_variable)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    if(!"do_smooth" %in% input$Transformations) return(NULL)
    
    smooth_option_choices <- c("All", "The Dependent", "The Independents", "Customized Selection")
    radioButtons("smooth_option", "Which Variables to do you want to be smoothed?", choices = smooth_option_choices, inline = TRUE)
  })
  
  output$smooth_window <- renderUI({
    if(!"do_smooth" %in% input$Transformations) return(NULL)
    
    sliderInput("smooth_window", label = "Smoothing Window", 
                min = 0, max = 5, step = 1,value = 0, width = "100%")
  })
  
  output$smooth_variables <- renderUI({
    if(!"do_smooth" %in% input$Transformations) return(NULL)
    if(input$smooth_option != "Customized Selection") return(NULL)
    
    variable_choices <- c(input$dependent_variable, input$independent_variables)
    
    selectizeInput(
      'smooth_variables', 'Variables to smooth', choices = variable_choices,
      options = list(
        placeholder = 'Choose Variables you want to smooth',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })
  
  output$log_transform <- renderUI({
    if(!"do_log_transform" %in% input$Transformations) return(NULL)
    
    variable_choices <- c(input$dependent_variable, input$independent_variables)
    
    selectInput('log_variables', 'Log-transform these Variables:', choices = variable_choices, selected = NULL, multiple = TRUE)
  })
  
  
  data <- reactive({
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$dependent_variable)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    if(is.null(input$independent_variables)) return(NULL)
    
    data <- read.xlsx(input$file$datapath, input$sheet, detectDates = TRUE, startRow = 1)
    
    if(!is.null(input$date_range)){
      print(class(data[[input$date_variable]]))
      data %<>%  filter_at(input$date_variable, function(x) x < input$date_range[2] & x > input$date_range[1]) 
    }  
    
    if("do_log_transform" %in% input$Transformations){
      vars_to_log <- input$log_variables
      if(length(vars_to_log) > 0){
        data %<>% mutate_at(vars_to_log, function(x) log(1 + x)) 
      }
    }
    
    if("detrend_values" %in% input$Transformations){
      data %<>% mutate_if(is.numeric, detrend)
    }
    
    if("first_diffs" %in% input$Transformations){
      data %<>% mutate_if(is.numeric, function(x) c(NA, diff(x)))
    }
    
    if("do_lag" %in% input$Transformations){
      if(input$lag_independent_variables != 0){
        data %<>% mutate_at(input$independent_variables, function(x) lag(x, n = as.numeric(input$lag_independent_variables)))
      }
    }
    
    if("do_smooth" %in% input$Transformations){
      if(input$smooth_window != 0){
        
        if(input$smooth_option == "All"){
          vars_to_smooth <- c(input$independent_variables, input$dependent_variable)
        }  
        
        if(input$smooth_option == "The Dependent") vars_to_smooth <- input$dependent_variable
        
        if(input$smooth_option == "The Independents") vars_to_smooth <- input$independent_variables
        
        if(input$smooth_option == "Customized Selection") vars_to_smooth <- input$smooth_variables
        
        if(length(vars_to_smooth) > 0){
          vars_to_smooth <- data %>% select_at(vars_to_smooth) %>% select_if(is.numeric) %>% colnames
          data %<>% mutate_at(vars_to_smooth, function(x) rollmean(x, k = input$smooth_window, fill = NA, align = "right"))
        }
      }
    }
    
    if("scale_values" %in% input$Transformations){
      vars_to_scale <- data %>% select_if(is.numeric) %>% 
        colnames
      data %<>% mutate_at(vars_to_scale, function(x) as.numeric(scale(x))) 
    }
    
    data
  })
  
  output$timeseries_plot <- renderPlot({
    if(!input$eval) return(NULL)
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$dependent_variable)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    if(is.null(input$independent_variables)) return(NULL)
    
    plot.dat <- data() %>% select_at(c(input$date_variable, input$dependent_variable, input$independent_variables))
    
    dependent_variable_index <- which(colnames(plot.dat) == input$dependent_variable)
    date_variable_index <- which(colnames(plot.dat) == input$date_variable)
    new_names <- colnames(plot.dat)
    new_names[dependent_variable_index] <- "y"
    new_names[date_variable_index] <- "date"
    colnames(plot.dat) <- new_names
    
    
    plot.dat_long <- plot.dat %>% pivot_longer(cols = -"date")
    plot <- ggplot(plot.dat_long, aes(x = date, y = value, group = name, col = name)) + geom_line()
    # plot <- ggplotly(plot)
    print(plot)
  })
  
  output$correlations_with_y <- renderPlot({
    if(!input$eval) return(NULL)
    if(is.null(input$file)) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$dependent_variable)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    if(is.null(input$independent_variables)) return(NULL)
    
    dat.y <- data() %>% select(input$dependent_variable)
    dat.x <- data() %>% select(input$independent_variables)
    
    cors.df <- cor(dat.x, dat.y, use = "pairwise.complete") %>% as.data.frame() %>% rownames_to_column("Variable") %>% rename(Correlation = 2)
    
    plot <- cors.df %>% ggplot(aes(x = reorder(Variable, Correlation),  y = Correlation)) + 
      geom_bar(stat = "identity") + coord_flip() + xlab(NULL) + ylim(c(-1, 1)) + 
      theme(axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 20))
    print(plot)
  })
  
  output$correlations_matrix_plot <- renderPlot({
    if(!input$eval) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$dependent_variable)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    if(is.null(input$independent_variables)) return(NULL)
    
    corr_plot <- data() %>% select_if(is.numeric) %>% 
      select_at(input$independent_variables) %>% 
      cor(use = "pairwise.complete") %>% corrplot(type = "lower", tl.cex = .8, tl.col = "black") 
    
    print(corr_plot)
  })
  
  output$correlation_matrix <- renderTable({
    if(!input$eval) return(NULL)
    if(is.null(input$sheet)) return(NULL)
    if(is.null(input$dependent_variable)) return(NULL)
    if(is.null(input$date_variable)) return(NULL)
    if(is.null(input$independent_variables)) return(NULL)
    
    cors_df <- data() %>% select_if(is.numeric) %>% 
      cor(use = "pairwise.complete")
    
  })
  
  output$Download <- downloadHandler(
    
    filename = function(){
      input_filename <- input$file$name
      input_filename %<>% substr(start = 1, stop = nchar(.) - 5) 
      paste(input_filename, "_Correlations.xlsx", sep = "")
    },
    
    content = function(file){
      if(!input$eval) return(NULL)
      if(is.null(input$sheet)) return(NULL)
      if(is.null(input$dependent_variable)) return(NULL)
      if(is.null(input$date_variable)) return(NULL)
      if(is.null(input$independent_variables)) return(NULL)
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "parameters")
      parameters_df <- data.frame(scale_values = input$scale_values, 
                                  first_diffs = input$first_diffs, 
                                  detrend_values = input$detrend_values,
                                  lag_independent_variables = input$lag_independent_variables)
      writeData(wb, sheet = "parameters", x = parameters_df)
      
      openxlsx::addWorksheet(wb, "data")
      writeData(wb, sheet = "data", x = data())
      
      openxlsx::addWorksheet(wb, "correlations x-y")
      dat.y <- data() %>% select(input$dependent_variable)
      dat.x <- data() %>% select(input$independent_variables)
      cors.df.xy <- cor(dat.x, dat.y, use = "pairwise.complete") %>% as.data.frame() %>% 
        rownames_to_column("Variable") %>% rename(Correlation = 2)
      writeData(wb, sheet = "correlations x-y", x = cors.df.xy)
      
      openxlsx::addWorksheet(wb, "correlations x-x")
      cors.df.xx <- data() %>% select_if(is.numeric) %>% 
        select_at(input$independent_variables) %>% 
        cor(use = "pairwise.complete") %>% as.data.frame() %>% rownames_to_column("Variable_1") %>% 
        pivot_longer(cols = -1, names_to = "Variable_2", values_to = "Correlation") %>% 
        filter(Correlation != 1)
      writeData(wb, sheet = "correlations x-x", x = cors.df.xx)
      
      openxlsx::saveWorkbook(wb, file = file, overwrite = FALSE)
    }
  )
  
  
}


