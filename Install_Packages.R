required_packages <- c("digest", "htmltools", "mime", "shiny", "openxlsx", "corrplot", 
                       "magrittr", "dplyr", "tibble", "tidyr", "pracma", "ggplot2")
for(i in 1:length(required_packages)){
  print(paste0("Install Package --- ", required_packages[i]))
  install.packages(required_packages[i])
}