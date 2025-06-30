prepare_data = function(formula, data, mode, list = list, dist, verbose = TRUE){
  
  if (verbose){
    cat("enter the data checking\n")
  }
  
  #get all the input data and store it:
  list$mode = mode
  list$dist = dist
  list$formula = as.formula(formula)
  list$data_variables = all.vars(formula)
  list$raw_data = data[list$data_variables] # subset data to only the data that was used
  list$data_all_including_unused = data #store all the other variables as well
  
  #check the data for NA's
  list$data_na.omit = na.omit(list$raw_data) #delete all rows with NA values
  list$data_na.omit = as.data.frame(list$data_na.omit) # in case of tibble as input, make a dataframe out of it
  list$data_na.omitted_number = nrow(list$raw_data) - nrow(list$data_na.omit)
  list$data_na.omitted_number_text = paste(list$data_na.omitted_number, "NA's omitted\n")
  
  # produce correlation plot for input data
  png('../output/plots/corr_plot.png', width = 6, units = "in", height = 6, res = 300) # start recording
  
  psych::pairs.panels(list$data_na.omit, method = "pearson") 
  
  dev.off() # end recording of graph
  
  # check for right data format: specifying factors in the formula might work, but is unstable. Therefore, ask to specify the data type outside the analyse() function
  formula_character = as.character(formula)
  
  if (any(grepl("as\\.", formula_character) == TRUE)){
    stop("\nYour formula contains 'as.' (such as as.factor) indicating that you change the data class in the formula of 'analyse'.\nThis causes unstable behaviour of the plotting module.\nPlease change the data class outside the 'analyse' - function." )
  }
  
  # number of data points per variable --> calculated in reporting script
  
  # skewness of data
  
  if (verbose){
    cat(list$data_na.omitted_number_text) # print out to console
    cat("finished data checking successfully!\n")
  }
  return(list)
}