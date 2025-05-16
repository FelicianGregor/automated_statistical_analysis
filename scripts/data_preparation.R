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
  list$data_na.omitted_number = length(list$raw_data) - length(list$data_na.omit)
  list$data_na.omitted_number_text = paste(list$data_na.omitted_number, "NA's omitted\n")
  
  # check the data for strong correlations between predictor variables 
  
  # number of data points per variable
  
  # skewness of data
  
  if (verbose){
    cat(list$data_na.omitted_number_text) # print out to console
    cat("finished data checking successfully!\n")
  }
  return(list)
}