# wrapper function: analyse ###

analyse = function(formula, data, mode = "test", dist = "uninormal", verbose = TRUE){
  
  #create the long storage list
  list = list() # use the same list throughout the whole analysis to store output 
  
  #divide the different modes
  if (mode == "test"){
    
    #prepare data
    source("data_preparation.R")
    list = prepare_data(formula, data, mode, list = list, dist = dist, verbose = TRUE)
    
    # build model
    source("model_fitting.R")
    list = build_model(list, verbose = TRUE)
    
    #model diagnostics
    source("diagnostics.R")
    list = diagnose(list, verbose = TRUE)
    
    #### plotting####
    source("plotting.R")
    list = plotting(list, verbose = TRUE)
    
    # reporting
    source("reporting.R")
    report(list, verbose = TRUE)
    
  } else if (mode == "predict"){
    
    #prepare data
    source("data_preparation.R")
    list = prepare_data(formula, data, mode, list = list, dist = dist, verbose = TRUE)
    
    # build model
    source("model_fitting.R")
    list = build_model(list, verbose = TRUE)
    
    #model diagnostics
    source("diagnostics.R")
    #list = diagnose(list, verbose = TRUE)
    
    #### plotting####
    source("plotting.R")
    list = plotting(list, verbose = TRUE)
    
    # reporting
    source("reporting.R")
    report(list, verbose = TRUE)
    
  } else if (mode == "explore"){
    
  } else {
    cat("please specify a valid mode of either `test` or `predict`")
  }
  
  return(list)
}