# wrapper function: analyse ###

analyse = function(formula, data, mode = "test", dist = "uninormal", verbose = TRUE){
  
  #create the long storage list
  list = list() # use the same list throughout the whole analysis to store output 
  
  #divide the different modes
  if (mode == "test"){
    
    #prepare data
    source("./scripts/data_preparation.R")
    list = prepare_data(formula, data, mode, list = list, dist = dist, verbose = TRUE)
    
    # build model
    source("./scripts/model_fitting.R")
    list = build_model(list, verbose = TRUE)
    
    #model diagnostics
    source("./scripts/diagnostics.R")
    list = diagnose(list, verbose = TRUE)
    
    #### plotting####
    source("./scripts/plotting.R")
    list = plotting(list, verbose = TRUE)
    
    # reporting
    source("./scripts/reporting.R")
    report(list, verbose = TRUE)
    
  } else if (mode == "predict"){
    
    #prepare data
    source("./scripts/data_preparation.R")
    list = prepare_data(formula, data, mode, list = list, dist = dist, verbose = TRUE)
    
    # build model
    source("./scripts/model_fitting.R")
    list = build_model(list, verbose = TRUE)
    
    #model diagnostics
    source("./scripts/diagnostics.R")
    #list = diagnose(list, verbose = TRUE)
    
    #### plotting####
    source("./scripts/plotting.R")
    list = plotting(list, verbose = TRUE)
    
    # reporting
    source("./scripts/reporting.R")
    report(list, verbose = TRUE)
    
  } else if (mode == "explore"){
    
  } else {
    cat("please specify a valid mode of either `test` or `predict`")
  }
  
  return(list)
}