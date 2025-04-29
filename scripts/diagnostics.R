diagnose = function(list, verbose = TRUE){
  
  #### conventional model diagnostics ####
  #dispersion
  if (list$model_summary@dispersion==1){
    #this might break....
    if (is.null(list$model@criterion$deviance)){
      list$diagn_dispersion_text = "We are probably having a problem with determining the deviance in vglm models! Deviance is not defined for most vglm models. See Yee (2015): Vector Generalized Linear and Additive Models. p. 117."
      cat("probably having a problem with determining the deviance in vglm models! Deviance is not defined for most vglm models. See Yee (2015): Vector Generalized Linear and Additive Models. p. 117.")
    }else if (list$model_summary@dispersion==1){
      list$diagn_dispersion_value = round((list$model@criterion$deviance) / (list$model_summary@df.residual), 2)
      list$diagn_dispersion_text = paste("The dispersion value (defined as residual deviance devided by residual degrees of freedom) is", 
                                         list$diagn_dispersion_value, ".")}
  } else{
    list$diagn_dispersion_text = paste0("Since the model uses a ", list$dist, 
                                        " distribution, ", "the dispersion parameter as residual deviance is estimated from data to be ", 
                                        list$model_summary@dispersion, ".")
  }
  
  #follow Dormann 2017 for criterion on over/under dispersion
  # in case dispersion parameter is estimated from data and 1:
  if (is.null(list$model@criterion$deviance)){
    list$diagn_dispersion_conclusion = "Therefore, please use the DHARMa based diagnostics for potential over or under dispersion issues."
  } else if(list$model_summary@dispersion!=1){
    list$diagn_dispersion_conclusion = "Therefore, there can not appear dispersion issues."
  }
  
  # if dispersion > 2: over dispersion, if dispersion < 0.6: under dispersed, other: dispersion not an issue
  if (is.null(list$model@criterion$deviance)){
    list$diagn_dispersion_conclusion = "Therefore, please use the DHARMa based diagnostics for potential over or under dispersion issues."
  } else if(is.numeric(list$diagn_dispersion_value)==TRUE){
    if (list$diagn_dispersion_value > 2){
      list$diagn_dispersion_conclusion = "Since the value is greater than 2, we detected over dispersion issues."
    }
    else if (list$diagn_dispersion_value < 0.6){
      list$diagn_dispersion_conclusion = "Since the values is smaller than 0.6, we detected under dispersion issues"
    }
    else{
      list$diagn_dispersion_conclusion = "Since the dispersion value lies between 0.6 and 2, no dispersion issues were detected."
    }
  }
  
  ##### DHARMa based diagnostics #####
  source("./scripts/helper_functions.R") #load function from other scripts
  all_integers = all(sapply(list$data_na.omit[,1], all_integers)) #sapply to apply function to every cell in col and check if every cell is int with all
  
  #do the prep simulations and create DHARMa object
  if (all_integers){
    cat("value is an integer. createDHARMa uses integerResponse  = T\n")
    list$diagn_DHARMa_sim_residuals = DHARMa::createDHARMa(simulatedResponse = as.matrix(simulate(list$model, nsim = 1000)), 
                                                           observedResponse = as.vector(list$model@y), 
                                                           integerResponse = T, 
                                                           fittedPredictedResponse = as.vector(VGAM::predictvglm(list$model, type = "response")))
  }else{
    cat("this is not an integer! createDHARMA uses integerResponse = F\n")
    list$diagn_DHARMa_sim_residuals = DHARMa::createDHARMa(simulatedResponse = as.matrix(simulate(list$model, nsim = 1000)), 
                                                           observedResponse = as.vector(list$model@y), 
                                                           integerResponse = F, 
                                                           fittedPredictedResponse = as.vector(VGAM::predictvglm(list$model, type = "response")))
  }
  
  if (verbose){
    cat("simulating DHARMa residuals finished\n")
  }
  
  #perform the DHARMa tests for: outlier, dispersion
  
  #"outlier" detection using DHARMa testOutliers
  png('./output/plots/diagnostics_DHARMa_outliers.png', width = 6, height = 4, res = 300, units = "in")
  list$diagn_DHARMa_outlier = DHARMa::testOutliers(list$diagn_DHARMa_sim, n = 1000)
  dev.off() 
  list$diagn_DHARMa_outlier_result = ifelse(list$diagn_DHARMa_outlier$p.value < 0.05, 
                                            "**DHARMa outlier test detected significant outliers.**",
                                            "DHARMa outlier test did not detect any outliers.")
  if (verbose){
    cat("DHARMa outlier test finished\n")
  }
  
  #continue with DHARMa model diagnostics
  
  #### DHARMa dispersion test
  # save plot to ".output/plots" as pdf for later incorporating graph in the report
  png('./output/plots/diagnostics_DHARMa_dispersion.png', width = 6, height = 4, res = 300, units = "in")
  list$diagn_DHARMa_dispersion =  DHARMa::testDispersion(list$diagn_DHARMa_sim)
  dev.off()
  list$diagn_DHARMa_dispersion_result = ifelse(list$diagn_DHARMa_dispersion$p.value < 0.05, 
                                               "**DHARMa detected over / underdispersion.**", 
                                               "DHARMa did not detect dispersion issues.") 
  #if disp$p.value < 0.05 --> dispersion test significant --> over / under dispersion
  if (verbose){
    cat("DHARMa dispersion test finished\n")
  }
  
  ### DHARMa residual test: test for uniformal distribution of simulated residuals
  #simulates residuals from the model, that should be equally distributed around the model
  # compares the quantiles of modeled scaled quantile residuals and quantiles of uniform distribution: modeled should follow uniform distribution, too --> straight line
  # ks. test tests if they are from the same distribution (so uniform)
  # Ha: don't come from the same dist, H0: come from the same dist
  # if H0 p < 0.05, H0 is rejected and we assume, Ha is true, so they dot come from same distribution
  png('./output/plots/diagnostics_DHARMa_uniform.png', width = 6, units = "in", height = 6, res = 300)
  list$diagn_DHARMa_residuals_uniform = DHARMa::testUniformity(list$diagn_DHARMa_sim)
  dev.off()
  list$diagn_DHARMa_residuals_uniform_result = ifelse(list$diagn_DHARMa_residuals_uniform$p.value < 0.05, 
                                                      "**DHARMa detected deviations from residual uniformity.**", 
                                                      "DHARMa did not detect suspicious deviances.")
  if (verbose){
    cat("DHARMa residual test finished\n")
  }
  return(list)
}