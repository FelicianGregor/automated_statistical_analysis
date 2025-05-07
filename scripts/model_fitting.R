build_model = function(list, verbose = TRUE){
  
  library(VGAM)
  
  #build model
  
  list$model = vglm(list$formula, family = list$dist, data = list$data_na.omit, model = TRUE) 
  
  #store model summary: result
  list$model_summary = summary(list$model)
  list$model_significance = list$model_summary@coef3[,4] #"Pr(>|t|)" / "Pr(>|z|) --> significant?
  list$model_coefficients_link_scale = coef(list$model)
  
  if (verbose){
    cat("model fitted!\n")
  }
  return(list)
}