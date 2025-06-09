build_model = function(list, verbose = TRUE){
  
  library(VGAM)
  
  #build model
  list$model = vglm(list$formula, family = list$dist, data = list$data_na.omit, model = FALSE) 
  
  #store model summary: result
  list$model_summary = summary(list$model)
  
  ### Bonferroni correction ### Quinn 2002: multiply the P-values with number of family-wise hypothesis.
  # Defining every predictor variable leading to a parameter estimate as hypothesis (H0: slope = 0)
  # thus, for correcting p Values, one needs to multiply with the number of estimated parameters in the model
  
  intercept_locs = which(grepl("Intercept", rownames(list$model_summary@coef3))) # get intercept locations in the dataframe to exclude them in next line
  p_values_uncorr = list$model_summary@coef3[,4][-c(intercept_locs)] # exclude the intercept:1 (location) and intercept:2 scale parameter 
  intercept_cols = which(grepl("Intercept", colnames(model.matrix(list$model))))
  print("intercept cols")
  print(intercept_cols)
  print("ncol(model.matrix())")
  print(ncol(model.matrix(list$model)))
  print("without intercepts")
  print(ncol(model.matrix(list$model)[, -c(intercept_cols)]))
  n_parameters = ncol(model.matrix(list$model)[, -c(intercept_cols)]) #number of estimated parameters 
  list$misc$n_parameters_beta = n_parameters # save information
  p_values_corrected = p_values_uncorr *  n_parameters # Bonferroni correction, multiply with number of parameters
  p_values_corrected = ifelse(p_values_corrected<1, p_values_corrected, 1) # restrict values to 1 (although they are no probabilities but solely decision criteria)
  
  # detect significant values if < 0.05
  significance = sapply(p_values_corrected, FUN = function(x)ifelse(x < 0.05, yes = "*", no = ""))
  
  ### slopes ### for reporting, following Nakagawa 2007
  # get inverse function for transformation from link to response scale
  linkinv = list$model@family@linkinv
  
  # access slope and backtransform (if link scale != response scale)
  slope_link = list$model_summary@coef3[,1][-c(intercept_locs)] 
  slope_link = round(slope_link, 2)
  
  ### CI's ### for reporting, following Nakagawa 2007
  # calculate CI on link scale
  upper_CI_link = slope_link + 1.96*list$model_summary@coef3[,2][-c(intercept_locs)]
  lower_CI_link = slope_link - 1.96*list$model_summary@coef3[,2][-c(intercept_locs)]
  
  #round 2 decimal places
  upper_CI_link = round(upper_CI_link, 2)
  lower_CI_link = round(lower_CI_link, 2)
  
  # make it pretty and store
  res = data.frame(slope_link, upper_CI_link, lower_CI_link, significance) # put values together into a data.frame
  list$model_slopes_CI_significance = res
  
  
  if (verbose){
    cat("model fitted!\n")
  }
  
  
  # variable importance: SHAP values ####
  vars = all.vars(list$model@misc$formula)[-1] # get all vars
  vars_number = length(vars) # get number of vars
  if (vars_number >1){
    if (verbose){
    cat("starting shapley values\n")
    }
    
    #make data ready for computing shapley values
    x_data = list$data_na.omit[,c(vars)] # get origial variable names and data, rescale it
    y_data = as.data.frame(scale(list$data_na.omit[, responseName(list$model)])) # get just y and scale it?
    
    #prediction wrapper specifying how to obtain the predicted values from vglm objects
    # differentiate whether one or two variables are fitted and thus given back by the predict function of vglm. Explain needs just preds of mean
    if (length(list$model@misc$predictors.names)==1){
      pred_vglm <- function(object, newdata, ...){
        predictvglm(object, newdata=newdata)
      }
    }else{
      pred_vglm <- function(object, newdata, ...){
        predictvglm(object, newdata=newdata)[,1]
      }
    }
    
    #compute values
    explained_model <- fastshap::explain(list$model, feature_names = NULL,  X=x_data, pred_wrapper=pred_vglm, adjust=T, nsim=50, shap_only = FALSE) # shap_inly = TRUE --> just shap values returned
    
    # make a tibble out of the dataframe...
    (tibble::as_tibble(explained_model$shapley_values))
    
    #visualize:
    #make a shapviz object out of the fastshap for using it in the visualizations
    shv.global <- shapviz(explained_model, interactions = TRUE)
    
    # get the mean of the absolute values 
    mean_abs_shap_values = as.data.frame(apply(shv.global$S, 2, function(x) mean(abs(x))))
    highest_shap_max_three = names(sort(colMeans(abs(shv.global$S)), decreasing = T))[1:3]
  
    # ordered barplot:
    shapley_barplot = sv_importance(shv.global, kind = "bar", show_numbers = T) + ggtitle(label = "mean absolute shapley values")
    ggsave(filename = "shapley_values_barplot.png", plot = shapley_barplot, path = './output/plots/',
          scale = 1, width = 3.5, height = 3.5, units = "in",
          dpi = 300, limitsize = TRUE)
    
    # more complicated visualization
    sv_importance(shv.global, show_numbers = T, kind = "beeswarm") + theme_minimal() + ggtitle(label = "mean absolute shapley values & feature values")
    
    # store everything in a list feeding into the large list
    list$shapley_values = list(shap_raw_values = explained_model, 
                               mean_abs_shap_values = mean_abs_shap_values, 
                               highest_shap_max_three = highest_shap_max_three)
  
    if (verbose){
      cat("shapley values finished\n")
    
    
    } else{
      if (verbose){
        cat("just one predictor variable - no shapley values computed\n")
      }
  }

  
  }
  return(list)
}