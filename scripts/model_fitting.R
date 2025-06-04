build_model = function(list, verbose = TRUE){
  
  library(VGAM)
  
  #build model
  list$model = vglm(list$formula, family = list$dist, data = list$data_na.omit, model = TRUE) 
  
  #store model summary: result
  list$model_summary = summary(list$model)
  
  ### Bonferroni correction ### Quinn 2002: multiply the P-values with number of family-wise hypothesis.
  # Defining every predictor variable leading to a parameter estimate as hypothesis (H0: slope = 0)
  # thus, for correcting p Values, one needs to multiply with the number of estimated parameters in the model
  
  intercept_locs = which(grepl("Intercept", rownames(list$model_summary@coef3))) # get intercept locations in the dataframe to exclude them in next line
  p_values_uncorr = list$model_summary@coef3[,4][-c(intercept_locs)] # exclude the intercept:1 (location) and intercept:2 scale parameter 
  p_values_corrected = p_values_uncorr * length(p_values_uncorr) # Bonferroni correction
  p_values_corrected = ifelse(p_values_corrected<1, p_values_corrected, 1) # restrict values to 1 (although they are no probabilities but solely decision criteria)
  
  # detect significant values if < 0.05
  significance = sapply(p_values_corrected, FUN = function(x)ifelse(x < 0.05, yes = "*", no = ""))
  
  ### slopes ### for reporting, following Nakagawa 2007
  # get inverse function for transformation from link to response scale
  linkinv = list$model@family@linkinv
  
  # access slope and backtransform (if link scale != response scale)
  slope_link = list$model_summary@coef3[,1][-c(intercept_locs)] 
  slope_response = linkinv(as.data.frame(slope_link), extra = list$model@extra)
  
  ### CI's ### for reporting, following Nakagawa 2007
  # calculate CI on link scale
  upper_CI_link = slope_link + 1.96*list$model_summary@coef3[,2][-c(intercept_locs)]
  lower_CI_link = slope_link - 1.96*list$model_summary@coef3[,2][-c(intercept_locs)]
  
  # calculate CI on response scale
  upper_CI_response = linkinv(as.data.frame(upper_CI_link), extra = list$model@extra)
  lower_CI_response = linkinv(as.data.frame(lower_CI_link), extra = list$model@extra)
  
  # make it pretty and store
  res = data.frame(significance, slope_link, upper_CI_link, lower_CI_link, upper_CI_response, lower_CI_response) # put values together into a data.frame
  var_names = rownames(res) # store rownames to reassign them later 
  res = as.data.frame(apply(res[, -1], MARGIN = 2, FUN = function(x) round(as.numeric(x), 2)), significance) #round data to 2 decimal places
  rownames(res) = var_names #reassign the rownames
  list$model_slopes_CI_significance = data.frame(res, significance)
  
  
  if (verbose){
    cat("model fitted!\n")
  }
  
  
  # variable importance: SHAP values ####
  
  if (ncol(list$model@model)>2){
    if (verbose){
    cat("starting shapley values\n")
    }
    
    #make data ready for computing shapley values
    x_data = as.data.frame(list$model@model[,-1]) # exclude y 
    names(x_data) = names(model.framevlm(list$model))[-1]
  
    y_data = as.data.frame(scale(list$model@model[, 1])) # get just y and scale it....?
    
    
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
    tibble::as_tibble(explained_model$shapley_values)
    
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