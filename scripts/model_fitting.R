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
    explained_model <- explain(list$model, feature_names = NULL,  X=x_data, pred_wrapper=pred_vglm, adjust=T, nsim=50, shap_only = FALSE) # shap_inly = TRUE --> just shap values returned
    
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