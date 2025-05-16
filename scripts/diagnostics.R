diagnose = function(list, verbose = TRUE){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##### DHARMa based diagnostics #####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  cat("entered model diagnostics\n")
  source("./scripts/helper_functions.R") #load function from other scripts
  list$misc$all_integer_response = all(ds4psy::is_wholenumber(list$data_na.omit[,1])) #apply function to every cell in col and check if every cell is integer
  # create DHARMa object: first simulating y from model & predicting y from model
  list$diagn_DHARMa$sim = createDHARMa(simulatedResponse = as.matrix(simulate.vlm(list$model, nsim = 1000)), 
                                       observedResponse = as.vector(list$model@y), 
                                       fittedPredictedResponse = as.vector(predictvglm(list$model, type = "response")), 
                                       integerResponse = list$misc$all_integer_response)
  
  if (verbose){cat("simulating DHARMa residuals finished\n")}
  
  ### perform the DHARMa tests for: outlier, uniformity, dispersion
  
  #"outlier" detection using DHARMa testOutliers
  list$diagn_DHARMa$outlier_test = testOutliers(list$diagn_DHARMa$sim, type = "default", plot = FALSE) # uses as default: binomial (outlier test significant if more than 1/(nSim +1) outliers detected) if continuous dist. bootstrap outlier estimation for nObs<500, but because runtime high: if nObs>500: tweak ?testOutlier end of 4th para in details section: closer than 1/(nSim+1) to the 0/1 border, which roughly occur at a rate of nData /(nSim +1). 
  #list$diagn_DHARMa$outlier_test$statistic # get the outlier number (two sided, at both margins)
  #list$diagn_DHARMa$outlier_test$estimate # expected and real outlier frequency
  #list$diagn_DHARMa$outlier_test$p.value # significance level 
  if (verbose){cat("testOutliers() finished\n")}

  # test for dispersion using DHARMa testDispersion
  list$diagn_DHARMa$dispersion_test = testDispersion(list$diagn_DHARMa$sim, plot = FALSE) # interpretation see ?testDispersion: over: significant and dispersion > 1, under if <1 and significant
  #list$diagn_DHARMa$dispersion_test$statistic # access dispersion value
  #list$diagn_DHARMa$dispersion_test$p.value # access significance
  if (verbose){cat("testDispersion() finished\n")}
  
  #test distribution using DHARMa testUniformity
  list$diagn_DHARMa$uniformity_test = testUniformity(list$diagn_DHARMa$sim) # gives p value and D stats (max distance from expected dist) p value problem, combine somehow with D as effect size --> no threshold in Dormann --> find one
  #list$diagn_DHARMa$uniformity_test$statistic # D value
  #list$diagn_DHARMa$uniformity_test$p.value # p value
  if (verbose){cat("testUniformity() finished\n")}
  
  # plot the residuals and perform a test for the quantiles: quantiles computed and spline (non parametric regression) fitted, looking for deviations from 0.25, 0.5 and 0.75 quantile line
  list$diagn_DHARMa$quantile_test = testQuantiles(list$diagn_DHARMa$sim) # just gives a p value, which is misleading in case a lot of data is there...
  #list$diagn_DHARMa$quantile_test$p.value # get combined (!) p.value
  if (verbose){cat("testQuantiles() finished\n")}
  
  # make plot on dharma object: creates QQ plot and quantile residuals plot, adding all the tests from above
  png("./output/plots/DHARMa_summary_plot.png", width = 10, height = 5, units = "in", res = 400)
  plot(list$diagn_DHARMa$sim)
  dev.off()
  if (verbose){cat("DHARMa plot saved\n")}
  
  #### some problems do occur with continuos predictors when performing testQuantiles with cont predictor (see plot...)
  ### also: please clean up the code and make it a bit more readable, less redundant
  ### and error: The `value` argument of `names<-()` can't be empty as of tibble 3.0.0. and must contain the same length as x
  
  # make the quantile plots for every predictor, additionally adding the name of the predictor
  list$misc$pred_names_formula = list[["data_variables"]][2:length(list[["data_variables"]])] #get the predictor variables name without e.g. as.factor()
  list$misc$pred_names_data_classes = attr(terms(list$model), "dataClasses")[2:length(attr(terms(list$model), "dataClasses"))] # names with as.factor()
  list$diagn_DHARMa$quantile_test_per_pred = list()
  
  #small tweak: assign with names() the as.a factor version to the real data and use it
  list$data_na.omit_as_fac_included = list$data_na.omit
  names(list$data_na.omit_as_fac_included) =  names(list$misc$pred_names_data_classes)
  
  for (i in names(list$misc$pred_names_data_classes)) {
    # check if categorical or continuous predictor to use proper test function (testQuantile is somehow producing a plot with cat predictor, but no output, seems to be a bug)
    png(paste0("./output/plots/quantile_plot_", i, ".png"), width = 5, height = 5, units = "in", res = 400)
    list$diagn_DHARMa$quantile_test_per_pred[[i]] <- if (list$misc$pred_names_data_classes[i] == "factor") {
      testCategorical(list$diagn_DHARMa$sim, catPred = list$data_na.omit_as_fac_included[[i]])
    } else {
      testQuantiles(list$diagn_DHARMa$sim, predictor = list$list$data_na.omit_as_fac_included[[i]])
    }
    par(xpd = NA) # deactivate border, enable drawing in border
    rect(-1, -1, 11.5, -0.18, col = "white", border = NA) # draw white rectangle on top of xlab
    title(xlab = if ((list$misc$pred_names_data_classes[[i]]) == "factor") paste("categorical predictor", i) else paste("predictor", i, "(rank transformed)")) #add predictor name to label
    par(xpd = FALSE) # set border settings back to default = FALSE
    dev.off() #stop recording png file
  }
  
  if (verbose){
    cat("testQuantiles per predictor finished\n")
    cat("DHARMa residual tests finished\n")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #check for collinearity problems:
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #can just occur if # preds > 1:
  if (ncol(list$model@model[2:ncol(list$model@model)])>1){
    #criterion Dormann 2017: abs value of correlations need to be below 0.7 (0.5-0.7, but I don't want to be that conservative)
    corr_mat_kendalls = polycor::hetcor(list$model@model[2:ncol(list$model@model)], 
               use="pairwise.complete.obs", method = "kendall")
  
    #extract pred pairs with thao larger than threshold:
    corr_mat_threshold = 0.7
    #set diagonal to NA (since cormat is mirrored)
    diag(corr_mat_kendalls$correlations) = NA
    corr_mat_kendalls$correlations[lower.tri(corr_mat_kendalls$correlations)] <- NA
  
    #save index in corr_mat_kendalls
    index = which(abs(corr_mat_kendalls$correlations) > corr_mat_threshold, arr.ind = T)
  
    #save the corr values as well as the respective pred vars
    values = round(as.numeric(corr_mat_kendalls$correlations[index]), 2) # round numbers 
    pred2 = row.names(as.data.frame(corr_mat_kendalls$correlations))[index[, 1]]
    pred1 = names(as.data.frame(corr_mat_kendalls$correlations))[index[, 2]]
  
    # write table
    corr_critical_res_table = data.frame("pred1" = pred1, 
                              "pred2" = pred2, 
                              "Tau" = values)
  
    # get number of corr values higher than abs (0.7)
    corr_number_critical_tau = nrow(corr_critical_res_table)
    
    if (verbose){cat("correlations finished!\n")}
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # variance inflation factor VIF
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # criterion Dormann 2013: problem with collinearity for VIF>10
  # cant be computed since the function vif() does not work for vglm(), manually would be time consuming to implement for now, especially with categorical variables (manually make dummy variables)
  
  if (ncol(list$model@model[2:ncol(list$model@model)])>1){
    terms = names(model.frame(list$model)[2:ncol(model.frame(list$model))])
    response = names(model.frame(list$model)[1])
    
    #initialize storage vecs
    r2 = rep(NA, length(terms))
    VIF = rep(NA, length(terms))
    
    for (i in 1:length(terms)){
    
      #get other preds
      other_preds = setdiff(terms, terms[i])
      
      #create formula for model with terms[i] as response, other vars as preds
      formula = paste0(terms[i], "~", paste(other_preds, collapse = "+")) 
      
      #fit model
      lin_model = lm(formula, data = as.data.frame(list$model@model))
    
      #extract r2
      r2[i] = summary(lin_model)$r.squared
    
      #formula for VIF
      VIF[i] = 1 / (1 - r2[i])
    }
  
    #store results with var names
    res = data.frame(VIF)
    
    # set threshold to VIF = 10, above = critical, following criterion from Dormann 2013
    VIF_threshold = 10
    VIF_values  = data.frame(terms = terms, res)
    VIF_critical_terms = VIF_values[which(res> VIF_threshold),]
    
    
    #save whole Collinearity stuff (corr and VIF) to list:
  
  list$diagnostics = list( # first the corr results
                          corr_critical_res_table = corr_critical_res_table, 
                          corr_mat_kendalls = corr_mat_kendalls, 
                          corr_mat_threshold = corr_mat_threshold, 
                          corr_number_critical_tau = corr_number_critical_tau, 
                          
                          #now the VIF results
                          VIF_threshold = VIF_threshold, 
                          VIF_values = VIF_values, 
                          VIF_critical_terms= VIF_critical_terms)
  
  }
  return(list)
}  
  ### to Do DHARMA:
  # add calculation of residuals per predictor - Done
  # add residuals plot 
  # research on criterion for deviance from uniformity in testUniformity: threshold for D and p value
  # research: how is dispersion computed for testDispersion? use criterion by Hartig in help or Dormann?
  
  # change the way how residuals are calculated
  # - refitting = FALSe (default), which is right, since I dont have a mixed effects model or so
  # - calculate also the residuals per group: recalculate residuals(grouping variable)
  # - transformation for other dist: no, uniform is great!
  # - integer response: I do check this already --> needed? Yes, since I will have to possibility to use lots of dists from vglm!
  # 
  
