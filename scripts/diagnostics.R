diagnose = function(list, verbose = TRUE){
  ##### DHARMa based diagnostics #####
  cat("entered model diagnostics\n")
  source("./scripts/helper_functions.R") #load function from other scripts
  list$misc$all_integer_response = all(sapply(list$data_na.omit[,1], all_integers)) #sapply to apply function to every cell in col and check if every cell is int with all
  
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
  
  # make plot on dharma object: cretaes QQ plot and quantile residuals plot, adding all the tests from above
  png("./output/plots/DHARMa_summary_plot.png", width = 10, height = 5, units = "in", res = 400)
  plot(list$diagn_DHARMa$sim)
  dev.off()
  if (verbose){cat("DHARMa plot saved\n")}
  
  # make the quantile plots for every predictor, additionally adding the name of the predictor
  list$misc$pred_names_formula = test[["data_variables"]][2:length(test[["data_variables"]])] #get the predictor variables name without e.g. as.factor()
  print(list$misc$pred_names_formula)
  list$diagn_DHARMa$quantile_test_per_pred = list()
  
  source("./scripts/helper_functions.R") #load helper function to determine whether data needs to get treated in DHARMa as categorical
  
  for (i in list$misc$pred_names_formula) {
    # check if categorical or continuous predictor to use proper test function (testQuantile is somehow producing a plot with cat predictor, but no output, seems to be a bug)
    png(paste0("./output/plots/quantile_plot_", i, ".png"), width = 5, height = 5, units = "in", res = 400)
    list$diagn_DHARMa$quantile_test_per_pred[[i]] <- if (is_fac(list$model)) {
      testCategorical(list$diagn_DHARMa$sim, catPred = as.factor(list$data_na.omit[[i]])) 
    } else {
      testQuantiles(list$diagn_DHARMa$sim, predictor = list$data_na.omit[[i]])
    }
    par(xpd = NA) # deactivate border, enable drawing in border
    rect(-1, -1, 11.5, -0.18, col = "white", border = NA) # draw white rectangle on top of xlab
    title(xlab = if (is_cat(list$data_na.omit[[i]])) paste("categorical predictor", i) else paste("predictor", i, "(rank transformed)")) #add predictor name to label
    par(xpd = FALSE) # set border settings back to default = FALSE
    dev.off() #stop recording png file
  }
  
  if (verbose){
    cat("testQuantiles per predictor finished\n")
    cat("DHARMa residual test finished\n")
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
  # - calculate also the residuals per group: recalculateresiduals(groupingvariable)
  # - transformation for other dist: no, uniform is great!
  # - integer response: I do check this already --> needed? Yes, since I will have to possibility to use lots of dists from vglm!
  # 
  
  # include: 
  # - all tests! 
  # --> outlier test result (report on number of outliers and expected frequency, real frequency)
  # --> uniformity test result: D value and p.value (add criterion, if I find one)
  # --> QQ plot
  # --> dispersion test result: dispersion value and p value
  # --> results from testQuantile and testCategorical (p values combined)
  # --> testQuantile plots (per predictor)
