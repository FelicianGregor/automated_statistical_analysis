report = function(list, verbose = T){
  if (verbose){cat("entered reporting\n")}
  
  library(stringr) # for removing pattern ("\n\n") from family character
  source("./scripts/helper_functions.R") #load header() add() and new_line() function
  #create reporting part in list:
  list$reporting = list()
  
  ##### input and data preparation ####
  list$reporting$input_data_prep$mode = list$mode
  list$reporting$input_data_prep$input_formula = paste(list$model@terms)
  list$reporting$input_data_prep$model_sentence = paste0("You fitted a generalized linear model (`vglm()`-function in R) assuming the following relationship: ", 
                                                         "`", list$reporting$input_data_prep$input_formula, "`")
  #save assumed distribution
  list$misc$distribution_name = stringr::str_remove(list$model@family@blurb[1], "\n\n")
  list$reporting$input_data_prep$dist_sentence = paste0("You assumed a ",list$misc$distribution_name , ". ")
  
  list$reporting$input_data_prep$NA_sentence = paste0("We detected ", list$data_na.omitted_number, " NA values that were deleted. ")
  
  num_preds = length(rownames(list$model_summary@coef3)[-grep("Intercept", rownames(list$model_summary@coef3))]) # access the model summary, where all the estimated parameters are listed, excluding Intercepts
  list$reporting$input_data_prep$number_data_points_per_var = (nrow(list$data_na.omit)) / (num_preds)
  
  list$reporting$input_data_prep$variable_number_sentence = paste0("Your model contains ", list$reporting$input_data_prep$number_data_points_per_var, 
                                                                   " data points per independent variable, ")
  
  # apply condition from dormann 2017: 5-10 data points per variable can cause issues of overfitting 
  list$reporting$input_data_prep$variable_number_issues_sentence = if (list$reporting$input_data_prep$number_data_points_per_var < 5){
    "which is less than 5 and therefore can cause serious problems due to overfitting with too little data points per predictor variable. "
  } else if (list$reporting$input_data_prep$number_data_points_per_var <= 10){
    "which is between 5 and 10 and therefore could cause problems due to overfitting with too little data points per predictor variable. "
  } else if (list$reporting$input_data_prep$number_data_points_per_var > 10){
    "which are enough observations per predictor variable to avoid overfitting issues of the model. "
  }
  
  # collinearity issues in case abs(corr) > 0.7 (Dormann 2017 / 2013):
  vars = all.vars(list$model@misc$formula)[-1] # get names of vars, exclude y
  vars_number = length(vars) # number of vars
  
  if (vars_number>1){ # just in case there are more than one predictor variables
    if (list$diagnostics$corr_number_critical_tau > 0){
    list$reporting$input_data_prep$corr_issues_sentence = paste("The following predictors are highly correlated suggesting issues with collinearity among the independent variables. This can lead to variance inflation (high standard errors) of your parameter estimates. Below, please find the critical predictors pairs and their respective correlation values (Kendall's Tau). ")
    list$reporting$input_data_prep$corr_table = '```{r}
#| echo: false
list = readRDS("list_reporting.RDS")
print(list$diagnostics$corr_critical_res_table)

```'
    } else {
    list$reporting$input_data_prep$corr_issues_sentence = "We did not detect high correlation values (tau > 0.7) among the predictor variables. "
    }
  }
  
  # collinearity issues in case VIF > 10 (Dormann 2017 / 2013)
  if (vars_number>1){ # just in case there are more than one predictor variables
    if (nrow(list$diagnostics$VIF_critical_terms) > 0){
    list$reporting$input_data_prep$VIF_issues_sentence = paste("The following predictors have variance inflation factors (VIF) higher than 10 suggesting issues with collinearity among the predictors. This can lead to variance inflation (high standard errors) of your parameter estimates. Below, please find the predictors and their respective VIF values. ")
    list$reporting$input_data_prep$VIF_table = '```{r}
#| echo: false
list = readRDS("list_reporting.RDS")
print(list$diagnostics$VIF_critical_terms)

```'
    } else {
    list$reporting$input_data_prep$VIF_issues_sentence = "There are no high variance inflation factors (VIF > 10) among the predictor variables. "
    }
  }
  
  # if both corr and VIF are not critical: print a sentance that nothing critical was found
  if (vars_number>1){ # just in case there are more than one predictor variable
    if (nrow(list$diagnostics$VIF_critical_terms) == 0 & list$diagnostics$corr_number_critical_tau == 0){
      list$reporting$input_data_prep$collinearity_issues_sentence = "This suggests that the model does not have any problems with collinearity.  "
    }
  }
  
  ##### model result ####
  list$reporting$model_results$intro = "In the table you find the parameter estimates or slopes of the GLM along with the upper and lower 95% confidence intervals (CI's) on the link scale with their significance denoted by an asterisk."
  list$reporting$model_results$significance_output = capture.output(list$model_slopes_CI_significance)
  
  list$reporting$model_results$model_significance_explanation = "The null hypothesis (H0) that the slope is zero is tested and P values < 0.05 (Bonferroni corrected) are considered significant. Note, that every estimated model parameter is here defined as one hypothesis. In case of a significant parameter estimate, the associated H0 is rejected concluding that the research hypothesis (H1) and the theory is supported by the given data. In case of a non significant parameter estimate, H0 cannot be rejected but is retained and we conclude that there is no support for either H1 and the theory or H0.
                    Below, the full model summary is provided for further details:"
  
  list$reporting$model_results$summary_output = capture.output(list$model_summary)
  
  num_preds = length(rownames(list$model_summary@coef3)[-grep("Intercept", rownames(list$model_summary@coef3))]) #exclude intercepts
  list$reporting$model_results$plots_and_text = if (vars_number>3){
    "Your model contains more than three predictor variables. Therefore, we used only the three predictor variables with highest variable importance (mean absolute shapley value) for producing the graphics. "
  } else{
    'Please have a look at the plots below.\n![plot](../plots/plot.png){width=100% fig-align="center"}'
  }
  
  #add plot for variable importance: shapley values:
  if (vars_number>1){
    list$reporting$model_results$shapley_plot_and_text = 'For determining the importance of specific independent variables on the predictions of your model, shapley values were computed and are visualized in the barplot below.\n![shapley values](../plots/shapley_values_barplot.png){width=50% height=50% fig-align="center"} '
  }
  
  
  ##### model diagnostics ####
  list$reporting$diagnostics$intro_DHARMa_text = paste('Please check the model diagnostics carefully to make sure the inferences made are valid!',
                                                       'For model diagnostics a simulation based approach with scaled (quantile) residuals from the `DHARMa`-package is used. These residuals can be interpreted intuitively in the same way as residuals from linear regression models. For more information please read [the introduction by Florian Hartig](https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html).')
  list$reporting$diagnostics$outlier_text = ifelse(list$diagn_DHARMa$outlier_test$p.value < 0.05, 
                                            paste("**DHARMa detected significantly more ouliers than usual!** "),
                                            "The DHARMa outlier test did not detect an unusual high number of outliers. ")
  list$reporting$diagnostics$dispersion_text = ifelse(list$diagn_DHARMa$dispersion_test$p.value < 0.05, 
                                                      "**DHARMa detected over / underdispersion issues.** ", 
                                                      "DHARMa did not detect dispersion issues. ") 
  list$reporting$diagnostics$uniformity_text = ifelse(list$diagn_DHARMa$uniformity_test$p.value < 0.05, 
                                                      "**DHARMa detected significant deviations from residual uniformity.** ", 
                                                      "DHARMa did not detect suspicious deviations from residual uniformity. ")
  list$reporting$diagnostics$quantiles_text = ifelse(list$diagn_DHARMa$quantile_test$p.value < 0.05, 
                                                    "**DHARMa detected significant quantile deviations from the expected values.** ", 
                                                    "DHARMa did not find suspicious deviations of the quantiles from th expected values. ")
  
  ##### conclusion ####
  
  # save list to output! --> then load it in quarto and use it for printing:
  saveRDS(list, "output/reports/list_reporting.RDS")


  ### create params list for R-like output in chunk (model summary, especially) - to get replaced by html table?
  params = list()
  params$model_results_output = list$reporting$model_results$summary_output
  params$significance_output = list$reporting$model_results$significance_output
  ### dynamically write text:
  source("./scripts/helper_functions.R") #load header() add() and new_line() function
  
  #write document step by step and render at the end
  cat(header(), file = "./output/reports/test_report.qmd") # create .qmd document
  
  ### input para
  add("## input data")
  new_line()
  new_line()
  add(list$reporting$input_data_prep$model_sentence)
  new_line()
  add(list$reporting$input_data_prep$dist_sentence)
  add(list$reporting$input_data_prep$NA_sentence)
  add(list$reporting$input_data_prep$variable_number_sentence)
  new_line()
  add(list$reporting$input_data_prep$variable_number_issues_sentence)
  new_line()
  new_line()
  if (vars_number>1){
    add(list$reporting$input_data_prep$corr_issues_sentence)
  new_line()
    if (list$diagnostics$corr_number_critical_tau>0){
      add(list$reporting$input_data_prep$corr_table)
      new_line()
    }
  }
  if (vars_number>1){
    add(list$reporting$input_data_prep$VIF_issues_sentence)
    new_line()
    if (nrow(list$diagnostics$VIF_critical_terms)>0){
      add(list$reporting$input_data_prep$VIF_table)
      new_line()
    }
  }
  if (vars_number>1){ # just in case there are more than one predictor variable
    if (nrow(list$diagnostics$VIF_critical_terms) == 0 & list$diagnostics$corr_number_critical_tau == 0){
      add(list$reporting$input_data_prep$collinearity_issues_sentence)
      new_line()
    }
  }
  
  
  
  #model results
  new_line()
  add("## results")
  new_line()
  new_line()
  add(list$reporting$model_results$intro)
  new_line()
  add('```{r}
#| echo: false
cat(params$significance_output, sep = "\n")

```')
  new_line()
  add(list$reporting$model_results$model_significance_explanation)
  new_line()
  add('```{r}
#| echo: false
cat(params$model_results_output, sep = "\n")

```')
  new_line()
  add(list$reporting$model_results$plots_and_text)
  new_line()
  new_line()
  if (num_preds>1){
    add(list$reporting$model_results$shapley_plot_and_text)
    new_line()
    new_line()
  }
  
  ### model diagnostics
  add("##  model diagnostics")
  new_line()
  new_line()
  add(list$reporting$diagnostics$intro_DHARMa_text)
  new_line()
  new_line()
  add(list$reporting$diagnostics$outlier_text)
  new_line()
  new_line()
  add(list$reporting$diagnostics$dispersion_text)
  new_line()
  new_line()
  add(list$reporting$diagnostics$uniformity_text)
  new_line()
  new_line()
  add(list$reporting$diagnostics$quantiles_text)
  new_line()
  new_line()
  add('![DHARMa summary plot](../plots/DHARMa_summary_plot.png){width=100% fig-align="center"}')
  new_line()
  
  #render
  quarto::quarto_render("./output/reports/test_report.qmd", execute_params = params)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  if (verbose){cat("report successfully written.")}
  return(list)
}
