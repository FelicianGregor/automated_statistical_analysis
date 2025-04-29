report = function(list, verbose = T){
  library(stringr) # for removing pattern ("\n\n") from family character
  
  #explaining text for p values
  list$model_significance_text = "If the p-value of the variable is smaller than 0.05 (denoted with one to three stars in the model summary below) 
                    this indicates that the null hypothesis (H0) should be rejected and thus, your alternative hypothesis (H1 / Ha) 
                    is assumed to hold until more data becomes available and could reject the null hypothesis.
                    Below, the full model summary is provided for further details:"
  
  #check quickly if there are enough data points for the number of predictor variables:
  #Dormann 2017 criterion: 5-10 data points per independent variable
  list$number_data_points_per_var = (nrow(list$data_na.omit)) / (ncol(list$model@x) - 1)
  
  if(list$number_data_points_per_var<5){
    list$number_data_points_per_var_text = paste("You have only ", list$number_data_points_per_var, 
                                                 " data points per independent variable in your model. 
                                                 Due to the small number of 5 - 10 data points per variable, 
                                                 issues of strong over fittin are likely Please either simplify the model 
                                                 by removing independent variables or collect more data.")
  } else if(list$number_data_points_per_var<=10){
    list$number_data_points_per_var_text = paste("You have only ", list$number_data_points_per_var, 
                                                 " data points per independent variable in your model. 
                                                 This less than  10 data points per variable and can 
                                                 cause overfitting. Please either simplify the model 
                                                 by removing independent variables or collect more data.")
  }else{
    list$number_data_points_per_var_text = paste("Your model contains ", list$number_data_points_per_var,  " data points per independent variable which is sufficient for your model to avoid over fitting.")
  }
  
  
  
  ##### reporting #####
  #create a long list params for quarto document
  
  # input:
  params_report = list()
  params_report$mode = ifelse(list$mode=="test", "hypothesis test", list$mode)
  params_report$dist = stringr::str_remove(list$model@family@blurb[1], "\n\n")
  params_report$formula = paste(list$model@terms)
  # paste(names(list$data_na.omit)[1], " ~ ", paste(attr(list$model$terms, "term.labels"), collapse = " + "))
  # data checking:
  params_report$na_omitted_number = list$data_na.omitted_number
  params_report$number_data_points_per_var_text = list$number_data_points_per_var_text
  
  # model fitting:
  params_report$model_significance = capture.output(list$model_significance)
  params_report$model_significance_text = list$model_significance_text
  params_report$summary = capture.output(summary(list$model))
  
  # model diagnostics
  #params_report$diagn_dispersion_value = list$diagn_dispersion_value
  params_report$diagn_dispersion_text = list$diagn_dispersion_text
  params_report$diagn_dispersion_conclusion = list$diagn_dispersion_conclusion
  params_report$diagn_DHARMa_residuals_uniform_result = list$diagn_DHARMa_residuals_uniform_result
  params_report$diagn_DHARMa_dispersion_result = list$diagn_DHARMa_dispersion_result
  params_report$diagn_DHARMa_outlier_result = list$diagn_DHARMa_outlier_result
  
  params_report$diagn_cooks_result = list$diagn_cooks_result
  
  #create quarto document for reporting
  cat('---
title: "report automated statistical analysis"
format: html
editor: visual
params:
  dist: "distribution failed"
  p.value: 0.03
  mode: "test failed"
  formula: "y~x failed"
  summary: "summary failed"
  na_omitted_number: NA
  diagn_dispersion_text: "text failed"
  diagn_dispersion_conclusion: "conclusion failed"
  diagn_cooks_result: "computing Cook`s distance failed"
  number_data_points_per_var_text: "this text was not displayed correctly"
  model_significance: "significance failed"
  model_significance_text: "significant text failed"
  diagn_DHARMa_residuals_uniform_result: "uniform result failed"
  diagn_DHARMa_dispersion_result: "dispersion text failed"
  diagn_DHARMa_outlier_result: "outlier test text failed"
---

## input for `r params$mode`

You fitted a generalized linear model (`glm()`-function in R) for `r params$mode` assuming the following relationship:
``r params$formula``

You assumed a `r params$dist`.
Please be aware that you chose the mode "`r params$mode`" of the automated statistician that differs from the two other available modes.

## data preparation
We detected `r params$na_omitted_number` NA values that were deleted.
`r params$number_data_points_per_var_text`


## result of `r params$mode`
Here, the significance values (p-values) for each independent variable are printed:
```{r}
#| echo: false
cat(params$model_significance, sep = "\n")

```
`r params$model_significance_text`

```{r}
#| echo: false
cat(params$summary, sep = "\n")

```
Please have a look at the plots. Note: Interaction terms (denoted as predictor1:predictor2 in the formula) can not be displayed.

![Plot of all independent variables and the response variable](../plots/plot.png){width=70% fig-align="center"}

## model diagnostics
Please check the model doagnostics carefully to make sure the inferences made are valid!
For model diagnostics, we use a "conventional" approach and simulation based approach for scaled (quantile) residuals from the `DHARMa`-package side by side. The latter scaled quantile residuals can be interpreted intuitively in the same way as residuals from linear regression models. For more information please read [the introduction by Florian Hartig](https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html).

### conventional model diagnostics
`r params$diagn_dispersion_text`
`r params$diagn_dispersion_conclusion`

`r params$diagn_cooks_result`


### DHARMa-based model diagnostics
DHARMa simulates a specified number of values for the dependent variable y given the defined model for each independent x value. The simulated values are sorted in ascending order and the quantile is determined for the observed y value, thus the values range from 0 to 1. In case the observed value is smaller or larger than every single simulated value (quantile 0 or 1) the value is considered as outlier. Please note that the probability of detecting outliers depends on the number of simulated values, making outliers more likely when increasing the number of simulations. We selected n = 1000.
For your model the 
`r params$diagn_DHARMa_outlier_result`

![DHARMa outlier test](../plots/diagnostics_DHARMa_outliers.png){width=70% fig-align="center"}

The DHARMa dispersion test computes the mean variance of the data given the specified model var(observed - predicted) and the variances of the simulated data using a histogram (computing the variance var(simulated - predicted) for every x value), thus displaying the distribution of the simulated variance. It performs a test for significant deviation of the real from the expected (simulated) variance and returns a p value. If p < 0.05, the variance differes significantly. In the plot the value of the observed mean residual variance is marked by a red line.
For the specified model `r params$diagn_DHARMa_dispersion_result`

![DHARMa dispersion test](../plots/diagnostics_DHARMa_dispersion.png){width=70% fig-align="center"}

The DHARMa uniformity test compares the distribution of the modeled/expected residuals (simulated from the fitted model) with a uniform distribution using a KS test (two sided as default) and returns a p value. If the p value is smaller than 0.05, the distribution of the simulated data is significantly different from the the expected uniformal distribution.
A QQ-plot is displayed. 
For the model specified `r params$diagn_DHARMa_residuals_uniform_result`

![DHARMa uniformity test](../plots/diagnostics_DHARMa_uniform.png){width=70% fig-align="center"}

## conclusion


\
', file = './output/reports/test.qmd')
  
  #render the reporting document:
  quarto::quarto_render('./output/reports/test.qmd', 
                        execute_params = params_report)
  
  
  
  
  
  
  if (verbose){cat("report successfully written.")}
  return(list)
}
