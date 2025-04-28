#### build system skeleton ####

#packages used
# DHARMa
# quarto
# VGAM -> system is based on the vglm() function
# for test data sets: lterdatasampler

system_function = function(formula, data, mode, dist = "uninormal"){
  #! comment on the input!!!
  #create the long storage list
  #create the long storage list
  list = list()
  
  #get data from formula
  list$data_variables = all.vars(formula)
  list$raw_data = data[list$data_variables] # subset data to only the data that was used
  
  #store all the other variables as well
  list$data_all_including_unused = data
  
  #store other input information
  list$mode = mode
  list$dist = dist
  #currently restricted to distributions supported by glm(): ?family (glm.nb from MASS to be implemented later)
  
  ##### data checking 
  list$data_na.omit = na.omit(list$raw_data) #delete all rows with NA values
  list$data_na.omitted_number = length(list$raw_data) - length(list$data_na.omit)
  list$data_na.omitted_number_text = paste(list$data_na.omitted_number, "NA's omitted\n")
  cat(list$data_na.omitted_number_text) # print out to console
  
  #check the mode and enter the hypothesis test mode if mode = "test" == TRUE
  #if (mode == "test"){
  
  #### build model ####
  #based on specified hypothesis
  library(VGAM)
  list$model = vglm(formula, data = list$data_na.omit, family = list$dist)
  # "gaussian", "binomial", "Gamma", "inverse.gaussian", "poisson", "quasibinomial", "quasipoisson", "quasi"
  
  #store model summary: result
  list$model_summary = summary(list$model)
  cat("model fitted!\n")
  list$model_significance = list$model_summary@coef3[,4] #"Pr(>|t|)" / "Pr(>|z|)"
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
  
  #effect sizes: Is the effect actually large?
  #following Nakagawa and Cuthill 2007 (review article on effect sizes in ecology): report slope and CI, as well as r statistics (correlations, partial corerelations)
  # pay attention to link and response scale: backtransform
  
  # get inverse link function for doing the transformation
  
  list$model_coefficients_link_scale = list$model@coefficients
  #standard error back transform??
  
  #correlations / partial correlations? (to adjust for other variables influence)
  
  #calculate the prediction for every stupid x variable / level...? 
  #Or provide just a short explanation how to do the calculation?
  
  
  
  
  
  
  #### model diagnostics####
  #normal model diagnostics:
  #compute dispersion in case the dispersion is taken to be one and not estimated from data (as for gaussian, Gamma)
  # follow Dormann 2017: dispersion = residual deviance / residual degrees of freedom
  
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
  
  ## follow Dormann 2017 for criterion on over/under dispersion
  
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
  
  #create DHARMa object
  # attention! simulated object needs to be a matrix
  # since normally, DHARMa does not support vglm objects: simulate 1000 data points for each observed point from the model using simulate()
  # diferentiate between integer response and non-integer response as asked for in createDHARMa help text
  #### prep-simulations for DHARMa diagnostics
  list$all_integers = function(response_value){ #define function to check every cell integer or not
    return(floor(response_value) %% response_value == 0)
  }
  all_integers = all(sapply(list$data_na.omit[,1], list$all_integers)) #sapply to apply function to every cell in col and check if every cell is int with all
  
  if (all_integers){
    cat("value is an integer. createDHARMa uses integerResponse  = T\n")
    list$diagn_DHARMa_sim_residuals = DHARMa::createDHARMa(simulatedResponse = as.matrix(simulate(list$model, nsim = 1000)), 
                                                           observedResponse = as.vector(list$model@y), 
                                                           integerResponse = F, 
                                                           fittedPredictedResponse = as.vector(VGAM::predictvglm(list$model, type = "response")))
  }else{
    cat("this is not an integer! createDHARMA uses integerResponse = F\n")
    list$diagn_DHARMa_sim_residuals = DHARMa::createDHARMa(simulatedResponse = as.matrix(simulate(list$model, nsim = 1000)), 
                                                           observedResponse = as.vector(list$model@y), 
                                                           integerResponse = F, 
                                                           fittedPredictedResponse = as.vector(VGAM::predictvglm(list$model, type = "response")))
  }
  cat("simulating DHARMa residuals finished\n")
  
  #plot(list$diagn_DHARMa_sim_residuals)
  # does not work properly, very sad
  DHARMa::testResiduals(list$diagn_DHARMa_sim_residuals)
  DHARMa::testOutliers(list$diagn_DHARMa_sim_residuals)
  DHARMa::testDispersion(list$diagn_DHARMa_sim_residuals)
  
  ### "outlier" detection using DHARMa testOutliers
  png('./output/plots/diagnostics_DHARMa_outliers.png', width = 6, height = 4, res = 300, units = "in")
  list$diagn_DHARMa_outlier = DHARMa::testOutliers(list$diagn_DHARMa_sim, n = 1000)
  dev.off() 
  list$diagn_DHARMa_outlier_result = ifelse(list$diagn_DHARMa_outlier$p.value < 0.05, 
                                            "**DHARMa outlier test detected significant outliers.**",
                                            "DHARMa outlier test did not detect any outliers.")
  cat("DHARMa outlier test finished\n")
  
  #### continue with DHARMa model diagnostics: ####
  
  #### DHARMa dispersion test
  # save plot to ".output/plots" as pdf for later incorporating graph in the report
  png('./output/plots/diagnostics_DHARMa_dispersion.png', width = 6, height = 4, res = 300, units = "in")
  list$diagn_DHARMa_dispersion =  DHARMa::testDispersion(list$diagn_DHARMa_sim)
  dev.off()
  list$diagn_DHARMa_dispersion_result = ifelse(list$diagn_DHARMa_dispersion$p.value < 0.05, 
                                               "**DHARMa detected over / underdispersion.**", 
                                               "DHARMa did not detect dispersion issues.") 
  #if disp$p.value < 0.05 --> dispersion test significant --> over / under dispersion
  cat("DHARMa dispersion test finished\n")
  
  ### DHARMa residual test: test for uniformal distribution of simulated residuals
  #simulates residuals from the model, that should be equally distributed around the model
  # compares the quantiles of modeled scaled quantile residuals and quantiles of uniform distribution: modeled should follow uniform distribution, too --> straight line
  # ks. test tests if they are from the same distribution (so uniform)
  # Ha: don't come from the same dist, H0: come from the same dist
  # if H0 p < 0.05, H0 is rejected and we assume, Ha is true, so they dot come from same distribution
  png('./output/plots/diagnostics_DHARMa_uniform.png', width = 6, units = "in", height = 4, res = 300)
  list$diagn_DHARMa_residuals_uniform = DHARMa::testUniformity(list$diagn_DHARMa_sim)
  dev.off()
  list$diagn_DHARMa_residuals_uniform_result = ifelse(list$diagn_DHARMa_residuals_uniform$p.value < 0.05, 
                                                      "**DHARMa detected deviations from residual uniformity.**", 
                                                      "DHARMa did not detect suspicious deviances.")
  cat("DHARMa residual test finished\n")
  
  #### plotting####
  
  #start the "recording" using png (before setting the par)
  png('./output/plots/plot.png', width = 8, units = "in", height = 5, res = 300)
  
  #set par(mfrow = c()) to 2 cols and right number of rows depending on data
  list$plot_rows_needed = ceiling(((ncol(list$data_na.omit)-1)/2))
  if ((ncol(list$data_na.omit)-1) < 1){
    par(mfrow = c(1,1))
  } else{
    #adjust some par for more space efficient plotting when having more than one rows
    par(mfrow = c(list$plot_rows_needed, 2))
    par(mar = c(4.5, 4, 0.5, 0.5))
    par(tcl = 0.5) # axis ticks inside the window
    par(mgp = c(2.5, 0.5, 0)) # move axis title etc. closer
    par(las = 1) #turn ticks
  }
  plot(formula, data = data, las = 1, ask = F, cex = 0.7)
  dev.off()
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) # set par() back
  cat("plot done\n")
  
  
  
  
  ##### reporting #####
  #create a long list params for quarto document
  
  # input:
  params_report = list()
  params_report$mode = ifelse(list$mode=="test", "hypothesis test", list$mode)
  params_report$dist = list$dist
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
`r params$formula`

You assumed a `r params$dist` distribution.
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
  
  return(list)
}

#try out with some data:
data = knz_bison
knz_bison$age = knz_bison$rec_year - knz_bison$animal_yob

system_function(formula = animal_weight ~ age * as.factor(animal_sex), data = knz_bison, mode = "test", dist = "uninormal")

#### test the function on some data ####
library(lterdatasampler) # data freely available, credits to https://lter.github.io/lterdatasampler/reference/and_vertebrates.html

#load data
data("hbr_maples")
#inspect briefly
head(hbr_maples) # I'm interested in the relationship of watershed (treatment with calcium or not) and stem_dry_mass (representing productivity)
summary(hbr_maples)
# also, elevation could have a strong influence on growth, so include the variable and possible interactions
# assumed relation: stem_dry_mass ~ watershed + elevation
# stem_dry_mass: continuous
# watershed: categorical: treated (W1), not treated reference (Reference)
# year: numeric --> make factor
# elevation: Low and Mid as levels 

#test the function
test = system_function(stem_dry_mass ~ watershed * as.factor(year), data = hbr_maples, mode = "test", dist = "gaussian")
# better reporting of DHARMa results!
# explanation of the model summary printed
plot(stem_dry_mass ~ watershed * elevation * year, data = hbr_maples, las = 1, alpha = 0.8, ask = F)


# another test:
#install.packages("AER")
library(AER)
data("NMES1988")
test2 = system_function(visits ~ health + age + gender + married + income + insurance,
                        data = NMES1988, dist = "poisson", mode = "test")

###### to do ######
# - 2 effect sizes! / or better short introduction on how to read the summary(glm)-output? 
# - 3 add effects in plots (lots of work in case i need to write the code by myself...) --> ggeffects::ggpredict()?
# - 1 make DHARMa diagnostics better! - add residuals plot!
# - 4 include automatic model adjustment just with deleted cooks D values (as a first simple check if model adjustments work)?
# - 5 cooks distance issues
# - 6 have errors due to poly(x, 2) in formula when plotting in mind
# - 7 write understandable code for dispersion and deviance stuff
# - 8 create own dedicated function for each step, then call them via source....
# - 9 add more information on input data! => comprehensive data checking and report the findings
# - 10 computation of effects (coefficients) in vglm on response scale did not work...
# - check if folder structure exists in directory and if not, create the structure