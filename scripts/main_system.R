#### build system skeleton ####
# use s3 classes for using generic plot functions etc. (--> later?)
# store everything in a long list
# do one branch for prediction, one branch for hyp. testing and one branch for data exploration

# - include possibility for multivariate models - DONE
# - reporting with quarto document
# - include other families as well --> works with all glm families (distribution)
# - effect sizes!
# - include possibility to give formula - DONE


system_function = function(formula,  data, mode, dist = "gaussian"){
  
  #create the long storage list
  list = list()
  
  #get data from formula
  list$data_variables = all.vars(formula)
  list$raw_data = data[list$data_variables] # subset data to just data that was used
  
  #store all the other variables as well
  list$data_all_including_unused = data

  #store other input information
  list$mode = mode
  list$dist = dist
  #currently restricted to distributions supported by glm(): ?family
  
  #data checking
  list$data_na.omit = na.omit(list$raw_data) #delete all rows with NA values
  list$data_na.omitted_number = length(list$raw_data) - length(list$data_na.omit)
  list$data_na.omitted_number_text = paste(list$data_na.omitted_number, "NA's omitted\n")
  cat(list$data_na.omitted_number_text) # print out to console :D
  
  #check the mode and enter the hypothesis test mode if mode = "test" == TRUE
  if (mode == "test"){
    
    #model fit, based on specified hypothesis
    list$model = glm(formula, data = list$data_na.omit, family = list$dist)
    # "gaussian", "binomial", "Gamma", "inverse.gaussian", "poisson", "quasibinomial", "quasipoisson", "quasi"
    
    #store model summary: result
    list$model_summary = summary(list$model)
    cat("model fitted!\n")
    list$model_significance = list$model_summary$coefficients[2,"Pr(>|t|)"]
    list$model_text = ifelse(list$model_significance<0.05, 
                             "significant effect detected\n", 
                             "no significant effect detected\n")
    cat(list$model_text)
    
    #effect sizes: Is the effect actually large?
    #following Nakagawa and Cuthill 2007 (review article on effect sizes): report slope and CI, as well as r statistics (correlations, partial corerelations)
    # pay attention to link and response scale: backtransform
    list$model$coefficients_response_scale = list$model$family$linkinv(list$model$coefficients)
    #standard error back transform??
    
    #correlations / partial correlations? (to adjust for other variables influence)
    
    #calculate the prediction for every stupid variable...?
    
    
    
    
    #### model diagnostics####
    #normal model diagnostics:
    #compute dispersion in case the dispersion is taken to be one and not estimated from data (as for gaussian, Gamma)
    # follow Dormann 2017: dispersion = residual deviance / residual degrees of freedom
    if (list$model_summary$dispersion==1){
      list$diagn_dispersion_value = (list$model_summary$deviance) / (list$model_summary$df.residual)
      list$diagn_dispersion_text = paste("The dispersion value (defined as residual deviance devided by residual degrees of freedom) is", 
                                               list$diagn_dispersion_value, ".")
    } else{
      list$diagn_dispersion_text = paste0("Since the model uses a ", list$dist, 
                                              "distribution, ", "the dispersion parameter is estimated from data to be ", 
                                              list$model_summary$dispersion, ".")
    }
    print(list$diagn_dispersion_text)
    
    ## follow Dormann 2017 for criterion on over/under dispersion
    
    # in case dispersion parameter is estimated from data and 1:
    if (grep("Since the model uses a", list$diagn_dispersion_text)){
      list$diagn_dispersion_conclusion = "Therefore, there can not appear dispersion issues."
    }
    # if dispersion > 2: over dispersion, if dispersion < 0.6: under dispersed, other: dispersion not an issue
    if (is.numeric(list$diagn_dispersion_value)==TRUE){
      if (list$diagn_dispersion > 2){
        list$diagn_dispersion_conclusion = "Since the value is greater than 2, we detected over dispersion issues."
      }
      if (list$diagn_dispersion_value < 0.6){
        list$diagn_dispersion_conclusion = "Since the values is smaller than 0.6, we detected under dispersion issues"
      }
      else{
        list$diagn_dispersion_conclusion = "Since the dispersion value lies between 0.6 and 2, no disperion issues were detected."
      }
    }
    
    #### prep-simulations for DHARMa diagnostics
    list$diagn_DHARMa_sim = DHARMa::simulateResiduals(list$model, n = 1000)
    cat("simulating DHARMa residuals finished\n")
    
    ### "outlier" detection using DHARMa testOutliers
    png('./output/plots/diagnostics_DHARMa_outliers.png', width = 6, height = 4, res = 300, units = "in")
    list$diagn_DHARMa_outlier = DHARMa::testOutliers(list$diagn_DHARMa_sim, n = 1000)
    dev.off() 
    list$diagn_DHARMa_outlier_result = ifelse(list$diagn_DHARMa_outlier$p.value < 0.05, 
                                              "DHARMa outlier test detected significant outliers",
                                              "DHARMa outlier test did not detect any outliers")
    cat("DHARMa outlier test finished\n")
    
    #### "outliers" - using standard cook's distance D
    list$diagn_influence_table = influence.measures(list$model) #compute other influence measures
    list$diagn_cooks = cooks.distance(list$model) #compute cooks distance
    #implement rule from Dormann 2017 (influential data points (n) have either D > 1 or D > 4/n)
    list$diagn_cooks_critical_values = which(list$diagn_cooks>1| list$diagn_cooks>(4/nrow(list$data_na.omit)))
    list$diagn_cooks_result = ifelse(length(list$diagn_cooks_critical_values)==0, 
                                     "great! no critical cooks distance (D) values detected", 
                                     paste("we detected", length(list$diagn_cooks_critical_values), "critical cooks distance values"))
    cat("computing cook's distance finished\n")
    
    #### continue with DHARMa: 
    #### DHARMa dispersion test
    # save plot to ".output/plots" as pdf for later incorporating graph in the report
    png('./output/plots/diagnostics_DHARMa_dispersion.png', width = 6, height = 4, res = 300, units = "in")
    list$diagn_DHARMa_dispersion =  DHARMa::testDispersion(list$diagn_DHARMa_sim)
    dev.off()
    list$diagn_DHARMa_dispersion_result = ifelse(list$diagn_DHARMa_dispersion$p.value < 0.05, 
                                                 "DHARMa detected over / underdispersion", 
                                                 "DHARMa did not detect dispersion issues") 
    #if disp$p.value < 0.05 --> dispersion test significant --> over / under dispersion
    cat("DHARMa dispersion test finished\n")
    
    ### DHARMa resudual test: test for uniformal distribution of simulated residuals
    #simulates residuals from the model, that should be equally distributed around the model
    # compares the modeled/expected residuals and observed residuals: modeled follow uniform distribution, observed should, too
    # ks. test tests if they are from the same distribution (so uniform)
    # Ha: don't come from the same dist, H0: come from the same dist
    # if H0 p < 0.05, H0 is rejected and we assume, Ha is true, so they dot come from same distribution
    png('./output/plots/diagnostics_DHARMa_uniform.png', width = 6, units = "in", height = 4, res = 300)
    list$diagn_DHARMa_residuals_uniform = DHARMa::testUniformity(list$diagn_DHARMa_sim)
    dev.off()
    list$diagn_DHARMa_residuals_uniform_result = ifelse(list$diagn_DHARMa_residuals_uniform$p.value < 0.05, 
                                                        "DHARMa detected deviations from residual uniformity", 
                                                        "DHARMa did not detect suspicious deviances")
    cat("DHARMa residual test finished\n")
    
    #### plotting####
    #define plot function that creates scatter plot and adds a simple abline (although not being the most elegant way)
    
    if (any(grep("numeric", attr(list$model_summary$terms, "dataClasses")))>0){
      # attention! plotting ois done on the link scale! (in case "gaussian" ,link scale = response scale)
      list$plotting_function = function(){
        #plot(formula, data = list$data_na.omit, las = 1)
        #abline(list$model, col = "purple", las = 2)
      }
    } else{
      list$plotting_function = function(){
        #plot(formula, data = list$data_na.omit, las = 1)
        #abline(list$model, lwd = 2, col = "purple")
      }
      
    }
    list$plotting_function()
    cat("plot done\n")
  
    
  }
  if (mode == "prediction"){
    
  }
  if (mode== "exploration"){
    
  }
  
  ##### reporting ####
  #create a long list params for quarto document
  
  # input:
  params_report = list()
  params_report$mode = ifelse(list$mode=="test", "hypothesis test", list$mode)
  params_report$dist = list$dist
  params_report$formula = paste(names(list$model$model)[1], " ~ ", paste(attr(list$model$terms, "term.labels"), collapse = " + "))
  
  # data checking:
  params_report$na_omitted_number = list$data_na.omitted_number
    
  # model fitting:
  params_report$p.value = list$model_summary$coefficients[2,"Pr(>|t|)"]
  params_report$summary = capture.output(summary(list$model))
  
  # model diagnostics
  params_report$diagn_dispersion_value = list$diagn_dispersion_value
  params_report$diagn_dispersion_text = list$diagn_dispersion_text
  params_report$diagn_dispersion_conclusion = list$diagn_dispersion_conclusion
  
  #create quarto document for reporting
  cat('---
title: "report automated statistical analysis"
format: html
editor: visual
params:
  dist: "gaussian"
  p.value: 0.03
  mode: "test"
  formula: "y~x"
  summary: "summary"
  na_omitted_number: NA
  diagn_dispersion_text: "text"
  diagn_dispersion_conclusion: "conclusion"
---

## input for `r params$mode`

You fitted a generalized linear model (`glm()`-function in R) for `r params$mode` assuming the following relationship:
`r params$formula`

You assumed a `r params$dist` distribution.
Please be aware that you chose the mode "`r params$mode`" of the automated statistician that differs from the two other available modes.

## data preparation
We detected `r params$na_omitted_number` NA values that were deleted.

## result of `r params$mode`

The p-value of `r params$p.value` indicates that your H0 can be rejected and thus, your Ha is assumed to hold until more data becomes available and rejects the hypothesis.
In the section below, please find the summary of the model you fitted:

```{r}
#| echo: false
cat(params$summary, sep = "\n")

```
## model diagnostics
Please check the model doagnostics carefully to make sure the inferences made are valid!
For model diagnostics, we use a "conventional" approach and simulation based approach for scaled (quantile) residuals from the `DHARMa`-package that side by side. The latter scaled quantile residuals can be interpreted intuitively in the same way as residuals from linear regression models. For more information please read [the introduction by Florian Hartig](https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html).

### conventional model diagnostics
`r params$diagn_dispersion_text`
`r params$diagn_dispersion_conclusion`


### DHARMa-based model diagnostics
The DHARMa outlier test:
![DHARMa outlier test](../plots/diagnostics_DHARMa_outliers.png){width=70% fig-align="center"}

The DHARMa dispersion test:
![DHARMa dispersion test](../plots/diagnostics_DHARMa_dispersion.png){width=70% fig-align="center"}

The DHARMa uniformity test compares the distribution of the modeled/expected residuals(simulated from the fitted model) with a uniform distribution using a KS test (two sided as default) and returns a p value. If the p value is smaller than 0.05, the distribution of the simulated data significantly different from the the expected uniformal distribution.
![DHARMa uniformity test](../plots/diagnostics_DHARMa_uniform.png){width=70% fig-align="center"}


\
', file = './output/reports/test.qmd')
  
  #render the reporting document:
  quarto::quarto_render('./output/reports/test.qmd', 
                        execute_params = params_report)
  
  
  #return list
  return(list)
}

#### test the function on some data ####
library(lterdatasampler) # data freely available, credits to https://lter.github.io/lterdatasampler/reference/and_vertebrates.html

#load data
data(knz_bison)
#inspect briefly
head(knz_bison) # I'm interested in the relationship of age and weight in pounds
#compute rough age of Bisons (I just have the birth and date when their weight was measured)
knz_bison$age = (knz_bison$rec_year - knz_bison$animal_yob) + (30*(knz_bison$rec_month)+knz_bison$rec_day)/365

plot(animal_weight~age+animal_sex, data = knz_bison, las = 1, 
     main = "relation of bisons age and weight", 
     xlab = "age [years]", 
     ylab = "weight [pounds]")

### test function
results = system_function(animal_weight~poly(age, 2)+animal_sex, data = knz_bison, mode = "test", dist = "gaussian")

#well, there is a certain relationship, but the model diagnostics don't really look great
