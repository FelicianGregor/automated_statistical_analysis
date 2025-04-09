#### build system skeleton ####
# use s3 classes for using generic plot functions etc. (--> later?)
# store everything in a long list
# do one branch for prediction, one branch for hyp. testing and one branch for data exploration

# - include possibility for multivariate models - DONE
# - reporting with quarto document
# - include other families as well --> works with all glm families (distribution)
# - effect size!
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
    #store model summary: result
    list$model_summary = summary(list$model)
    cat("model fitted!\n")
    list$model_significance = list$model_summary$coefficients[2,"Pr(>|t|)"]
    list$model_text = ifelse(list$model_significance<0.05, 
                             "significant effect detected\n", 
                             "no significant effect detected\n")
    cat(list$model_text)
    
    
    #### model diagnostics####
    
    #### prep-simulations for DHARMa diagnostics
    list$diagn_DHARMa_sim = DHARMa::simulateResiduals(list$model, n = 1000)
    cat("simulating DHARMa residuals finished\n")
    
    ### "outlier" detection using DHARMa testOutliers
    list$diagn_DHARMa_outlier = DHARMa::testOutliers(list$diagn_DHARMa_sim, n = 1000)
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
    list$diagn_DHARMa_dispersion =  DHARMa::testDispersion(list$diagn_DHARMa_sim)
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
    list$diagn_DHARMa_residuals_uniform = DHARMa::testUniformity(list$diagn_DHARMa_sim)
    list$diagn_DHARMa_residuals_uniform_result = ifelse(list$diagn_DHARMa_residuals_uniform$p.value < 0.05, 
                                                        "DHARMa detected deviations from residual uniformity", 
                                                        "DHARMa did not detect suspicious deviances")
    cat("DHARMa residual test finished\n")
    
    #### plotting####
    #define plot function that creates scatter plot and adds a simple abline (although not being the most elegant way)
    
    if (any(grep("numeric", attr(list$model_summary$terms, "dataClasses")))>0){
      #ggpredict(list$model, terms = list$model$terms) %>% plot()
      list$plotting_function = function(){
        plot(formula, data = list$data_na.omit, las = 1)
        abline(list$model, col = "purple", las = 2)
      }
    } else{
      list$plotting_function = function(){
        plot(formula, data = list$data_na.omit, las = 1)
        #abline(list$model, lwd = 2, col = "purple")
      }
      
    }
    list$plotting_function()
    cat("plot done\n")
    
  }
  if (mode == "predict"){
    
  }
  if (mode== "explore"){
    
  }
  
  ##### reporting ####
  
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

plot(animal_weight~age, data = knz_bison, las = 1, 
     main = "relation of bisons age and weight", 
     xlab = "age [years]", 
     ylab = "weight [pounds]")

#i don't differentiate between sex here... --> (i specify a bad model)

### test function
results = system_function(animal_weight~age, data = knz_bison, mode = "test", dist = "gaussian")
knz_bison$animal_weight
#well, there is a certain relationship, but the model diagnostics don't really look great
