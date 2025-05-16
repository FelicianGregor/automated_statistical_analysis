#### build system skeleton ####
# but with functions - replace every major step with a function....

# packages used
# DHARMa
# stringr: for removing a pattern from a string to report distribution (reporting.R)
# quarto
# VGAM -> system is based on the vglm() function
# for test data sets: lterdatasampler


system_function = function(formula, data, mode, dist = "uninormal", verbose = TRUE){

  # create a folder structure for saving the results, if not existing:
  if (dir.exists("./automated_statistical_analysis")){
    
    #print message if directories are created
    message("The Automated Statistician creates a folder structure for saving results in your working directory.")
    
    dir.create("./automated_statistical_analysis")
    dir.create("./automated_statistical_analysis/output")
    dir.create("./automated_statistical_analysis/output/plots")
    dir.create("./automated_statistical_analysis/output/report")
    dir.create("./automated_statistical_analysis/output/tables")
  }
  
  #create the long storage list
  #create the long storage list
  list = list() # use the same list throughout the whole analysis?
  
  #devide the different modes
  if (mode == "test"){
    
    #prepare data
    source("./scripts/data_preparation.R")
    list = prepare_data(formula, data, mode, list = list, dist = dist, verbose = TRUE)
    
    # build model
    source("./scripts/model_fitting.R")
    list = build_model(list, verbose = TRUE)
    
    #model diagnostics
    source("./scripts/diagnostics.R")
    list = diagnose(list, verbose = TRUE)
    
    #### plotting####
    source("./scripts/plotting.R")
    list = plotting(list, verbose = TRUE)
    
    # reporting
    source("./scripts/reporting.R")
    report(list, verbose = TRUE)

  } else if (mode == "predict"){
    
    #prepare data
    source("./scripts/data_preparation.R")
    list = prepare_data(formula, data, mode, list = list, dist = dist, verbose = TRUE)
    
    # build model
    source("./scripts/model_fitting.R")
    list = build_model(list, verbose = TRUE)
    
    #model diagnostics
    source("./scripts/diagnostics.R")
    #list = diagnose(list, verbose = TRUE)
    
    #### plotting####
    source("./scripts/plotting.R")
    list = plotting(list, verbose = TRUE)
    
    # reporting
    source("./scripts/reporting.R")
    report(list, verbose = TRUE)
    
  } else if (mode == "explore"){
    
  } else {
    cat("please specify a valid mode of either `test`, `predict` or `explore`")
  }
  
  return(list)
}

#try out with some data:
library(lterdatasampler)
library(DHARMa)
library(VGAM)
library(ds4psy) # for is_wholenumber()
library(polycor) # for hetcor() --> continuous, polychoric / polyserial correlations
library(shapviz) # for shapley values (in script model_fitting)
library(ggplot2) # for visualizing shapley values (script model_fitting)
library(ggthemes) # for making shapley values pretty (script model_fitting)
data = knz_bison
knz_bison$rec_month = as.factor(data$rec_month)
knz_bison$animal_sex = as.factor(data$animal_sex)
knz_bison$age = knz_bison$rec_year - knz_bison$animal_yob

result = system_function(formula = animal_weight ~ animal_sex*age, data = knz_bison, mode = "test", dist = "uninormal")

# two cat, one cont:
data = mtcars
data$am = as.factor(data$am)
data$vs = as.factor(data$vs)
data$gear = as.factor(data$gear)

result2 = system_function(formula =  mpg~ hp*qsec, data = data, mode = "test", dist = "uninormal")


#### test the function on some data ####
library(lterdatasampler) # data freely available, credits to https://lter.github.io/lterdatasampler/reference/and_vertebrates.html

#load data
data("hbr_maples")
#inspect briefly
head(hbr_maples) # I'm interested in the relationship of watershed (treatment with calcium or not) and stem_dry_mass (representing productivity)
hbr_maples$year = as.factor(hbr_maples$year)
hbr_maples$watershed = as.factor(hbr_maples$watershed)
summary(hbr_maples)
# also, elevation could have a strong influence on growth, so include the variable and possible interactions
# assumed relation: stem_dry_mass ~ watershed + elevation
# stem_dry_mass: continuous
# watershed: categorical: treated (W1), not treated reference (Reference)
# year: numeric --> make factor
# elevation: Low and Mid as levels 

#test the function
test = system_function(formula = stem_dry_mass ~ watershed * year, data = hbr_maples, mode = "test", dist = uninormal())
# better reporting of DHARMa results!
# explanation of the model summary printed
plot(stem_dry_mass ~ watershed * elevation, data = hbr_maples, las = 1, alpha = 0.8, ask = F)


# another test:
#install.packages("AER")
library(AER)
data("NMES1988")
NMES1988$adl = as.factor(NMES1988$adl)
NMES1988$region = as.factor(NMES1988$region)
NMES1988$health = as.factor(NMES1988$health)

test2 = system_function(visits ~ health + adl + gender,
                        data = NMES1988, dist = "poissonff", mode = "test")

#last example test
data("ntl_icecover")

test3 = system_function(ice_duration ~ year,
                        data = ntl_icecover, dist = "uninormal", mode = "test", verbose = T)

###### to do ######
# - shapley values as variable importance
# - 6 fix errors due to poly(x, 2) (error start with other data type...)
# - 9 add more information on input data! => comprehensive data checking and report the findings
# - 11 for small number of observation: add error bars using bootstrapping
# - Mit options(warn=2) kann man R zwingen, alle Warnungen in Fehlermeldungen umzuwandeln, bei warn=-1 werden sie alle ignoriert: siehe ?options unter warn.
