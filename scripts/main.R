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
    
  } else if (mode == "explore"){
    
  } else {
    cat("please specify a valid mode of either `test`, `predict` or `explore`")
  }
  
  return(list)
}

#try out with some data:
library(lterdatasampler)
library(DHARMa)
data = knz_bison
knz_bison$age = knz_bison$rec_year - knz_bison$animal_yob

result = system_function(formula = animal_weight ~ age * as.factor(animal_sex), data = knz_bison, mode = "test", dist = "poissonff")

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
test = system_function(formula = stem_dry_mass ~ watershed * as.factor(year), data = hbr_maples, mode = "test", dist = uninormal())
# better reporting of DHARMa results!
# explanation of the model summary printed
plot(stem_dry_mass ~ watershed * elevation * year, data = hbr_maples, las = 1, alpha = 0.8, ask = F)


# another test:
#install.packages("AER")
library(AER)
data("NMES1988")
test2 = system_function(visits ~ health + age + gender + married + income + insurance,
                        data = NMES1988, dist = "poissonff", mode = "test")

###### to do ######
# - 2 effect sizes! / or better short introduction on how to read the summary(glm)-output? 
# - 3 add effects in plots (lots of work in case i need to write the code by myself...) --> ggeffects::ggpredict()?
# - 1 make DHARMa diagnostics better! - add residual plot!
# - 4 include automatic model adjustment just with deleted cooks D values (as a first simple check if model adjustments work)?
# - 5 cooks distance issues #
# - 6 have errors due to poly(x, 2) in formula when plotting in mind
# - 7 write understandable code for dispersion and deviance stuff # later, after answer from Dormann
# - 9 add more information on input data! => comprehensive data checking and report the findings
# - 10 computation of effects (coefficients) in vglm on response scale did not work...
# - 11 for small number of observation: add error bars using bootstrapping
# - Mit options(warn=2) kann man R zwingen, alle Warnungen in Fehlermeldungen umzu-
# - Mit options(warn=2) kann man R zwingen, alle Warnungen in Fehlermeldungen umzuwandeln, bei warn=-1 werden sie alle ignoriert: siehe ?options unter warn.
