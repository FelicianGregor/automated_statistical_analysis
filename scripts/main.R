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
data(mtcars)
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
test = system_function(formula = stem_dry_mass ~ watershed * as.factor(year), data = hbr_maples, mode = "test", dist = uninormal())
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


test2 = system_function(visits ~ as.factor(gender) * poly(age, 2),
                        data = NMES1988, dist = "poissonff", mode = "test", verbose = T)

#last example test
data("ntl_icecover")

test3 = system_function(ice_duration ~ year,
                        data = ntl_icecover, dist = "uninormal", mode = "test", verbose = T)

# another example data set:

data = mtcars

test4 = system_function(disp ~  poly(hp, 3) + as.factor(am) * as.factor(gear), data = mtcars, dist = "uninormal", mode = "test", verbose = T)

ozone

O3 ~ temp + humidity + I(temp^2) + I(temp^3) + I(wind^3)
test4 = system_function(O3 ~ (poly(temp, 4) + poly(wind, 4) + poly(humidity, 4) + poly(vis, 3))^3, data = ozone, dist = "uninormal", mode = "test", verbose = T)

data = mtcars
data$gear = as.factor(mtcars$gear)
data$am = as.factor(mtcars$am)
data$cyl = as.factor(mtcars$cyl)

test4 = system_function(disp ~ am + poly(as.factor(gear), 2) + cyl, data = data, dist = "uninormal", mode = "test", verbose = T)


str(mtcars)

# test with three categorial vars:
# Load required library
library(VGAM)

# Set seed for reproducibility
set.seed(123)

# Define levels for each factor
fac1_levels <- c("A", "B", "C")
fac2_levels <- c("X", "Y")
fac3_levels <- c("Low", "Medium", "High")

# Number of observations per combination
n_per_comb <- 25

# Create all combinations of factor levels
df <- expand.grid(fac1 = fac1_levels,
                  fac2 = fac2_levels,
                  fac3 = fac3_levels,
                  rep = 1:n_per_comb)

# Remove 'rep' to keep only the desired variables
df$rep <- NULL

# Turn into factors (ensuring proper order/structure)
df$fac1 <- factor(df$fac1)
df$fac2 <- factor(df$fac2)
df$fac3 <- factor(df$fac3, levels = c("Low", "Medium", "High"))

# Simulate a response variable
# Additive and interaction effects + noise
df$response <- with(df,
                    5 +
                      as.numeric(fac1) * 2 +
                      as.numeric(fac2) * -1.5 +
                      as.numeric(fac3) * 3 +
                      as.numeric(fac1) * as.numeric(fac2) * 0.5 +  # proper numeric interaction
                      rnorm(nrow(df), mean = 0, sd = 2))

# test
test4 = system_function(response ~ fac3, data = df, dist = "uninormal", mode = "test", verbose = T)

##### test on binomial data ####
### test binomial data on passer montanus (that is not montanus haha) ####
library(jSDM)
data("birds")

#inspect data:
head(birds) # selct just one column and the other cols
important_cols = c("Passer montanus", "siteID", 
                   "coordx", 
                   "coordy", 
                   "elev", 
                   "rlength", 
                   "nsurvey", # 2 or 3, 3 if below the tree line, 2 above
                   "forest", #percentage of forest in 1x1km quadrat
                   "obs14")

#subset
birds = birds[, important_cols]
head(birds) 
summary(birds) # two surveys mostly... --> convert to 0 / 1 data

make_binary = function(x){if( x >= 1){return(1)}else{return(0)}}
birds$Passer_montanus_binary = sapply(birds$`Passer montanus`, make_binary)

# make to success and failure:
binodata = data.frame(succ = birds$Passer_montanus_binary, fail = 2-birds$Passer_montanus_binary)
proportion = binodata$succ / (binodata$fail+binodata$succ)
birds = cbind(birds, proportion, binodata)

quantiles_bino = quantile(x = birds$forest, probs = c(0, 0.25, 0.5, 0.75, 1))
x_quantile_factor = cut(birds$forest, breaks = quantiles_bino, include.lowest = T, labels = FALSE)
forest_Q <- factor(x_quantile_factor, 
                            labels = paste0("forest Q", 1:(length(quantiles_bino) - 1)))

forest = data.frame(original = birds$forest, quantile_group = x_quantile_factor)

# include the forests in quartiles
birds = data.frame(forest_Q = forest_Q, birds)
str(birds$forest_Q)
head(birds)

# vglm(cbind(succ, fail) ~ forest * elev, data = birds, family = "binomialff")
test4 = system_function(succ ~ elev*forest, data = birds, dist = "binomialff", mode = "test", verbose = T)



###### to do ######
# - shapley values as variable importance
# - 6 fix errors due to poly(x, 2) (error start with other data type...)
# - 9 add more information on input data! => comprehensive data checking and report the findings
# - 11 for small number of observation: add error bars using bootstrapping
# - Mit options(warn=2) kann man R zwingen, alle Warnungen in Fehlermeldungen umzuwandeln, bei warn=-1 werden sie alle ignoriert: siehe ?options unter warn.
