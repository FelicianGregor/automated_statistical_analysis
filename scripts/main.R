### main script to call the other scripts and test the system ###

# prepare everything to run the analyse()-function

# check for right working directory:
if(endsWith(getwd(), "scripts")){
  cat("Great, your working directory is set correctly!")
} else{
  warning(paste)("Attention! Your working directory must be the folder 'scripts',\n
            but your current working directory is ", getwd(), ".")}

# install / load packages used by AS
source("helper_functions.R")
install_packages_AS() # installs packages needed in AS in cases not yet installed, and loads them

# create folder structure for outputs
create_folder()

# get analyse function 
source("analyse.R")

### 1. t-test example ####
# normally distributed data, group comparison, linear

north <- c(12, 23, 15, 18, 20)
south <- c(5, 8, 7, 9, 9)
t.test(north, south, var.equal=F)

# using the AS:
# change the format of the data to a data.frame:
data_north = data.frame(num_species = north, direction = rep(factor("north"), length(north)))
data_south = data.frame(num_species = south, direction = rep(factor("south"), length(south)))
mosses = rbind(data_north, data_south)

# analyse with the AS
test1 = analyse(formula = num_species ~ direction, data = mosses, mode = "test", dist = uninormal(), verbose = T)
# another data format necessary
# more explicit assumption of distribution (normal)
# less explicit: assumption on variance is less explicit and no chance to conduct welch unequal variance test in the GLM framework, since they estimate only the mean (but mixed effects model would be needed?)
# gives another type of information: are they equal or not, but does not (directly) give the exact estimate for every value (just graphically, however, could easily be translated into a table using predict() and then every combination, as done already)
# no proper units labeling the axis!!

# compare CI
moss_model = vglm(formula = num_species ~ direction, data = mosses, family = uninormal())
summary(moss_model) # does not give means
predict(moss_model, newdata = data.frame(direction = "north")) # makes also clear that the variances are assumed to be equal
predict(moss_model, newdata = data.frame(direction = "south"))




# test the system on data
data = knz_bison
knz_bison$rec_month = as.factor(data$rec_month)
knz_bison$animal_sex = as.factor(data$animal_sex)
knz_bison$age = knz_bison$rec_year - knz_bison$animal_yob

# run the system
result = analyse(formula = animal_weight ~ animal_sex*poly(age, 4), data = knz_bison, mode = "test", dist = "uninormal")

# two cat, one cont:
data(mtcars)
mtcars$am = as.factor(mtcars$am)
mtcars$vs = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)

test = analyse(formula =  mpg~ hp*qsec, data = mtcars, mode = "test", dist = "poissonff")
model = vglm(formula =  mpg~ hp*qsec, data = mtcars, family = "uninormal")
model_p = vglm(formula =  mpg~ hp*qsec, data = mtcars, family = "poissonff")
model.matrix(model_p)


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
test1 = analyse(formula = stem_dry_mass ~ watershed * as.factor(year), data = hbr_maples, mode = "test", dist = uninormal())

# another test:
#install.packages("AER")
library(AER)
data("NMES1988")
NMES1988$adl = as.factor(NMES1988$adl)
NMES1988$region = as.factor(NMES1988$region)
NMES1988$health = as.factor(NMES1988$health)

# run the analysis function
test2 = analyse(visits ~ as.factor(gender) * poly(age, 2),
                        data = NMES1988, dist = "uninormal", mode = "test", verbose = T)
#problems: for many data points, the points are lying on top of the predicted response with error bars so that one cannot see everything
# might be specific to one cat and one cont in plotting
##### example test
data("ntl_icecover")

# run the analysis function
test3 = analyse(ice_duration ~ year,
                        data = ntl_icecover, dist = "uninormal", mode = "test", verbose = T)
# problem: looks good. just the plotting window quadratic!

##### another example data set
data = mtcars

# run the analysis function
test4 = analyse(disp ~  poly(hp, 3) + as.factor(am) + as.factor(gear), data = mtcars, dist = "uninormal", mode = "test", verbose = T)
# looks ok!

##### another test, including poly()
ozone # ozone dataset

test4.1 = analyse(O3 ~ (poly(temp, 4) + poly(wind, 4) + poly(humidity, 4) + poly(vis, 3))^3, data = ozone, dist = "uninormal", mode = "test", verbose = T)
test4.2 = analyse(O3 ~ temp * wind * humidity, data = ozone, dist = "uninormal", mode = "test", verbose = T)

##### test for including poly and categorical variables
data = mtcars
data$gear = as.factor(mtcars$gear)
data$am = as.factor(mtcars$am)
data$cyl = as.factor(mtcars$cyl)

test5 = analyse(disp ~ am + poly(as.factor(gear), 2) + cyl, data = data, dist = "uninormal", mode = "test", verbose = T)
# makes predictions also for NA values??


### 2. simple regression ####
head(trees) # data from Rdatasets, girth = diameter at breast hight

# fit model, as done in book Dormann 2020, page 95
fm2 <- glm(Volume ~ Girth, data=trees, family=gaussian)

# using the analyse function from the AS:
test2 = analyse(formula = Volume ~ Girth, data = trees, dist = "uninormal", mode = "test")
# mainly reproduces the results 
# legend with CI and estimate is missing (what are the dotted lines one may ask)

### 3 poisson regression using flycatcher ####
attract = read.delim("./data/flycatcher.txt", header = TRUE, sep = "")
fm3 = glm(items ~ attract, data = attract, family = "poisson")

# analyse with AS
test3 = analyse(formula = items ~ poly(attract, 2), data = attract, dist = "poissonff", mode = "test")
# there is not really an interpretation of the results in the book
# however, we made the decision for plotting on the response scale, not the link scale emphazising different aspects 


#### 4. multiple regression with normally distributed data & interaction ####
cormorant = read.delim("./data/cormorant.txt", header = TRUE, sep = "", stringsAsFactors = T)
fm4 = glm(divetime ~ season + subspecies, data = cormorant, family = "gaussian")
anova(fm4)

# use the analyse function from the AS:
test4 = analyse(divetime ~ season + subspecies, data = cormorant, dist = "poissonff", mode = "test")
# the plot is really different, AS produces boxplot, not facets
# we don't really know whether the subspecies effect or season effect is significant - one could produce an anove like table out of this
anova.vglm(test4$model_mf) # use the model object with model frame, using model = T as default can create odd behaviour during plotting, that is why we created a second model object that can get used for the anova table


data("airquality")
airquality

model = vglm(Ozone ~ Solar.R * Temp, data = airquality, family = "poissonff")
anova(model)
summary(model)

##### test on three categorical vars ####
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
test4 = analyse(response ~ fac3*fac2*fac1, data = df, dist = "uninormal", mode = "test", verbose = T)

# test binomial data on passer montanus ####
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
test4 = analyse(succ ~ forest, data = birds, dist = "binomialff", mode = "test", verbose = T)
model_binom = vglm(succ ~ forest, data = birds, family = "binomialff")
model.matrix(model_binom)
intercept_cols = which(grepl("Intercept", colnames(model.matrix(model_binom))))
n_parameters = ncol(as.data.frame(model.matrix(model_binom))[-c(intercept_cols)])

mm = model.matrix(model_binom)
as.data.frame(model.matrix(model_binom))[1]

###### to do ######
# - 9 add more information on input data! => comprehensive data checking and report the findings
# - 11 for small number of observation: add error bars using bootstrapping
# - Mit options(warn=2) kann man R zwingen, alle Warnungen in Fehlermeldungen umzuwandeln, bei warn=-1 werden sie alle ignoriert: siehe ?options unter warn.
