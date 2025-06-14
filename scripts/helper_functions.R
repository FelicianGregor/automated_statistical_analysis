header = function(){
  "creates header of quarto document. has params predefined!!!
  used in reporting script"
  header_text = '---
format: html
editor: visual
title: "report automated statistical analysis"
warning: false
params:
  model_results_output: "summary cat failed"
  significance_output: "significance output failed"
---\n'
  return(header_text)
}
add = function(text){
  "adds text to file called test_reporting.qmd, used in reporting script"
  cat(text, file = "./output/reports/test_report.qmd", append = T)
}
new_line = function(){
  'adds 2 new lines to "./output/reports/test_report.qmd", used in reporting script'
  cat("\n", file = "./output/reports/test_report.qmd", append = T)
}

install_packages_AS = function(){
  packages_needed_AS = c("DHARMa", # simulation-based diagnostics 
                         "lterdatasampler", # free environmental data sets
                         "VGAM", # for model building with vglm()
                         "ds4psy", # for is_wholenumber()
                         "polycor", # for hetcor() --> continuous, polychoric / polyserial correlations
                         "shapviz", # for shapley values (in script model_fitting)
                         "ggplot2", # for visualizing shapley values
                         "ggthemes",  # for theme_minimal in visualization of shapley values
                         "gt", # for nicely formatted tables for report
                         "tibble",  # needed for gt tables
                         "stringr") # for str_remove_all function
  
  # Install packages not yet installed
  installed_packages = packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {install.packages(packages[!installed_packages])}
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE)) # invisible to suppress output in console
}

create_folder = function(){
  # create a folder structure for saving the results, if not existing:
  if (dir.exists("./automated_statistical_analysis")){
  
    #print message if directories are created
    message("The Automated Statistician creates a folder structure for saving results in your working directory.")
  
    dir.create("./automated_statistical_analysis/output")
    dir.create("./automated_statistical_analysis/output/plots")
    dir.create("./automated_statistical_analysis/output/report")
    dir.create("./automated_statistical_analysis/output/tables")
  } else{
  warning("Your directory is not named correctly.\nPlease name it './automated_statistical_analysis'.")
  }
}


