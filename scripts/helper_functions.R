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
  cat(text, file = "../output/report/test_report.qmd", append = T)
}
new_line = function(){
  'adds 2 new lines to "./output/report/test_report.qmd", used in reporting script'
  cat("\n", file = "../output/report/test_report.qmd", append = T)
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
                         "stringr", # for str_remove_all function
                         "quarto") # to write and render the report as quarto document and html file 
  
  # Install packages not yet installed
  installed_packages = packages_needed_AS %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {install.packages(packages_needed_AS[!installed_packages])}
  
  # Packages loading
  invisible(lapply(packages_needed_AS, library, character.only = TRUE)) # invisible to suppress output in console
}

create_folder = function(){
  # create a folder structure for saving the results, if not existing:
  if (endsWith(getwd(), "scripts")){
    
    dir.create("../output")
    dir.create("../output/plots")
    dir.create("../output/report")
    dir.create("../output/tables")
    
    #print message if directories are created
    message("The Automated Statistician created a folder structure for saving results in your working directory.")
    
  } else{
    warning(paste)("Your working directory must be the folder 'scripts',\n
            but your current working directory is ", getwd(), ".")
  }
}
remove_html = function(file_name){
  "takes .html file as input and removes the start of the document that would look ugly in rendered quarto document.
  Unfortunately, the use of .html files in quarto directly makes troubles rendering it as PDF later"
  html_string <- readLines(file_name)
  # Remove lines containing <html>, <head>, etc.
  html_doc_type_removed <- html_string[!grepl("<!DOCTYPE html>", html_string)]
  writeLines(html_doc_type_removed, file_name)
}
