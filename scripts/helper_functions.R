all_integers = function(response_value){ 
  #define function to check if value is integer or not (in order to use integerResponse in DHARMa correctly)
  return(floor(response_value) %% response_value == 0 | is.null(response_value))
}
is_cat = function(x){
  "takes input and returns true if first element needs to get treated categorical, meaning being either a character or factor"
  res = (is.character(x) || is.factor(x))
  return(res)
  }
header = function(){
  "creates header of quarto document. has params predefined!!!
  used in reporting script"
  header_text = '---
format: html
editor: visual
title: "report automated statistical analysis"
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
  cat("\n\n", file = "./output/reports/test_report.qmd", append = T)
}
