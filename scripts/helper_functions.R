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
