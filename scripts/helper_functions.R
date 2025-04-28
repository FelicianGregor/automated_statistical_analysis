all_integers = function(response_value){ 
  #define function to check if value is integer or not (in order to use integerResponse in DHARMa correctly)
  return(floor(response_value) %% response_value == 0)
}