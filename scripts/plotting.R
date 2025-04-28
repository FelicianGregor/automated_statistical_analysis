plotting = function(list, verbose = T){
  #start the "recording" using png (before setting the par)
  png('./output/plots/plot.png', width = 8, units = "in", height = 5, res = 300)

  #set par(mfrow = c()) to 2 cols and right number of rows depending on data
  list$plot_rows_needed = ceiling(((ncol(list$data_na.omit)-1)/2))
  if ((ncol(list$data_na.omit)-1) < 1){
    par(mfrow = c(1,1))
  } else{
    #adjust some par for more space efficient plotting when having more than one rows
    par(mfrow = c(list$plot_rows_needed, 2))
    par(mar = c(4.5, 4, 0.5, 0.5))
    par(tcl = 0.5) # axis ticks inside the window
    par(mgp = c(2.5, 0.5, 0)) # move axis title etc. closer
    par(las = 1) #turn ticks
  }
  
  #start actual plotting
  plot(formula, data = data, las = 1, ask = F, cex = 0.7)
  dev.off()
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) # set par() back
  if (verbose){cat("plot done\n")}
  
  return(list)
}

