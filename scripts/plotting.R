plotting = function(list, verbose = T){
  if (verbose){cat("entered plotting\n")}

  
  #store data first
  data = list$model@model
  model = list$model
  
  #get the model frame
  mf <- model.frame(list$model)
  
  # get the variables:
  terms = terms(model)
  preds = attr(terms, "term.labels") #all variables with interactions (denoted with ":")
  
  # differentiate between interactions and no interactions:
  if (length(grep(":", preds)>0)){
    preds_simple = preds[-grep(":", preds)]
    preds_inter = setdiff(preds, preds_simple) # get the other variables
  } else{
    preds_simple = preds
    preds_inter = 0
  }
  
  #determine the classes:
  data_classes = attr(terms, "dataClasses")
  data_classes_simple = data_classes[preds_simple] #without interactions, just taking the normal terms
  
  #split in cont and fac preds
  cont = names(which(data_classes_simple=="numeric"))
  factors = names(which(data_classes_simple=="factor" | data_classes_simple=="ordered" | data_classes_simple == "character"))
  if(verbose){cat(print(factors), "\n")}
  
  # what cant get displayed yet:
  if (length(factors)>3){
    cat("models with more than three factors can't get displayed yet!")}
  if (length(cont)>3 & length(factors)==0){
    cat("models with more than three continuous variables can't get displayed!")}
  if((length(cont)==3 + length(factors)) > 3){
    cat("models with more than three predictors in total can't get displayed yet!")}
  
  # if statements cont & cat facet plots####
  if (length(factors) == 2 & length(cont)== 1){
    # make scatter plot for cont pred & cont response, color  = pred cat1, factet for cat2
    
    #get the variables
    fac1 = factors[1]
    fac2 = factors[2]
    
    #create new values for prediction
    
    # continuous
    cont_new = seq(min(model@model[cont]), max(model@model[cont]), length = 10000)
    
    # categorical variable one level:
    levels_col_vec = levels(mf[[fac1]])
    levels_plot_vec = levels(mf[[fac2]]) 
    
    plot_rows_needed = ceiling((length(levels_plot_vec)-1)/2)
    
    if (plot_rows_needed < 1){
      #start the "recording" using png (before setting the par)
      png('./output/plots/plot.png', width = 4, units = "in", height = 4, res = 300)
      par(mfrow = c(1,1))
    } else {
      #start recording with height depending on number of plots
      png('./output/plots/plot.png', width = 8, units = "in", height = 4*plot_rows_needed, res = 300)
      #adjust some par for more space efficient plotting when having more than one rows
      par(mfrow = c(plot_rows_needed, 2))
      par(mar = c(4.5, 4, 2, 2))
      par(tcl = 0.5) # axis ticks inside the window
      par(mgp = c(2.5, 0.5, 0)) # move axis title etc. closer
      par(las = 1) #turn ticks
    }
    
    
    for (level_plot in levels_plot_vec){
      #add empty plot to have comparable plot windows
      #original data:
      response_full = model@model[,1]
      cont_pred_full = model@model[[cont]]
      
      #range to add empty plotting window
      x_lim_range = range(cont_pred_full)
      y_lim_range = range(response_full)
      plot(cont_pred_full, response_full, type = "n",  
           xlab = cont, ylab = responseName(model), 
           xlim = x_lim_range, ylim = y_lim_range, 
           main = paste0("model predictions with ",fac2, ": ",  level_plot), 
           pch = 16, las = 1)
      grid(col = "lightgrey")
      
      #add legend and create colors wit length of number of levels
      colors <- rainbow(length(levels_col_vec))
      legend("topleft", legend = levels_col_vec, col = colors, 
             pch = rep(16, 2), lty = rep(1, 2), 
             title = fac1)
      
      #set counter for colors
      counter = 0
      for (level in levels_col_vec){
        counter = counter + 1
        new_data <- data.frame(
          dummy_cont = cont_new,
          dummy_fac1 = level,
          dummy_fac2 = level_plot)
        
        #set names according to variable cat predictor name
        names(new_data) = c(cont, fac1, fac2)
        
        #predict
        preds = predict(model, newdata = new_data, type = "response")
        
        #plot
        
        #just subset of data for cat preds
        cat_predictor_treat = cont_pred_full[which(model@model[[fac2]]==level_plot & model@model[[fac1]]==level)]
        cat_response_treat = response_full[which(model@model[[fac2]]==level_plot & model@model[[fac1]]==level)]
        
        # add the data points
        points(cat_predictor_treat, cat_response_treat, pch = 16, las = 1, col = colors[counter])
        
        #add lines
        lines(cont_new, preds, lwd = 2, col = colors[counter])
      }
    }
    
    #stop reporting
    dev.off()
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) # set par() back
    
  }
  if (length(factors) == 1 & length(cont)== 1){
    
    # make scatterplot for cont pred & cont response, color  = pred cat1
    
    #get the levels
    fac1 = factors[1]
    
    #create new values for prediction
    
    # continuous
    cont_new = seq(min(model@model[cont]), max(model@model[cont]), length = 10000)
    
    # categorical variable one level:
    
    levels_col_vec = levels(mf[[fac1]])
    
    if (is.null(levels_col_vec)){
      levels_col_vec = unique(mf[[fac1]])
    }
    
    print(paste("problem, if NULL: ", levels_col_vec))
    
    #add empty plot to have comparable plot windows
    #original data:
    response_full = model@model[,1]
    cont_pred_full = model@model[[cont]]
    
    #start recording
    png('./output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    
    #range to add empty plotting window
    x_lim_range = range(cont_pred_full)
    y_lim_range = range(response_full)
    plot(cont_pred_full, response_full, type = "n",  
         xlab = cont, ylab = responseName(model), 
         xlim = x_lim_range, ylim = y_lim_range, 
         main = paste0("model predictions depending on ", cont, " & ", fac1), 
         pch = 16, las = 1)
    grid(col = "lightgrey")
    
    #add legend and create colors wit length of number of levels
    colors <- rainbow(length(levels_col_vec))
    legend("topleft", legend = levels_col_vec, col = colors, 
           pch = rep(16, 2), lty = rep(1, 2), 
           title = fac1)
    
    #set counter for colors
    counter = 0
    for (level in levels_col_vec){
      counter = counter + 1
      new_data <- data.frame(
        dummy_cont = cont_new,
        dummy_fac1 = level)
      
      #set names according to variable cat predictor name
      names(new_data) = c(cont, fac1)
      print(names(new_data))
      
      #predict
      preds = predict(model, newdata = new_data, type = "response")
      
      #plot
      
      #just subset of data for cat preds
      cat_predictor_treat = cont_pred_full[model@model[[fac1]]==level]
      cat_response_treat = response_full[model@model[[fac1]]==level]
      
      length(cat_response_treat)
      
      # add the data points
      points(cat_predictor_treat, cat_response_treat, pch = 16, las = 1, col = colors[counter])
      
      #add lines
      lines(cont_new, preds, lwd = 2, col = colors[counter])
      
    }
    # stop plot recording
    dev.off()
  }
  if (length(cont)==1 & length(factors)==0){
    cont_x = cont[1]
    
    #make prediction sequence for both cont pred variables
    cont_x_seq <- seq(min(data[[cont_x]]), max(data[[cont_x]]), length = 1000)
    new_data = data.frame(dummy_cont_x = cont_x_seq)
    names(new_data) = c(cont_x)
    
    #make prediction
    preds = predict(model, newdata = new_data, response = T)
    print(head(preds))
    
    #plot
    x = model@model[[cont_x]]
    y = model@y
    
    #start recording
    png('./output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    
    plot(x, y,
         xlab = paste("predictor", cont_x), 
         ylab = paste("response", responseName(model)), 
         col = "grey",
         pch = 16, 
         las = 1)
    lines(cont_x_seq, preds[,1], lwd = 2, col = "red")
    #lines(cont_x_seq, preds$se.fit[,1] + 1.96*preds$se.fit[,2], lwd = 2, col = "pink", lty = 2)
    #lines(cont_x_seq, preds$se.fit[,1] - 1.96*preds$se.fit[,2], lwd = 2, col = "pink", lty = 2)
    
    #stop recording:
    dev.off()
  }
  
  ### if statements more than two cont: contour / heatmap ####
  if (length(cont)==3 & length(factors)== 0){
    #heatplot with contours for cont1 & cont2 with z being response, cont3 is made discrete through quantile and plotted to three distinct contourplots
    
    #split the thirds continuous in quantile and plot a seperate contourplot for each quantile
    cont_x = cont[1]
    cont_y = cont[2]
    cont_quant = cont[3]
    
    cont_x_seq <- seq(min(data[[cont_x]]), max(data[[cont_x]]), length = 30)
    cont_y_seq <- seq(min(data[[cont_y]]), max(data[[cont_y]]), length = 30)
    
    quantiles = c(0.25, 0.5, 0.75)
    cont_quant_values = quantile(data[[cont_quant]], probs = quantiles, na.rm = T) # make quantiles for visualization
    
    #save plots
    # since 3 quantiles are used: 3 rows
    png('./output/plots/plot.png', width = 10, units = "in", height = 4*2, res = 300)
    par(mfrow = c(2, 2))
    
    for (i in 1:length(cont_quant_values)){
      
      z <- outer(cont_x_seq, cont_y_seq, 
                 #define the function
                 FUN = function(X, Y) {
                   
                   #inside the function the dataframe with the data
                   new_data = data.frame(
                     dummy_cont_x = X, 
                     dummy_cont_y = Y, 
                     dummy_cont_quant = as.vector(cont_quant_values[i])
                   )
                   
                   #set the names
                   names(new_data) = c(cont_x, cont_y, cont_quant)
                   
                   #call the actual predict function 
                   predict(model, newdata = new_data, type = "response")
                 })
      
      
      # plotting
      image(cont_x_seq, cont_y_seq, z, cex.lab = 1.5,
            ylab = paste(cont_y), xlab = paste(cont_x),
            col = heat.colors(30), 
            main = paste0("response ",responseName(model), " with ", quantiles[i]*100, "th quant. value ", cont_quant_values[i], " of varible ", cont_quant))
      grid(col = "lightgrey")
      contour(cont_x_seq, cont_y_seq, z, add=T)
      points(data[[cont_x]], data[[cont_y]], pch="+", cex=1)
      ch <- chull(cbind(data[[cont_x]], data[[cont_y]]))
      polygon(cbind(data[[cont_x]], data[[cont_y]])[ch,])
      
    }
    #stop recording
    dev.off()
    par(mfrow = c(1,1))
  }
  if (length(cont)==2 & length(factors)==0){
    # one contourplot produced (cont1 against cont2 and z = response)
    
    #split the data to the two seperate preds
    cont_x = cont[1]
    cont_y = cont[2]
    
    #make prediction sequence for both cont pred variables
    cont_x_seq <- seq(min(data[[cont_x]]), max(data[[cont_x]]), length = 30)
    cont_y_seq <- seq(min(data[[cont_y]]), max(data[[cont_y]]), length = 30)
    
    #prediction
    z = outer(X = cont_x_seq, 
              Y = cont_y_seq, 
              FUN = function(X, Y) {
                
                #inside the function the dataframe with the data
                new_data = data.frame(
                  dummy_cont_x = X, 
                  dummy_cont_y = Y)
                
                #set the names
                names(new_data) = c(cont_x, cont_y)
                
                #call the actual predict function 
                predict(model, newdata = new_data, type = "response")
              })
    
    #start saving
    png('./output/plots/plot.png', width = 6, units = "in", height = 6, res = 300)
    
    #make plot
    image(cont_x_seq, cont_y_seq, z, cex.lab = 1.5,
          ylab = paste(cont_y), xlab = paste(cont_x),
          col = heat.colors(30))
    #add grid
    grid(col = "lightgrey")
    #add contours
    contour(cont_x_seq, cont_y_seq, z, add=T)
    #add the points
    points(data[[cont_x]], data[[cont_y]], pch="+", cex=1)
    #add the margin line for points
    ch <- chull(cbind(data[[cont_x]], data[[cont_y]]))
    polygon(cbind(data[[cont_x]], data[[cont_y]])[ch,])
    
    #stop recording
    dev.off()
  }
  if (length(cont)==2 & length(factors)==1){
    # contourplot / heatmap for cont1 & cont2 (z being response), for each level of cat1 new conturplot
    #(make the same as for 3 cont vars, where the factor are the quantiles)
    #heatplot with contours for cont1 & cont2 with z being response, cont3 is made discrete through quantile and plotted to three distinct contourplots
    
    #split the thirds continuous in quantile and plot a seperate contourplot for each quantile
    cont_x = cont[1]
    cont_y = cont[2]
    cat1 = factors[1]
    
    cont_x_seq <- seq(min(data[[cont_x]]), max(data[[cont_x]]), length = 50)
    cont_y_seq <- seq(min(data[[cont_y]]), max(data[[cont_y]]), length = 50)
    
    cat1_levels = levels(data[[cat1]]) # get the levels of cat1
    
    #rows needed:
    rows_needed = ceiling(length(cat1_levels)/2)
    png('./output/plots/plot.png', width = 10, units = "in", height = 4*rows_needed, res = 300)
    par(mfrow = c(rows_needed, 2))
    for (i in 1:length(cat1_levels)){
      
      z <- outer(cont_x_seq, cont_y_seq, 
                 #define the function
                 FUN = function(X, Y) {
                   
                   #inside the function the dataframe with the data
                   new_data = data.frame(
                     dummy_cont_x = X, 
                     dummy_cont_y = Y, 
                     dummy_cat1 = as.vector(cat1_levels[i])
                   )
                   
                   #set the names
                   names(new_data) = c(cont_x, cont_y, cat1)
                   
                   #call the actual predict function 
                   predict(model, newdata = new_data, type = "response")
                 })
      
      
      # plotting
      image(cont_x_seq, cont_y_seq, z, cex.lab = 1.5,
            ylab = paste(cont_y), xlab = paste(cont_x),
            col = heat.colors(30), 
            main = paste0("response ",responseName(model), " with cat predictor ", cat1, ' & level "', cat1_levels[i], '"'))
      grid(col = "lightgrey")
      contour(cont_x_seq, cont_y_seq, z, add=T)
      points(data[[cont_x]], data[[cont_y]], pch="+", cex=1)
      ch <- chull(cbind(data[[cont_x]], data[[cont_y]]))
      polygon(cbind(data[[cont_x]], data[[cont_y]])[ch,])
      
    }
    
    #stop recording
    dev.off()
    
  }
  
  ### if statements just factors: boxplots ####
  if(length(factors)==3 & length(cont)==0){
    
    #factor variable names
    fac1 = factors[1]
    fac2 = factors[2]
    fac3 = factors[3]
    
    
    # levels per variable
    levels_fac1 = levels(mf[[fac1]])
    levels_fac2 = levels(mf[[fac2]])
    levels_fac3 = levels(mf[[fac3]])
    
    #rows needed and start recording png
    rows_needed = ceiling(length(levels_fac3)/2)
    png('./output/plots/plot.png', width = 10, units = "in", height = 4*rows_needed, res = 300)
    par(mfrow = c(rows_needed, 2))
    
    #start loop
    for (level_fac3 in levels_fac3){
      grid <- expand.grid(levels_fac2, levels_fac1)
      names(grid) <- c(fac2, fac1)
      
      # make sure factors are preserved
      grid[[fac1]] <- factor(grid[[fac1]], levels = levels_fac1)
      grid[[fac2]] <- factor(grid[[fac2]], levels = levels_fac2)
      
      #add fac3 as pred
      grid[[fac3]] = rep(as.factor(level_fac3), ncol(grid))
      
      
      grid$preds = predict(model, newdata = grid, type = "response")
      
      colors = rainbow(length(levels_fac2))
      
      #start plotting:
      par(mar = c(5, 4, 4, 8), xpd = TRUE)  # Expand right margin
      
      #exclude fac3, since I make different plots for the different levels of fac3
      plot_formula = as.formula(paste0(responseName(model), "~", fac1, "*", fac2))
      
      boxplot(plot_formula, data = mf, xaxt = "n", col = colors, 
              xlab = fac1, las = 1, 
              main = paste0('plot model with factor "', fac3, '" & level "', level_fac3, '" as reference'))
      
      # set custom axis labels — one per fac1 group
      n_fac2 = length(levels(data[[fac2]]))
      n_fac1 = length(levels(data[[fac1]]))
      
      # compute positions (middle points) per fac1 group
      group_positions = seq(1, n_fac2 * n_fac1, by = n_fac2) + (n_fac2 - 1) / 2
      
      # draw axis: one label per group of fac1 
      axis(1, at = group_positions, labels = levels_fac1)
      
      # add model preiction
      points(x = 1:length(grid$preds), y = grid$pred, pch = 16, cex = 3, col = "orange")
      
      
      # legend (placed outside)
      legend("right", legend = c(levels_fac2, "preds"), col = c(colors, "orange"), title = fac2, 
             pch = c(rep(15, n_fac2), 16), pt.cex = 2, 
             inset = c(-0.25, 0), 
             bty = "n")
      
      #set par back:
      par(xpd = FALSE)
      
      # ddd vertical lines to separate groups
      for (i in 1:(n_fac1 - 1)) {
        abline(v = i * n_fac2 + 0.5, col = "black", lty = 2)
      }
      
    }
    dev.off()
    
  }
  if(length(factors)==2 & length(cont)==0){
    
    #factor variable names
    fac1 = factors[1]
    fac2 = factors[2]
    
    # levels per variable
    levels_fac1 = levels(mf[[fac1]])
    levels_fac2 = levels(mf[[fac2]])
    
    grid <- expand.grid(levels_fac2, levels_fac1)
    names(grid) <- c(fac2, fac1)
    
    # make sure factors are preserved
    grid[[fac1]] <- factor(grid[[fac1]], levels = levels_fac1)
    grid[[fac2]] <- factor(grid[[fac2]], levels = levels_fac2)
    
    
    grid$preds = predict(model, newdata = grid, type = "response")
    
    colors = rainbow(length(levels_fac2))
    
    #start plotting:
    
    png('./output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    par(mar = c(5, 4, 4, 8), xpd = TRUE)  # Expand right margin
    
    boxplot(formula.vlm(model), data = mf, xaxt = "n", col = colors, 
            xlab = fac1, las = 1)
    
    # set custom axis labels — one per fac1 group
    n_fac2 = length(levels(data[[fac2]]))
    n_fac1 = length(levels(data[[fac1]]))
    
    # compute positions (middle points) per fac1 group
    group_positions = seq(1, n_fac2 * n_fac1, by = n_fac2) + (n_fac2 - 1) / 2
    
    # draw axis: one label per group of fac1 
    axis(1, at = group_positions, labels = levels_fac1)
    
    # add model preiction
    points(x = 1:length(grid$preds), y = grid$pred, pch = 16, cex = 3, col = "orange")
    
    
    # legend (placed outside)
    legend("right", legend = c(levels_fac2, "preds"), col = c(colors, "orange"), title = fac2, 
           pch = c(rep(15, n_fac2), 16), pt.cex = 2, 
           inset = c(-0.25, 0), 
           bty = "n")
    
    #set par back:
    par(xpd = FALSE)
    
    # add vertical lines to separate groups
    for (i in 1:(n_fac1 - 1)) {
      abline(v = i * n_fac2 + 0.5, col = "black", lty = 2)
    }
    
    #stop recording:
    dev.off()
  }
  if (length(factors)==1 & length(cont)==0){
    
    #factor variable names
    fac1 = factors[1]
    
    # levels per variable
    levels_fac1 = levels(mf[[fac1]])
    
    grid = expand.grid(dummy_fac1 = levels_fac1)
    names(grid) = c(fac1)
    grid
    
    grid$preds = predict(model, newdata = grid, type = "response")
    
    colors = rainbow(length(levels_fac1))
    
    #start plotting and recording
    png('./output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    par(mar = c(5, 4, 4, 8), xpd = TRUE)  # Expand right margin
    boxplot(formula.vlm(model), data = mf, col = colors, 
            xlab = fac1, las = 1, 
            main = paste("predictions for every level of", fac1))
    
    # add model preiction
    points(x = 1:length(grid$preds), y = grid$pred, pch = 16, cex = 3, col = "orange")
    
    # legend (placed outside)
    legend("right", legend = c("preds"), col = c("orange"), title = "legend", 
           pch = c(16), pt.cex = 2, 
           inset = c(-0.25, 0), 
           bty = "n")
    
    #stop recording
    dev.off()
    
    #set par back:
    par(xpd = FALSE)
    par(mar = c(5.1, 4.1, 4.1, 2.1)) 
    
    
  } 
  

  if (verbose){cat("plotting done\n")}
  
  return(list)
}

