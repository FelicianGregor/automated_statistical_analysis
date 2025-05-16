plotting = function(list, verbose = T){
  if (verbose){cat("entered plotting\n")}

  
  #store data first
  data = list$model@model
  model = list$model
  
  # start preps for plotting, getting the variables
  mf = model.frame(model)
  
  
  # get the variables:
  terms = terms(model)
  preds = attr(terms, "term.labels") #all variables with interactions (denoted with :)
  
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
  data_classes_simple = data_classes[preds_simple] #without interactions
  
  #split in cont and fac preds
  cont = names(which(data_classes_simple=="numeric"))
  factors = names(which(data_classes_simple=="factor" | data_classes_simple=="ordered" | data_classes_simple == "character"))
  
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
    
    plot_rows_needed = ceiling((length(levels_plot_vec))/2) 
    if (plot_rows_needed == 0){
      plot_rows_needed = 1
    }
    
    #set par depending on number of plots and rows needed
      if (plot_rows_needed == 1){
        #start the "recording" using png (before setting the par)
        png('./output/plots/plot.png', width = 8, units = "in", height = 4, res = 300)
        par(mfrow = c(1,2))
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
      
      grid(col = "lightgrey") # add grid
      
      #add legend and create colors wit length of number of levels
      colors <- rainbow(length(levels_col_vec))
      legend("topleft", legend = levels_col_vec, col = colors, 
             pch = rep(16, 2), lty = rep(1, 2), 
             title = fac1, 
             cex = 0.7)
      
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
        
        #predict on link scale
        preds = predict(model, newdata = new_data,  se.fit = T )
        
        # backtransform
        fit = model@family@linkinv(preds$fitted.values, extra = model@extra)
        upper_CI = model@family@linkinv(preds$fitted.values + 1.96*preds$se.fit , extra = model@extra)
        lower_CI = model@family@linkinv(preds$fitted.values - 1.96*preds$se.fit , extra = model@extra)
        
        #plot
        
        #just subset of data for cat preds
        cat_predictor_treat = cont_pred_full[which(model@model[[fac2]]==level_plot & model@model[[fac1]]==level)]
        cat_response_treat = response_full[which(model@model[[fac2]]==level_plot & model@model[[fac1]]==level)]
        
        # add the data points
        points(cat_predictor_treat, cat_response_treat, pch = 16, las = 1, col = colors[counter])
        
        #add lines
        lines(cont_new, fit, lwd = 2, col = colors[counter])
        lines(cont_new, upper_CI, lwd = 0.7, lty = 2, col = colors[counter])
        lines(cont_new, lower_CI, lwd = 0.7, lty = 2, col = colors[counter])
      }
    }
    #stop reporting
    dev.off()
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) # set par() back
    
  } # done CI!
  if (length(factors) == 1 & length(cont)== 1){
    
    # make scatterplot for cont pred & cont response, color  = pred cat1
    
    #get the levels
    fac1 = factors[1]
    
    #create new values for prediction
    
    # continuous
    cont_new = seq(min(model@model[cont]), max(model@model[cont]), length = 10000)
    
    # categorical variable one level:
    levels_col_vec = levels(mf[[fac1]])
    
    #add empty plot to have comparable plot windows
    #original data:
    response_full = model@model[,1]
    cont_pred_full = model@model[[cont]]
    
    #start recording
    png('./output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    
    #set par and expand margin on the right side for legend
    # enable writing in the margin
    par(mar = c(5, 4, 4, 8), xpd = TRUE)
    
    #range to add empty plotting window
    x_lim_range = range(cont_pred_full)
    y_lim_range = range(response_full)
    plot(cont_pred_full, response_full, type = "n",  
         xlab = cont, ylab = responseName(model), 
         xlim = x_lim_range, ylim = y_lim_range, 
         main = paste0("model predictions depending on ", cont, " & ", fac1), 
         pch = 16, las = 1)
    
    #add legend and create colors wit length of number of levels
    colors <- rainbow(length(levels_col_vec))
    legend("right", legend = levels_col_vec, col = colors, 
           pch = rep(16, 2), lty = rep(1, 2), 
           title = fac1, 
           inset = c(-0.3, 0), bty = "n")
    
    par(xpd = FALSE) # set expand back to default FALSE, so that drawing in margins is disabled
    
    grid(col = "lightgrey") # add grid 
    
    #set counter for colors
    counter = 0
    for (level in levels_col_vec){
      counter = counter + 1
      new_data <- data.frame(
        dummy_cont = cont_new,
        dummy_fac1 = level)
      
      #set names according to variable cat predictor name
      names(new_data) = c(cont, fac1)
      
      #predict
      preds = predict(model, newdata = new_data, se.fit = T)
      
      # back transform to response scale and compute CI's
      fit = model@family@linkinv(preds$fitted.values, extra = model@extra)
      upper_CI = model@family@linkinv(preds$fitted.values + 1.96*preds$se.fit , extra = model@extra)
      lower_CI = model@family@linkinv(preds$fitted.values - 1.96*preds$se.fit , extra = model@extra)
      
      #plot
      
      #just subset of data for cat preds
      cat_predictor_treat = cont_pred_full[model@model[[fac1]]==level]
      cat_response_treat = response_full[model@model[[fac1]]==level]
      
      length(cat_response_treat)
      
      # add the data points
      points(cat_predictor_treat, cat_response_treat, pch = 16, las = 1, col = colors[counter])
      
      #add lines
      lines(cont_new, fit, lwd = 2, col = colors[counter])
      lines(cont_new, upper_CI, lwd = 0.7, lty = 2, col = colors[counter])
      lines(cont_new, lower_CI, lwd = 0.7, lty = 2, col = colors[counter])
    }
    # stop plot recording
    dev.off()
  } # done CI!
  if (length(cont)==1 & length(factors)==0){
    cont_x = cont[1]
    
    #make prediction sequence for both cont pred variables
    cont_x_seq <- seq(min(data[[cont_x]]), max(data[[cont_x]]), length = 1000)
    new_data = data.frame(dummy_cont_x = cont_x_seq)
    names(new_data) = c(cont_x)
    
    #make prediction
    preds = predict(model, newdata = new_data, se.fit = T)
    
    #plot
    x = model@model[[cont_x]]
    y = model@y
    
    # back transform to response scale and compute CI's
    fit = model@family@linkinv(preds$fitted.values, extra = model@extra)
    upper_CI = model@family@linkinv(preds$fitted.values + 1.96*preds$se.fit , extra = model@extra)
    lower_CI = model@family@linkinv(preds$fitted.values - 1.96*preds$se.fit , extra = model@extra)
    
    #start recording
    png('./output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    
    plot(x, y,
         xlab = paste("predictor", cont_x), 
         ylab = paste("response", responseName(model)), 
         col = "grey",
         pch = 16, 
         las = 1)
    lines(cont_x_seq, fit, lwd = 2, col = "red")
    lines(cont_x_seq, upper_CI, lwd = 2, col = "pink", lty = 2)
    lines(cont_x_seq, lower_CI, lwd = 2, col = "pink", lty = 2)
    
    #stop recording:
    dev.off()
  } # done CI!
  
  ### if statements more 2 cont: contour / heatmap ####
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
  } # no CI's
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
          col = heat.colors(30), 
          main = paste("response", responseName(model), "depending on", cont_y, "&", cont_x))
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
  } # no CI's
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
    rows_needed <- ceiling(length(levels_fac3) / 2)
    if (rows_needed == 0){
      rows_needed = 1 # set 1 nrwo to minim, otherwise an error occurs
    }
    
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
    
  } # no CI's
  
  ### if statements just factors: boxplots ####
  if(length(factors) == 3 & length(cont) == 0){
    
    # factor variable names
    fac1 <- factors[1]
    fac2 <- factors[2]
    fac3 <- factors[3]
    
    # levels per variable
    levels_fac1 <- levels(mf[[fac1]])
    levels_fac2 <- levels(mf[[fac2]])
    levels_fac3 <- levels(mf[[fac3]])
    
    # number of rows needed for plotting
    rows_needed <- ceiling(length(levels_fac3) / 2)
    if (rows_needed == 0){
      rows_needed = 1 # set 1 nrwo to minim, otherwise an error occurs
    }
    
    png('./output/plots/plot.png', width = 10, units = "in", height = 4 * rows_needed, res = 300)
    par(mfrow = c(rows_needed, 2))
    
    for (level_fac3 in levels_fac3) {
      
      #subset model frame to current fac3 level
      mf_sub <- mf[mf[[fac3]] == level_fac3, ]
      
      # create prediction grid for fac1 and fac2
      grid <- expand.grid(levels_fac2, levels_fac1)
      names(grid) <- c(fac2, fac1)
      
      # add current fac3 level to the grid
      grid[[fac3]] <- factor(level_fac3, levels = levels_fac3)
      
      # ensure correct factor levels
      grid[[fac1]] <- factor(grid[[fac1]], levels = levels_fac1)
      grid[[fac2]] <- factor(grid[[fac2]], levels = levels_fac2)
      grid[[fac3]] <- factor(grid[[fac3]], levels = levels_fac3)
      
      # predictions
      # predictions
      #grid$preds = predict(model, newdata = grid, type = "link", se.fit = T)$fit # glm version
      grid$preds = predict(model, newdata = grid, type = "link", se.fit = T)$fitted.values
      grid$preds_se = predict(model, newdata = grid, type = "link", se.fit = T)$se.fit
      
      #backtransform to response scale
      grid$fit_response = model@family@linkinv(grid$preds, extra = model@extra)
      grid$upper_CI = model@family@linkinv(grid$preds + 1.96 * grid$preds_se, extra = model@extra)
      grid$lower_CI = model@family@linkinv(grid$preds - 1.96 * grid$preds_se, extra = model@extra)
      
      # colors for fac2 levels
      colors <- rainbow(length(levels_fac2))
      
      # create interaction group for boxplot ordering
      mf_sub$interaction_group <- interaction(mf_sub[[fac1]], mf_sub[[fac2]], sep = "_")
      grid$interaction_group <- interaction(grid[[fac1]], grid[[fac2]], sep = "_")
      
      ordered_groups <- levels(interaction(mf_sub[[fac1]], mf_sub[[fac2]], sep = "_"))
      
      # plot
      par(mar = c(5, 4, 4, 8), xpd = TRUE)
      
      
      boxplot(mf_sub[[responseName(model)]] ~ mf_sub$interaction_group,
              col = rep(colors, times = length(levels_fac1)),
              xaxt = "n", xlab = fac1, las = 1,
              ylim = range(c(mf[[responseName(model)]], grid$upper_CI, grid$lower_CI)),
              ylab = responseName(model),
              main = paste(fac3, "=", level_fac3))
      
      # Match prediction order
      pred_order <- match(ordered_groups, grid$interaction_group)
      points(1:length(pred_order), grid$fit_response[pred_order], pch = 16, cex = 2, col = "orange")
      #points(1:length(pred_order), grid$upper_CI[pred_order], pch = "_", cex = 2, col = "orange")
      #points(1:length(pred_order), grid$lower_CI[pred_order], pch = "_", cex = 2, col = "orange")
      
      #connect lower and upper CI's to make brackets
      segments(x0 = 1:length(pred_order), 
               x1 = 1:length(pred_order),
               y0 = grid$lower_CI[pred_order], 
               y1 = grid$upper_CI[pred_order], 
               col = "orange", lwd = 2)
      
      
      # Axis for fac1 group labels
      n_fac2 <- length(levels_fac2)
      group_positions <- seq(1, length(ordered_groups), by = n_fac2) + (n_fac2 - 1) / 2
      axis(1, at = group_positions, labels = levels_fac1)
      
      # Legend
      legend("right", legend = c(levels_fac2, "preds &\n95% CI"), col = c(colors, "orange"), title = fac2, 
             pch = c(rep(15, length(colors)), NA), pt.cex = 2, inset = c(-0.5, 0), bty = "n")
      
      # plot legend again
      legend("right", legend = c(levels_fac2, "preds &\n95% CI"), col = c(colors, "orange"), title = fac2, 
             pch = c(rep(15, length(colors)), 16), pt.cex = 2, inset = c(-0.5, 0), bty = "n")
      legend("right", legend = c(levels_fac2, "preds &\n95% CI"), col = c(colors, "orange"), title = fac2, 
             pch = c(rep(NA, length(colors)), "|"), pt.cex = 2, inset = c(-0.5, 0), bty = "n")
      
      #set xpd = FALSE so that i dont draw lines in mar
      par(xpd = FALSE)
      # Add vertical lines
      for (i in 1:(length(levels_fac1) - 1)) {
        abline(v = i * length(levels_fac2) + 0.5, col = "black", lty = 2)
      }
    }
    
    dev.off()
  } # CIs done
  if(length(factors)==2 & length(cont)==0){
    
    # factor variable names
    fac1 = factors[1]
    fac2 = factors[2]
    
    #levels per variable
    levels_fac1 = levels(mf[[fac1]])
    levels_fac2 = levels(mf[[fac2]])
    
    #create grid for predictions
    grid <- expand.grid(levels_fac2, levels_fac1)
    names(grid) <- c(fac2, fac1)
    
    # predictions
    grid$preds = predict(model, newdata = grid, type = "link", se.fit = T)$fitted.values
    grid$preds_se = predict(model, newdata = grid, type = "link", se.fit = T)$se.fit
    
    #backtransform to response scale
    grid$fit_response = model@family@linkinv(grid$preds, extra = model@extra)
    grid$upper_CI = model@family@linkinv(grid$preds + 1.96 * grid$preds_se, extra = model@extra)
    grid$lower_CI = model@family@linkinv(grid$preds - 1.96 * grid$preds_se, extra = model@extra)
    
    #grid$preds = predict(model, newdata = grid, type = "link", se.fit = T)
    
    #add colors from rainbow
    colors = rainbow(length(levels_fac2))
    
    #prepare grouping variable for correct boxplot order
    mf$interaction_group <- interaction(mf[[fac1]], mf[[fac2]], sep = "_")
    grid$interaction_group <- interaction(grid[[fac1]], grid[[fac2]], sep = "_")
    
    # order boxplot by interaction
    ordered_groups <- levels(interaction(mf[[fac1]], mf[[fac2]], sep = "_"))
    
    png('./output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    par(mar = c(5, 4, 4, 8), xpd = TRUE)
    
    boxplot(mf[[responseName(model)]] ~ mf$interaction_group, col = rep(colors, times = length(levels_fac1)), 
            xaxt = "n", xlab = fac1, las = 1, ylim = range(c(mf[[responseName(model)]], grid$upper_CI, grid$lower_CI)), 
            ylab = responseName(model))
    
    #add predictions in correct order
    pred_order = match(ordered_groups, grid$interaction_group)
    points(1:length(pred_order), grid$fit_response[pred_order], pch = 16, cex = 2, col = "orange")
    
    segments(x0 = 1:length(pred_order), 
             x1 = 1:length(pred_order),
             y0 = grid$lower_CI[pred_order], 
             y1 = grid$upper_CI[pred_order], 
             col = "orange", lwd = 3)
    
    #axis with fac1 group labels at group centers
    n_fac2 = length(levels_fac2)
    group_positions = seq(1, length(ordered_groups), by = n_fac2) + (n_fac2 - 1) / 2
    axis(1, at = group_positions, labels = levels_fac1)
    
    # legend
    legend("right", legend = c(levels_fac2, "preds &\n95% CI"), col = c(colors, "orange"), title = fac2, 
           pch = c(rep(15, length(colors)), 16), pt.cex = 2, inset = c(-0.3, 0), bty = "n")
    
    #tweak to get the CI vertical line on top of the pch 16 symbol: plot opaque legend again, but with changed symbol to pch = "|"
    legend("right", legend = c(levels_fac2, "preds &\n95% CI"), col = c(colors, "orange"), title = fac2, 
           pch = c(rep(15, length(colors)), NA), pt.cex = 2, inset = c(-0.3, 0), bty = "n")
    
    #tweak to get the CI vertical line on top of the pch 16 symbol: plot opaque legend again, but with changed symbol to pch = "|"
    legend("right", legend = c(levels_fac2, "preds &\n95% CI"), col = c(colors, "orange"), title = fac2, 
           pch = c(rep(NA, length(colors)), "|"), pt.cex = 2, inset = c(-0.3, 0), bty = "n")
    
    par(xpd = FALSE)
    
    #vertical lines
    for (i in 1:(length(levels_fac1) - 1)) {
      abline(v = i * length(levels_fac2) + 0.5, col = "black", lty = 2)
    }
    
    dev.off()
  } # CI's done!
  if (length(factors)==1 & length(cont)==0){
    
    #factor variable names
    fac1 = factors[1]
    
    # levels per variable
    levels_fac1 = levels(mf[[fac1]])
    
    grid = expand.grid(dummy_fac1 = levels_fac1)
    names(grid) = c(fac1)
    grid
    
    preds = predict(model, newdata = grid, type = "link", se.fit = T)
    
    #back transform from link to response scale
    preds_response = model@family@linkinv(preds$fitted.values, extra = model@extra)
    upper_CI_response = model@family@linkinv(preds$fitted.values + 1.96 * preds$se.fit, extra = model@extra)
    lower_CI_response = model@family@linkinv(preds$fitted.values - 1.96 * preds$se.fit, extra = model@extra)
    
    colors = rainbow(length(levels_fac1))
    
    #start plotting and recording
    png('./output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    par(mar = c(5, 4, 4, 8), xpd = TRUE)  # Expand right margin
    boxplot(formula.vlm(model), data = mf, col = colors, 
            xlab = fac1, las = 1, 
            main = paste("predictions for every level of", fac1))
    
    # add model preiction
    points(x = 1:length(preds_response), y = preds_response, pch = 16, cex = 2, col = "orange")
    #points(x = 1:length(preds_response), y = upper_CI_response, pch = "_", cex = 2, col = "orange")
    #points(x = 1:length(preds_response), y = lower_CI_response, pch = "_", cex = 2, col = "orange")
    
    # add brackets (connect upper and lower with vertical line)
    segments(x0 = 1:length(preds_response), 
             x1 = 1:length(preds_response),
             y0 = lower_CI_response, 
             y1 = upper_CI_response, 
             col = "orange", lwd = 3)
    
    # legend (placed outside)
    legend("right", legend = c("preds &\n95% CI"), col = c("orange"), title = "legend", 
           pch = "|", pt.cex = 2, 
           inset = c(-0.35, 0), 
           bty = "n")
    
    legend("right", c("preds &\n95% CI"), col = c("orange"), title = "legend", 
           pch = 16, pt.cex = 2,
           inset = c(-0.35, 0), 
           bty = "n")
    
    #stop recording
    dev.off()
    
    #set par back:
    par(xpd = FALSE)
    par(mar = c(5.1, 4.1, 4.1, 2.1)) 
    
    
  } # CI's done!
  
  return(list)
}

