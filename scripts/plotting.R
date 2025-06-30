plotting = function(list, verbose = T){
  if (verbose){cat("entered plotting\n")}

  #store data first
  data = list$data_na.omit # contains names such as poly()1 and interactions, important for predict etc
  model = list$model
  data_na.omit = list$data_na.omit # just plain variable names (as in input data) and respective data
  
  # get the vars names
  vars = all.vars(model@misc$formula)[-1]
  vars_number = length(vars)
  
  # get the respective class of the vars to get plotted
  data_classes = sapply(data_na.omit[,vars, drop = FALSE], FUN = class)
  
  #split in cont and fac preds
  cont = names(which(data_classes=="numeric" | data_classes == "integer"))
  factors = names(which(data_classes=="factor" | data_classes=="ordered" | data_classes == "character"))
  
  if (verbose){
    cat("these are the cont:\n")
    cat(cont)
    cat("\nthese are the factors:\n")
    cat(factors)
  }
  
  # what can't get displayed yet: if more than 3 vars
  if (vars_number>3){
    warning("\nmodels with more than three variables as predictors can't get displayed by the AS!\n")
    #make mar smaller
    par(mar = c(1,1,1,1))
    
    # produce plot with text that this cannot get displayed:
    png('../output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    
    # draw plotting window
    plot(c(0, 10), c(0, 10), type = "n", ann = FALSE, bty = "o", xaxt = "n", yaxt = "n")
    
    # add text description to make the issue more explicit 
    text(x = 5, y = 5,
         labels = "The AS cannot display a plot for this model.\nThis is because you have more than 3 predictors.",
         cex = 1)
    
    #stop recording of the plot:
    dev.off()
    
    #set mar back to default
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  }
  
  ### if statements cont & cat facet plots####
  if (length(factors) == 2 & length(cont)== 1){
    # make scatter plot for cont pred & cont response, color  = pred cat1, factet for cat2
    print("enetered right if statement")
    #get the variables
    fac1 = factors[1]
    fac2 = factors[2]
    
    #create new values for prediction
    
    # continuous
    cont_new = seq(min(data_na.omit[cont]), max(data_na.omit[cont]), length = 10000)
    
    # categorical variable one level:
    levels_col_vec = levels(data_na.omit[[fac1]])
    levels_plot_vec = levels(data_na.omit[[fac2]]) 
    
    plot_rows_needed = ceiling((length(levels_plot_vec))/2) 
    if (plot_rows_needed == 0){
      plot_rows_needed = 1
    }
    
    #set par depending on number of plots and rows needed
      if (plot_rows_needed == 1){
        #start the "recording" using png (before setting the par)
        png('../output/plots/plot.png', width = 8, units = "in", height = 4, res = 300)
        par(mfrow = c(1,2))
      } else {
        #start recording with height depending on number of plots
        png('../output/plots/plot.png', width = 8, units = "in", height = 4*plot_rows_needed, res = 300)
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
      response_full = data_na.omit[, responseName(model)]
      cont_pred_full = data_na.omit[[cont]]
      
      #range to add empty plotting window
      x_lim_range = range(cont_pred_full)
      y_lim_range = range(response_full)
      plot(cont_pred_full, response_full, type = "n",  
           xlab = cont, ylab = responseName(model), 
           xlim = x_lim_range, ylim = y_lim_range, 
           main = paste0("model predictions with ",fac2, ": ",  level_plot), 
           pch = 1, las = 1)
      
      grid(col = "lightgrey") # add grid
      
      #add legend and create colors wit length of number of levels
      colors <- rainbow(length(levels_col_vec))
      legend("topleft", legend = levels_col_vec, col = darken(colors, amount = 0.2), 
             pch = rep(1, 2), lty = rep(1, 2), 
             title = fac1, 
             cex = 1)
      
      # add points first, then the prediction lines:
      counter = 0
      for (level in levels_col_vec){
        counter = counter + 1
        new_data <- data.frame(
          dummy_cont = cont_new,
          dummy_fac1 = level,
          dummy_fac2 = level_plot)
      
        
        
        #set names according to variable cat predictor name
        names(new_data) = c(cont, fac1, fac2)
        
        #just subset of data for cat preds
        cat_predictor_treat = cont_pred_full[which(data_na.omit[[fac2]]==level_plot & data_na.omit[[fac1]]==level)]
        cat_response_treat = response_full[which(data_na.omit[[fac2]]==level_plot & data_na.omit[[fac1]]==level)]
        
        # add the data points
        points(cat_predictor_treat, cat_response_treat, pch = 1, las = 1, col = colors[counter], cex = 0.7)
      }
      
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
        cat_predictor_treat = cont_pred_full[which(data_na.omit[[fac2]]==level_plot & data_na.omit[[fac1]]==level)]
        cat_response_treat = response_full[which(data_na.omit[[fac2]]==level_plot & data_na.omit[[fac1]]==level)]
        
        # add the data points
        points(cat_predictor_treat, cat_response_treat, pch = 1, las = 1, col = colors[counter], cex = 0.5)
        #if (ncol(data_na.omit)>100){
        #  points(cat_predictor_treat, cat_response_treat, pch = 1, las = 1, col = colors[counter], cex = 0.5)
        #}else {
        #  points(cat_predictor_treat, cat_response_treat, pch = ".", las = 1, col = colors[counter])
        #}
        
        # make cont_new to same range as points.
        val = which(cont_new >= min(cat_predictor_treat) & cont_new <= max(cat_predictor_treat))
        
        # Subset all using the same index
        cont_new_range = cont_new[val]
        fit_range = fit[val]
        upper_CI_range = upper_CI[val]
        lower_CI_range = lower_CI[val]
        
        
        #add lines
        lines(cont_new_range, fit_range, lwd = 2, col = darken(colors[counter], amount = 0.2))
        lines(cont_new_range, upper_CI_range, lwd = 0.7, lty = 2, col = darken(colors[counter], amount = 0.2))
        lines(cont_new_range, lower_CI_range, lwd = 0.7, lty = 2, col = darken(colors[counter], amount = 0.2))
      }
    }
    #stop reporting
    dev.off()
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) # set par() back
    
  } # poly done
  if (length(factors) == 1 & length(cont)== 1){
    
    if (verbose){cat("\nentered plotting: 1 cat & 1 cont\n")}
    
    # make scatterplot for cont pred & cont response, color  = pred cat1
    
    #get the levels
    fac1 = factors[1]
    
    #create new values for prediction
    
    # continuous
    cont_new = seq(min(data_na.omit[cont]), max(data_na.omit[cont]), length = 10000)
    
    # categorical variable one level:
    levels_col_vec = levels(data_na.omit[[fac1]])
    
    #add empty plot to have comparable plot windows
    #original data:
    response_full = data_na.omit[,responseName(model)]
    cont_pred_full = data_na.omit[[cont]]
    
    #start recording
    png('../output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    
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
         pch = 1, las = 1)
    
    #add legend and create colors wit length of number of levels
    colors <- rainbow(length(levels_col_vec))
    legend("right", legend = levels_col_vec, col = darken(colors, amount = 0.2), 
           pch = rep(1, 2), lty = rep(1, 2), 
           title = fac1, 
           inset = c(-0.3, 0), bty = "n")
    
    par(xpd = FALSE) # set expand back to default FALSE, so that drawing in margins is disabled
    
    grid(col = "lightgrey") # add grid 
    
    # add the points first
    counter = 0
    for (level in levels_col_vec){
      counter = counter + 1
      new_data <- data.frame(
        dummy_cont = cont_new,
        dummy_fac1 = level)
      
      #set names according to variable cat predictor name
      names(new_data) = c(cont, fac1)
    
      #just subset of data for cat preds
      cat_predictor_treat = cont_pred_full[data_na.omit[[fac1]]==level]
      cat_response_treat = response_full[data_na.omit[[fac1]]==level]
    
    
      # add the data points
      points(cat_predictor_treat, cat_response_treat, pch = 1, las = 1, col = colors[counter])
      
    }
    
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
      cat_predictor_treat = cont_pred_full[data_na.omit[[fac1]]==level]
      cat_response_treat = response_full[data_na.omit[[fac1]]==level]
      
      
      # add the data points
      #points(cat_predictor_treat, cat_response_treat, pch = 16, las = 1, col = colors[counter])
      
      # make cont_new to same range as points.
      val = which(cont_new >= min(cat_predictor_treat) & cont_new <= max(cat_predictor_treat))
      
      # Subset all using the same index
      cont_new_range = cont_new[val]
      fit_range = fit[val]
      upper_CI_range = upper_CI[val]
      lower_CI_range = lower_CI[val]
      

      #add lines
      lines(cont_new_range, fit_range, lwd = 2, col = darken(colors[counter], amount = 0.2))
      lines(cont_new_range, upper_CI_range, lwd = 0.7, lty = 2, col = darken(colors[counter], amount = 0.2))
      lines(cont_new_range, lower_CI_range, lwd = 0.7, lty = 2, col = darken(colors[counter], amount = 0.2))
    }
    # stop plot recording
    dev.off()
  } # poly done
  if (length(cont)==1 & length(factors)==0){
    cont_x = cont[1]
    
    #make prediction sequence for both cont pred variables
    cont_x_seq <- seq(min(data_na.omit[[cont_x]]), max(data_na.omit[[cont_x]]), length = 1000)
    new_data = data.frame(dummy_cont_x = cont_x_seq)
    names(new_data) = c(cont_x)
    
    #make prediction
    preds = predict(model, newdata = new_data, se.fit = T)
    
    #plot
    x = data_na.omit[[cont_x]]
    y = data_na.omit[[responseName(model)]]
    
    # back transform to response scale and compute CI's
    fit = model@family@linkinv(preds$fitted.values, extra = model@extra)
    upper_CI = model@family@linkinv(preds$fitted.values + 1.96*preds$se.fit , extra = model@extra)
    lower_CI = model@family@linkinv(preds$fitted.values - 1.96*preds$se.fit , extra = model@extra)
    
    #start recording
    png('../output/plots/plot.png', width = 4, units = "in", height = 4, res = 300)
    
    #make the plot
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
  } # 
  
  ### if statements more 2 cont: contour / heatmap ####
  if (length(cont)==3 & length(factors)== 0){
    #heatplot with contours for cont1 & cont2 with z being response, cont3 is made discrete through quantile and plotted to three distinct contourplots
    
    #split the thirds continuous in quantile and plot a seperate contourplot for each quantile
    cont_x = cont[1]
    cont_y = cont[2]
    cont_quant = cont[3]
    
    cont_x_seq <- seq(min(data_na.omit[[cont_x]]), max(data_na.omit[[cont_x]]), length = 30)
    cont_y_seq <- seq(min(data_na.omit[[cont_y]]), max(data_na.omit[[cont_y]]), length = 30)
    
    quantiles = c(0.25, 0.5, 0.75)
    cont_quant_values = quantile(data_na.omit[[cont_quant]], probs = quantiles, na.rm = T) # make quantiles for visualization
    
    #save plots
    # since 3 quantiles are used: 3 rows
    png('../output/plots/plot.png', width = 10, units = "in", height = 4*2, res = 300)
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
                   
                   # add median for other variables
                   
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
      points(data_na.omit[[cont_x]], data_na.omit[[cont_y]], pch="+", cex=1)
      ch <- chull(data_na.omit[[cont_x]], data_na.omit[[cont_y]])
      polygon(cbind(data_na.omit[[cont_x]], data_na.omit[[cont_y]])[ch,])
      
    }
    #stop recording
    dev.off()
    par(mfrow = c(1,1))
  } # poly done! 
  if (length(cont)==2 & length(factors)==0){
    # one contourplot produced (cont1 against cont2 and z = response)
    
    #split the data to the two seperate preds
    cont_x = cont[1]
    cont_y = cont[2]
    
    #make prediction sequence for both cont pred variables
    cont_x_seq <- seq(min(data_na.omit[[cont_x]], na.rm = T), max(data_na.omit[[cont_x]], na.rm = T), length = 30)
    cont_y_seq <- seq(min(data_na.omit[[cont_y]], na.rm = T), max(data_na.omit[[cont_y]], na.rm = T), length = 30)
    
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
    png('../output/plots/plot.png', width = 6, units = "in", height = 6, res = 300)
    
    #make plot
    image(cont_x_seq, cont_y_seq, z, cex.lab = 1.5,
          ylab = paste(cont_y), xlab = paste(cont_x),
          col = heat.colors(30), las = 1, 
          main = paste("response", responseName(model), "depending on", cont_y, "&", cont_x))
    #add grid
    grid(col = "lightgrey")
    #add contours
    contour(cont_x_seq, cont_y_seq, z, add=T)
    #add the points
    points(data_na.omit[[cont_x]], data_na.omit[[cont_y]], pch="+", cex=1)
    #add the margin line for points
    ch <- chull(cbind(data_na.omit[[cont_x]], data_na.omit[[cont_y]]))
    polygon(cbind(data_na.omit[[cont_x]], data_na.omit[[cont_y]])[ch,])
    
    #stop recording
    dev.off()
  } # poly done!
  if (length(cont)==2 & length(factors)==1){
    if(verbose){cat('\nenter plotting: 2 cont & 1 cat')}
    
    
    # contourplot / heatmap for cont1 & cont2 (z being response), for each level of cat1 new conturplot
    #(make the same as for 3 cont vars, where the factor are the quantiles)
    #heatplot with contours for cont1 & cont2 with z being response, cont3 is made discrete through quantile and plotted to three distinct contourplots
    
    #split the thirds continuous in quantile and plot a seperate contourplot for each quantile
    cont_x = cont[1]
    cont_y = cont[2]
    cat1 = factors[1]
    
    cont_x_seq <- seq(min(data_na.omit[[cont_x]]), max(data_na.omit[[cont_x]]), length = 50)
    cont_y_seq <- seq(min(data_na.omit[[cont_y]]), max(data_na.omit[[cont_y]]), length = 50)
    
    cat1_levels = levels(data_na.omit[[cat1]]) # get the levels of cat1
    
    #rows needed:
    rows_needed <- ceiling(length(cat1_levels) / 2)
    if (rows_needed == 0){
      rows_needed = 1 # set 1 nrwo to minim, otherwise an error occurs
    }
    
    png('../output/plots/plot.png', width = 10, units = "in", height = 4*rows_needed, res = 300)
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
            col = heat.colors(30), las = 1, 
            main = paste0("response ",responseName(model), " with cat predictor ", cat1, ' & level "', cat1_levels[i], '"'))
      grid(col = "lightgrey")
      contour(cont_x_seq, cont_y_seq, z, add=T,)
      points(data_na.omit[[cont_x]], data_na.omit[[cont_y]], pch="+", cex=1)
      ch <- chull(cbind(data_na.omit[[cont_x]], data_na.omit[[cont_y]]))
      polygon(cbind(data_na.omit[[cont_x]], data_na.omit [[cont_y]])[ch,])
      
    }
    
    #stop recording
    dev.off()
    
  } # poly done! but weird behaviour!
  
  ### if statements just factors: boxplots ####
  if(length(factors) == 3 & length(cont) == 0){
    
    if(verbose){cat("\nenter plotting: 3 cat\n")}
    
    # factor variable names
    fac1 <- factors[1]
    fac2 <- factors[2]
    fac3 <- factors[3]
    
    # levels per variable
    levels_fac1 <- levels(data_na.omit[[fac1]])
    levels_fac2 <- levels(data_na.omit[[fac2]])
    levels_fac3 <- levels(data_na.omit[[fac3]])
    
    # number of rows needed for plotting
    rows_needed <- ceiling(length(levels_fac3) / 2)
    if (rows_needed == 0){
      rows_needed = 1 # set 1 nrwo to minim, otherwise an error occurs
    }
    
    png('../output/plots/plot.png', width = 10, units = "in", height = 4 * rows_needed, res = 300)
    par(mfrow = c(rows_needed, 2))
    
    for (level_fac3 in levels_fac3) {
      
      #subset model frame to current fac3 level
      mf_sub <- data_na.omit[data_na.omit[[fac3]] == level_fac3, ]
      
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
              ylim = range(c(data_na.omit[[responseName(model)]], grid$upper_CI, grid$lower_CI)),
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
  } # poly done
  if(length(factors)==2 & length(cont)==0){
    
    # factor variable names
    fac1 = factors[1]
    fac2 = factors[2]
    
    
    
    #levels per variable
    levels_fac1 = levels(data_na.omit[[fac1]])
    levels_fac2 = levels(data_na.omit[[fac2]])
    
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
    data_na.omit$interaction_group <- interaction(data_na.omit[[fac1]], data_na.omit[[fac2]], sep = "_")
    grid$interaction_group <- interaction(grid[[fac1]], grid[[fac2]], sep = "_")
    
    # order boxplot by interaction
    ordered_groups <- levels(interaction(data_na.omit[[fac1]], data_na.omit[[fac2]], sep = "_"))
    
    png('../output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    par(mar = c(5, 4, 4, 8), xpd = TRUE)
    
    boxplot(data_na.omit[[responseName(model)]] ~ data_na.omit$interaction_group, col = rep(colors, times = length(levels_fac1)), 
            xaxt = "n", xlab = fac1, las = 1, ylim = range(c(data_na.omit[[responseName(model)]], grid$upper_CI, grid$lower_CI)), 
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
  } # poly done
  if (length(factors)==1 & length(cont)==0){
    
    #factor variable names
    fac1 = factors
    
    # levels per variable
    levels_fac1 = levels(data_na.omit[[fac1]])
    
    grid = expand.grid(dummy_fac1 = levels_fac1)
    names(grid) = c(fac1)
    
    preds = predict(model, newdata = grid, type = "link", se.fit = T)
    
    #back transform from link to response scale
    preds_response = model@family@linkinv(preds$fitted.values, extra = model@extra)
    upper_CI_response = model@family@linkinv(preds$fitted.values + 1.96 * preds$se.fit, extra = model@extra)
    lower_CI_response = model@family@linkinv(preds$fitted.values - 1.96 * preds$se.fit, extra = model@extra)
    
    colors = rainbow(length(levels_fac1))
    
    #start plotting and recording
    png('../output/plots/plot.png', width = 6, units = "in", height = 4, res = 300)
    par(mar = c(5, 4, 4, 8), xpd = TRUE)  # Expand right margin
    boxplot(formula.vlm(model), data = data_na.omit, col = colors, 
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
    
    
  } # 
  
  return(list)
}

