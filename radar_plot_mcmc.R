# Function to plot multiple datasets on the same radar chart
multi_radar_plot <- function(categories, values_list, names = NULL, title = "", 
                             circle = FALSE, colors = NULL, webclr = "grey80", fill = FALSE, alpha = 0.05) {
  # Number of categories and datasets
  n <- length(categories)
  m <- length(values_list)
  nmc <- nrow(values_list[[1]])
  # Set default names if not provided
  if (is.null(names)) {
    names <- paste("Data", 1:m)
  }
  # Set default colors if not provided
  if (is.null(colors)) {
    colors <- viridis::viridis(m)
  }
  max_values <- apply(do.call("rbind", values_list), 2, max)

  # Calculate angles for each category
  angles <- seq(0, 2 * pi, length.out = n + 1)
  
  # Create a blank plot
  plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
       xlab = "", ylab = "", main = title, axes = FALSE)
  
  # Draw axis lines for each category
  grid_values <- seq(0, 1, length.out = 5)[-1]
  if (circle){
    for (r in grid_values) {
      points(x = r * cos(seq(0, 2 * pi, length.out = 100)),
             y = r * sin(seq(0, 2 * pi, length.out = 100)),
             type = "l", lty = 1, col = webclr)
    }
  }
  for (i in 1:n) {
    lines(c(0, cos(angles[i])), c(0, sin(angles[i])), col = webclr)
    if(!circle){
        x_one <- grid_values * cos(angles[i])
        x_two <- grid_values * cos(angles[i + 1])
        y_one <- grid_values * sin(angles[i])
        y_two <- grid_values * sin(angles[i + 1])
        segments(x_one, y_one, x_two, y_two, col = webclr)
    }
    # Add category labels
    text_x <- 1.1  * cos(angles[i])
    text_y <- 1.1  * sin(angles[i])
    
    # Adjust text alignment
    hjust <- 0.5
    vjust <- 0.5
    if (cos(angles[i]) < -0.1) hjust <- 1
    if (cos(angles[i]) > 0.1) hjust <- 0
    if (sin(angles[i]) < -0.1) vjust <- 1
    if (sin(angles[i]) > 0.1) vjust <- 0
    text(text_x, text_y, labels = categories[i], adj = c(hjust, vjust))
    #add max axis values
    text(cos(angles[i]), sin(angles[i]), labels = round(max_values[i]), col = webclr, cex = 0.8, adj = c(hjust, vjust))  
  }
  
  # Plot each dataset
  for (i in 1:m) {
    values <- values_list[[i]]
    for (j in 1:ncol(values)){
        values[,j] <- values[,j] / max_values[j]
    }
    for(imc in 1:nmc){  
    # Calculate coordinates
      data_x <- values[imc, ] * cos(angles[1:n])
      data_y <- values[imc, ] * sin(angles[1:n])
    
    # Closed polygon coordinates
      data_x_closed <- c(data_x, data_x[1])
      data_y_closed <- c(data_y, data_y[1])
    
    # Fill polygon if requested
      if (fill) {
        polygon(data_x_closed, data_y_closed, 
                col = adjustcolor(colors[i], alpha.f = alpha), 
                border = NA)
      }
    
      # Draw outline
      lines(data_x_closed, data_y_closed, col = adjustcolor(colors[i], alpha.f = alpha), lwd = 2)
    
      # Add points
      #points(data_x, data_y, pch = 16, col = colors[i])
    }
  }
  
  # Add legend
  legend(par("usr")[2] * 0.7, par("usr")[4] + (par("usr")[4] - par("usr")[3]) * 0.2,
        legend = names, col = colors, lty = 1, lwd = 2, pch = 16, bty = "n")
}

#Multiple scenarios mcmc results
scenarios <- c("Scenario A", "Scenario B", "Scenario C", "Scenario D")
categories <- c("Catch", "Cpue", "Revenue", "Collapse", "Variability")

values1 <- sapply(c(80, 7.2, 9.0, 6.8, 8.3),FUN = function(x) rnorm(100,x,x*0.05))
values2 <- sapply(c(90, 8.5, 5.2, 3.0, 2.5),FUN = function(x) rnorm(100,x,x*0.05))
values3 <- sapply(c(100, 1.5, 8.2, 7.0, 4.5),FUN = function(x) rnorm(100,x,x*0.05))
values4 <- sapply(c(115, 5.5, 3.6, 2.0, 9.5),FUN = function(x) rnorm(100,x,x*0.05))

values_list <- list(values1, values2, values3, values4)

op <- list("omi" = par("omi"), "mar" = par("mar"), "xpd" = par("xpd"))
par(omi = c(1,1,1,1),mar = c(1, 1, 1, 1), xpd = NA)
multi_radar_plot(
   categories = categories ,
   values_list = values_list,
   names = scenarios,
   title = "",
   alpha = 0.05,
   fill = FALSE,
   circle = FALSE,
 )
par(op)
