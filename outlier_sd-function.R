outlier_sd <- function(var, sd_diff = 2, drop = F, NA.replace = F, plot = F) {
  cutoff_low <- mean(var) - sd(var) * sd_diff
  cutoff_high <- mean(var) + sd(var) * sd_diff
  outliers <- which(var < cutoff_low | var > cutoff_high)
  values <- var[outliers]
  output <- data.frame(Position = outliers, Value = values)
  
  x <- as.numeric(var)
  data <- data.frame(x)
  name <- deparse(substitute(var))
  
  if(drop == T) {
    
    if(NA.replace == F) {
      var <- var[-outliers]
    }
    else {
      var[outliers] <- NA
    }
    
  }
  else {
    var <- paste("Use 'drop = T' to get the variable without outliers SD >", sd_diff)
    print(output)
  }
  
  
  if(drop == F && NA.replace == T) {
    print("You must set 'drop = T' in order to replace outliers with 'NA'.")
  }
  
  if(plot == T) {
    p <- ggplot(data = data, aes(x = x), environment = environment()) +
      stat_bin(binwidth = .05, col = "black", fill = "black") +
      geom_vline(xintercept = cutoff_low, 
                 color = "red", 
                 linetype = "dashed") +
      geom_vline(xintercept = cutoff_high, 
                 color = "red", 
                 linetype = "dashed") +
      geom_text(aes(x = cutoff_low,
                    y = 13,
                    label = "Cutoff",
                    hjust = -.225,
                    vjust = 0,
                    angle = 0,
                    fontface = 1), 
                color = "red",
                size = 4) +
      geom_text(aes(x = cutoff_high,
                    y = 13,
                    label = "Cutoff",
                    hjust = 1.3,
                    vjust = 0,
                    angle = 0,
                    fontface = 1), 
                color = "red",
                size = 4) +
      labs(title = paste("Outlier analysis for", name), 
           subtitle = paste("SD >", sd_diff),
           x = "value",
           y = "absolute frequency") +
      theme_light() +
      theme(axis.text.x=element_text(size=rel(.75), 
                                     angle=90, 
                                     margin = margin(b = 12))) +
      theme(plot.title = element_text(size = 18, 
                                      face = "bold"))
    
    print(p)
  }
  
  return(var)
  
}