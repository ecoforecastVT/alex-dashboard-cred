degToCompass <- function(num){
  
  arr = c("N","NNE","NE","ENE","E","ESE", "SE", "SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
  
  val = as.numeric((num/22.5) + 0.5)
  val_position <- val %% 16
  
  
  if (any(val > 16)){
    val_over <- which(val > 16)
    
    for (i in val_over){
      print(i)
      val_position[i] <- 16 ## hard code the index to 16 (last value in the wind directions)
      print(length(val_position[i]))
    }
  } 
  
  if (any(val < 1)){
    val_under <- which(val < 1)
    for (i in val_under){
      #print(i)
      val_position[i] <- 1 ## hard code the index to 16 (last value in the wind directions)
      #print(length(val_position[i]))
    }  
  }
  
  wind_direction <- arr[val_position] 
  
  return(wind_direction)
}




# library(ggplot2)
# 
# 
# 
# # Function to create compass labels
# 
# compass_labels <- function(angle) {
#   
#   if (angle < 22.5 || angle > 337.5) { return("N") } 
#   
#   else if (angle < 67.5) { return("NE") }
#   
#   else if (angle < 112.5) { return("E") }
#   
#   else if (angle < 157.5) { return("SE") }
#   
#   else if (angle < 202.5) { return("S") }
#   
#   else if (angle < 247.5) { return("SW") }
#   
#   else if (angle < 292.5) { return("W") }
#   
#   else { return("NW") }
#   
# }
# 
# 
# 
# # Create data for compass plot
# 
# df <- data.frame(angle = seq(0, 360, by = 1))
# 
# df$label <- sapply(df$angle, compass_labels)
# 
# 
# 
# # Generate the compass plot
# 
# ggplot(df, aes(x = 0, y = 0,  xend = cos(angle*pi/180), yend = sin(angle*pi/180))) +
#   
#   geom_segment(arrow = arrow(length = unit(0.1, "cm"))) + 
#   
#   geom_text(aes(label = label), data = subset(df, angle %% 45 == 0), 
#             
#             position = position_polar(radius = 1.1), size = 3) +
#   
#   coord_polar(theta = "x") + 
#   
#   theme_void() + 
#   
#   labs(title = "Compass with Degrees")
# 
# 

## compass plot code taken from chatGPT
compass_plot <- function(degrees) {
  # Convert degrees to radians
  radians <- degrees * (pi / 180)
  
  # Set up plot canvas
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1.3, 1.4), 
       type = "n", xlab = "", ylab = "", asp = 1,
       main = paste("Average Wind Direction"),
       yaxt="n", xaxt="n")
  symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)
  
  # Draw compass lines
  arrows(0, 0, cos(radians), sin(radians), col = "red", lwd = 2)
  text(1.3 * cos(radians), 1.3 * sin(radians), labels = paste(degrees, "°"), col = "red")
  
  # Add compass directions
  text(1, 0, "E", pos = 4)
  text(-1, 0, "W", pos = 2)
  text(0, 1, "N", pos = 3)
  text(0, -1, "S", pos = 1)
}

# Example use
compass_plot(40)  # Plot pointing to 45 degrees

