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
