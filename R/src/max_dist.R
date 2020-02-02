#' This function takes a simulator object and an index for the next datapoint 
#  to select. It calculates the distance between each datapoint left in the
#' sampling pool and the next datapoint.
#' It return a array of indexes of all the datapoints selected. 
#' The number of datapoints selected is determined by 
#' the simulator$setting[['batch_size']].
#' This function is related to the constraint sampling section of the
#' supplementary material.

max_dist<- function (simulator, index) {
  batch_size <- simulator$setting[["batch_size"]]
  pool <- simulator$pool
  select <- as.numeric()
  select[1] <- index[1]
  removed <- as.numeric()
  left <- setdiff(index, select)
  
  while(length(select) < batch_size){
    for (l in left){
      d <- 10 # A random large number
      # Compute the distance of each left datapoint to all the selected
      # datapoints and store the minimum.
      for (s in select) {
        d <- min(d, 
                 sqrt(sum((pool[l, 1:ncol(pool)] - pool[s, 1:ncol(pool)]) ** 2)))
      }
      
      left <- setdiff(left, l) # No matter selected or not, no need to consider 
                               # it in the next iteration of while loop.
      if (d > 0.35) {
        select <- c(select, l)
        break
      }
    }
  }
  return(select)
}
