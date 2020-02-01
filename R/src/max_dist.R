#' This function takes a simulator object and an index for the next datapoint to select, calculate the distance between each datapoint left in the sampling pool and the next datapoint,
#' and finally return a array of indexes of all the datapoints selected. How many datapoints are selected are determined by the simulator$setting[['add']].
#' This function is related to the constraint sampling section of the Supplementary material.

max_dist<- function (simulator,index) {

  add = simulator$setting[["add"]]
  pool = simulator$pool
  select = as.numeric()
  select[1] = index[1]
  removed = as.numeric()
  left = setdiff(index,select)
  
  while(length(select)<add){
    for (l in left){
      d = 10 # a random large number
      # compute the distance of each left datapoint to all the select 
        # datapoints and store the minimum
      for (s in select) {
      d = min(d,sqrt(sum((pool[l,1:ncol(pool)]- pool[s,1:ncol(pool)])**2)))
      }
      
      left = setdiff(left,l) # no matter selected or not no need to consider it in the 
                             # next iteration of while loop
      if (d > 0.35) {
        select = c(select,l)
        break
      }
    }
  }
  return(select)
}
