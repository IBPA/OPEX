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