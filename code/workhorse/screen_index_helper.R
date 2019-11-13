compute_covariance_h <- function(simulator,set1,set2){
  #set2 can be pool or benchmark or the merge of these two
  # set1= as.matrix(set1)
  # set2 = as.matrix(set2)
  
  d = simulator$model[["numDim"]]
  beta = simulator$model[["beta"]]
  sigma = simulator$model[["sig2"]]
  number = nrow(set1)
  newcov=matrix(nrow=dim(set2)[1],ncol=number)
  
  for ( i in 1:dim(set2)[1]) {
    for (j in 1:number) {
      dist= -sum((set2[i,1:d]-set1[j,1:d])^2*beta)
      newcov[i,j]= sigma*exp(dist)
    }
  }
  cat("compute kernel done \n")
  return(newcov)
}

# you have three metrics, so you should have three functions to compute the metrics.
# This is the outline
predict_testset <- function(simulator,testset, unfold=FALSE) {
  #unfold is true, return the mean and covariance.
  sigma = simulator$model[["sig2"]]
  inverse=simulator$model$invVarMatrix
  mu=simulator$model[["mu"]]
  fitZ = simulator$model$Z
  nugget= simulator$model[["nugget"]]
  
  newcov = compute_covariance_h(simulator,set1=simulator$train,set2 = testset)
  
  diag(newcov)=diag(newcov) #+nugget
  mean=newcov%*%inverse%*%(fitZ - mu)+mu[1]
  var=diag(sigma+nugget-newcov%*%inverse%*%t(newcov))
  mean_var = list("mean"=mean,"var"=var)
  
  real = testset[,ncol(testset)]
  MAE = mean(abs(mean-real))
  
  return(if (unfold) mean_var else MAE)
}

EN_metric <- function(simulator){
  res =  predict_testset(simulator,simulator$pool[, 1:14], unfold=TRUE) 
  var = res$var
  return(var)
}

compute_MI_denominator <- function(simulator){
  
  d = simulator$model[["numDim"]]
  beta = simulator$model[["beta"]]
  sigma = simulator$model[["sig2"]]
  nugget=simulator$model[["nugget"]]
  
  ## cov(unob, unob)
  cov_unob = compute_covariance_h(simulator,simulator$pool[, 1:14], simulator$pool[, 1:14])
  diag(cov_unob)=diag(cov_unob)+nugget+sigma
  # compute denominator for each point in pool
  divisor= as.numeric()
  for (i in 1:nrow(simulator$pool)) {
    invers_bar = chol2inv(chol(cov_unob[-i,-i]))
    divisor[i] =  sigma+nugget - cov_unob[i,-i,drop=FALSE] %*% invers_bar %*% t(cov_unob[i,-i,drop=FALSE])
  }
  
  return(divisor)
}

MI_metric <-function(simulator){
  res =  predict_testset(simulator,simulator$pool[, 1:14], unfold=TRUE) 
  var = res$var
  divisor = compute_MI_denominator(simulator) 
  metric = var/divisor
  return(metric)
}

COV_metric <- function(simulator){
  n_pool = nrow(simulator$pool[, 1:14])
  pool_bench = rbind(simulator$pool, simulator$benchmark)
  n_total = nrow(pool_bench)
  cov_unob = compute_covariance_h(simulator,pool_bench,pool_bench)

  metric = diag(cov_unob[1:n_pool,1:n_pool])/apply(cov_unob[1:n_pool,(n_pool+1):n_total],1,sum)
  return(metric)
  
}


calculate_div_ <- function(m){
  # a helper fun for calcualting the imbalance of antibiotic cols or biocide cols
  counts = apply(m, 2, sum)
  total = dim(m)[2]
  return(total*sd(counts/max(counts)))
}

calculate_div_one_condition_ <- function(m){
  # a helper fun for calcualting the total imbalance of a group of selected culture conditions
  div1 = calculate_div_(m[, c(1:10)])
  div2 = calculate_div_(m[, c(11:14)])
  return(div1+div2)
}

diversity_metric <- function(simulator) {
  con_tr = simulator$train[, c(1:14)]
  con_benchmark = simulator$benchmark[, c(1:14)]
  divs = as.numeric()
  for (i in 1:dim(con_benchmark)[1]) {
    new = con_benchmark[i,,drop=FALSE] 
    tr_new = rbind(con_tr, new)
    divs = c(divs, calculate_div_one_condition_(tr_new))
  }
  return(divs)
}


tanimoto_metric <- function(simulator) {
  dists = simulator$dists
  # extract the biocides and antibiotics in training data
  con_tr = simulator$train[, 1:14]
  names = colnames(con_tr)
  indexes = apply(con_tr[1:10,]==1, 1, which)
  biocides_tr = names[indexes[1, ]]
  antibiotics_tr = names[indexes[2, ]]
  
  # extract the biocides and antibiotics in the left data
  con_test = simulator$benchmark[, 1:14, drop=FALSE]
  names = colnames(con_test)
  indexes = apply(con_test==1, 1, which)
  biocides = names[indexes[1, ]]
  antibiotics = names[indexes[2, ]]
  
  # calculate dist
  biocide_dist = apply(dists[biocides, biocides_tr], 1, mean)
  antibiotic_dist = apply(dists[antibiotics, antibiotics_tr], 1, mean)
  
  # sum up the two dist
  overall_dist = biocide_dist + antibiotic_dist
  return(overall_dist)
}

