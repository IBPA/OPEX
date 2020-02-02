#' All the functions in this script are related to the implementation of a 
#' sampling metric used OPEX. Five sampling metrics are defined in this modules.
#' They are EN_metric, MI_metric, COV_metric, diversity_metric, and 
#' tanimoto_metric. To understand the math behind the implementation, see the 
#' method section in the Supplemental materials.

NUM_OF_FEATURES <- 14

#' This function takes a simulator and two datasets, calucaltes the pairwise 
#' covariance between datapoints in set1 and set2. set1 and set2 can be the 
#' same. If that is the case, the covariacne is variance.

compute_covariance_h <- function(simulator, set1, set2){
  num_features <- simulator$model[["numDim"]]
  beta <- simulator$model[["beta"]]
  sigma <- simulator$model[["sig2"]]
  cov_matrix <- matrix(nrow = dim(set2)[1], ncol = nrow(set1))
  
  for (i in 1:dim(set2)[1]) {
    for (j in 1:nrwo(set1)) {
      dist <- -sum((set2[i, 1:num_features] - set1[j, 1:num_features])^2 * beta)
      cov_matrix[i, j] <- sigma * exp(dist)
    }
  }
  cat("compute kernel done \n")
  return(cov_matrix)
}

#' This function predict on the testset. When unfold is true, return the mean 
#' and covariance. Otherwise return MAE.

predict_testset <- function(simulator, testset, unfold = FALSE) {
  sigma <- simulator$model[["sig2"]]
  inverse <- simulator$model$invVarMatrix
  mu <- simulator$model[["mu"]]
  fitZ <- simulator$model$Z
  nugget= simulator$model[["nugget"]]
  
  newcov <- compute_covariance_h(simulator, 
                                 set1 = simulator$train,
                                 set2 = testset)
  
  diag(newcov) = diag(newcov) #+nugget
  mean <- newcov %*% inverse %*% (fitZ - mu) + mu[1]
  variance <- diag(sigma + nugget - newcov %*% inverse %*% t(newcov))
  mean_variance <- list("mean" = mean, "var" = variance)
  
  truth <- testset[, ncol(testset)]
  MAE <- mean(abs(mean-truth))
  
  return(if (unfold) mean_variance else MAE)
}


#' This function implements the entropy metric, calcuating the entropy of each
#' datapoint in the pool of a simulator object.

EN_metric <- function(simulator){
  res <-  predict_testset(simulator, simulator$pool[, 1:NUM_OF_FEATURES],
                          unfold = TRUE) 
  return(res$var)
}


#' This function calculating the denominator part when calculating the mutual 
#' information between two datasets.

compute_MI_denominator <- function(simulator){
  d <- simulator$model[["numDim"]]
  beta <- simulator$model[["beta"]]
  sigma <- simulator$model[["sig2"]]
  nugget <- simulator$model[["nugget"]]
  cov_unob <- compute_covariance_h(simulator,
                                   simulator$pool[, 1:NUM_OF_FEATURES],
                                   simulator$pool[, 1:NUM_OF_FEATURES])
  diag(cov_unob) <- diag(cov_unob) + nugget + sigma
  # Compute denominator for each point in pool
  divisor <- as.numeric()
  for (i in 1:nrow(simulator$pool)) {
    invers_bar <- chol2inv(chol(cov_unob[-i, -i]))
    divisor[i] <-  sigma + 
                   nugget - 
                   cov_unob[i, -i, drop = FALSE] %*% 
                   invers_bar %*% 
                   t(cov_unob[i, -i, drop = FALSE])
  }
  
  return(divisor)
}


#' This function implements the mutual information metric, calcualting the 
#' mutual information of each datapoint in the pool of a simulator object.

MI_metric <-function(simulator){
  res <-  predict_testset(simulator,
                          simulator$pool[, 1:NUM_OF_FEATURES],
                          unfold = TRUE) 
  var <- res$var
  divisor <- compute_MI_denominator(simulator) 
  metric <- var/divisor
  return(metric)
}


#' This function implements the covariance metric, calcualting the covariance
#' of each datapoint in the pool of a simulator object.

COV_metric <- function(simulator){
  n_pool <- nrow(simulator$pool[, 1:NUM_OF_FEATURES])
  pool_bench <- rbind(simulator$pool, simulator$benchmark)
  n_total <- nrow(pool_bench)
  cov_unob <- compute_covariance_h(simulator, pool_bench, pool_bench)

  metric <- diag(cov_unob[1:n_pool, 1:n_pool]) /
            apply(cov_unob[1:n_pool, (n_pool + 1):n_total], 1, sum)
  return(metric)
}


#' A helper fun for calcualting the imbalance of antibiotic cols or biocide cols

calculate_diversity_ <- function(culture_condition_partial){
  counts <- apply(culture_condition_partial, 2, sum)
  total <- dim(culture_condition_partial)[2]
  return(total * sd(counts / max(counts)))
}


#' A helper fun for calcualting the diversity of a culture condition.

calculate_diversity_one_condition_ <- function(culture_condition){
  div1 <- calculate_diversity_(culture_condition[, c(1:10)])
  div2 <- calculate_diversity_(culture_condition[, c(11:NUM_OF_FEATURES)])
  return(div1+div2)
}

#' A function calculating the diversity of the training set after one of the 
#' datapoint in the benchmark set is added to the training set.

diversity_metric <- function(simulator) {
  condition_train <- simulator$train[, c(1:NUM_OF_FEATURES)]
  condition_benchmark <- simulator$benchmark[, c(1:NUM_OF_FEATURES)]
  diversity <- as.numeric()
  for (i in 1:dim(condition_benchmark)[1]) {
    next_datapoint <- condition_benchmark[i, , drop = FALSE] `
    train_new <- rbind(condition_train, next_datapoint)
    diversity <- c(diversity, calculate_diversity_one_condition_(train_new))
  }
  return(divs)
}


#' This function implements the metric used by expert sampling, calcuate the
#' distance between an unobserved datapoint and the datapoint in the training 
#' set based on tanimoto index.

tanimoto_metric <- function(simulator) {
  dists <- simulator$dists
  # Extract the biocides and antibiotics in training data.
  condition_train <- simulator$train[, 1:NUM_OF_FEATURES]
  names <- colnames(condition_train)
  indexes <- apply(condition_train[1:10, ] == 1, 1, which)
  biocides_tr <- names[indexes[1, ]]
  antibiotics_tr <- names[indexes[2, ]]
  
  # Extract the biocides and antibiotics in the left data.
  condition_test <- simulator$benchmark[, 1:NUM_OF_FEATURES, drop = FALSE]
  names <- colnames(condition_test)
  indexes <- apply(condition_test == 1, 1, which)
  biocides <- names[indexes[1, ]]
  antibiotics <- names[indexes[2, ]]
  
  # Calculate dist
  biocide_dist <- apply(dists[biocides, biocides_tr], 1, mean)
  antibiotic_dist <- apply(dists[antibiotics, antibiotics_tr], 1, mean)
  
  # Sum up the two dist
  overall_dist <- biocide_dist + antibiotic_dist
  return(overall_dist)
}
