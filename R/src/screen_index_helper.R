#' All the functions in this script are related to the implementation of a 
#' sampling metric used OPEX. Five sampling metrics are defined in this modules.
#' They are EN_metric, MI_metric, COV_metric, diversity_metric, and 
#' tanimoto_metric. To understand the math behind the implementation, see the 
#' method section in the Supplemental materials.


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
  nugget <- simulator$model[["nugget"]]
  
  newcov <- compute_covariance_h(simulator, 
                                 set1 = simulator$train,
                                 set2 = testset)
  
  diag(newcov) <- diag(newcov) #+nugget
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
  res <-  predict_testset(simulator, simulator$pool[, 2:ncol(simulator$train)-1],
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
                                   simulator$pool[, 2:ncol(simulator$train)-1],
                                   simulator$pool[, 2:ncol(simulator$train)-1])
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
                          simulator$pool[, 2:ncol(simulator$train)-1],
                          unfold = TRUE) 
  var <- res$var
  divisor <- compute_MI_denominator(simulator) 
  metric <- var/divisor
  return(metric)
}


#' This function implements the covariance metric, calcualting the covariance
#' of each datapoint in the pool of a simulator object.

COV_metric <- function(simulator){
  n_pool <- nrow(simulator$pool[, 2:ncol(simulator$train)-1])
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
  div2 <- calculate_diversity_(culture_condition[, c(12:ncol(simulator$train)-1)])
  return(div1+div2)
}

# GP bagging committee metric
resample <- function(data, idx_com){
  selected = 1:15
  left_rows = 16:nrow(data)
  num_left = length(left_rows)
  coms = combinations(num_left, as.integer(0.8*num_left), left_rows, repeats=FALSE)
  selected = c(selected, coms[idx_com, ])
  return(selected)
}

GP_committee <- function(simulator){
  size_committee = 3
  means_pred = matrix( , nrow=nrow(simulator$pool), size_committee)
  for (i in 1:size_committee) {
    one_simulator = Simulator(file_path,setting_)
    prepare_data(one_simulator)
    selected = resample(simulator$train, i)
    one_simulator$train = simulator$train[selected, ]
    train_simulator(one_simulator)
    means_pred[, i] = predict_testset(one_simulator, simulator$pool[, 1:14], unfold=TRUE)$mean
  }
  return(rowVars(means_pred))
  cat("calculating variance is done-------------------")
}


# Diverse committee
# create a formula for training a linear regression or artifical neural network model
create_formula <- function(data){
  f_s = paste(c(colnames(data)[15], " ~ ."), sep="", collapse = "")
  f <- as.formula(f_s)
  return(f)
}


linear_regression <- function(data, pool){
  f = create_formula(data)
  model = lm(f, data=as.data.frame(data))
  prediction = predict(model, as.data.frame(pool))
  return(prediction)
}

feedforward_nn <- function(data, pool){
  f = create_formula(data)
  model = neuralnet(f, data, hidden=c(10), linear.output=T)
  prediction = compute(model, pool)$net.result
  return(prediction)
}

random_forest <- function(data, pool){
  f = create_formula(data)
  model = random_forest(f, data)
  prediction = predict(model, pool)
  return(prediction)
}

gaussian_process <- function(data, pool){
  model = mlegp(data[, 1:14], data[,15])
  model = mlegp(data[, 1:14], data[,15])
  prediction = predict(model, pool[, 1:14])
  return(prediction)
}

svr_model <- function(data, pool){
  f = create_formula(data)
  model = svm(f, data)
  prediction = predict(model, pool)
  return(prediction)
}

# This function materialilze the function used for training a model from the above options.
materialize_fun <- function(model_type){
  framework <- switch(model_type,
                      "LR"=linear_regression,
                      "FNN"=feedforward_nn,
                      "RF"=random_forest,
                      "GP"=gaussian_process,
                      "SVR"=svr_model)
  return(framework)
}

# Train four types of models and calculate the variance in the predictions by the four models
diverse_committee <- function(simulator){
  model_types = c("SVR","LR", "GP","FNN")
  size_committee = length(model_types)
  means_pred = matrix( , nrow=nrow(simulator$pool), size_committee)
  for (i in 1:size_committee) {
    framework <- materialize_fun(model_types[i])
    prediction <- framework(simulator$train[, c(1:14, simulator$gene_id)], simulator$pool[, 1:14])
    means_pred[, i] = as.numeric(prediction)
  }
  return(rowVars(means_pred))
  
}

# d_optimal criterion
calculate_det_df <- function(df){
  x = t(as.matrix(df))
  AA = x %*% t(x)
  return(det(AA))
}

d_optimal <- function(siumulator){
  train = simulator$train[-11, 1:14]
  pool = simulator$pool[, 1:14]
  ds = as.numeric()
  for (i in 1:nrow(pool)){
    train_one_candiate = rbind(train, pool[i, , drop=FALSE])
    d = calculate_det_df(train_one_candiate)
    ds = c(ds, d)
  }
  return(ds)
}


#' A function calculating the diversity of the training set after one of the 
#' datapoint in the benchmark set is added to the training set.

diversity_metric <- function(simulator) {
  condition_train <- simulator$train[, c(2:ncol(simulator$train)-1)]
  condition_benchmark <- simulator$benchmark[, c(2:ncol(simulator$train)-1)]
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
  condition_train <- simulator$train[, 2:ncol(simulator$train)-1]
  names <- colnames(condition_train)
  indexes <- apply(condition_train[1:10, ] == 1, 1, which)
  biocides_tr <- names[indexes[1, ]]
  antibiotics_tr <- names[indexes[2, ]]
  
  # Extract the biocides and antibiotics in the left data.
  condition_test <- simulator$benchmark[, 2:ncol(simulator$train)-1, drop = FALSE]
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
