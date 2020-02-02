# Define a class named Simulator, which is the workhorse of main.R.

library(mlegp)

Simulator <- function(file_path_, setting_){
    # This is the constructor for initilize a Simulator object. It takes two parameters, file_path_, a string representing a directory, and setting__, a list representing a setting for running this simulation.
    # The data of an Simulator object is updated in simulation.
    setting <- setting_
    input_path <- file_path_
    dists <- NA 
    gene_id <- 15 # The first 14 columns in the dataset are features. Starting from the 15th, each column in the dataset represents a gene.  
    res_path <- NA
    res_name <- NA
    res_path <- NA
    data_all <- NA
    train <- NA
    train_backup <- NA
    pool <- NA
    pool_all  <- NA
    benchmark <- NA
    model <- NA
    errors <- NA
    structure(class="Simulator", environment())
  }

source("prepare_data.R")

train_simulator <- function(simulator) {
    # This is a method of the Simulator class, training a GP model using the training dataset of a Simulator object.
    train <- simulator$train[, c(1:14, simulator$gene_id)]
    print("training data gene id")
    simulator$model <- mlegp(train[, 2:ncol(train)-1], train[, ncol(train)])
  } 

source("update_train_pool.R")

screen_given_a_metric <- function(simulator, metric_fun){
   # This method of the Simulator class generated an array of indexes of datapoints selected given a sampling metric, which is denoted by the parameter, metric_fun.
   selected_index <- as.numeric()
   for (i in 1:simulator$setting[["batch_size"]]) {
        metric <- metric_fun(simulator)
        index=sort(metric, decreasing=TRUE, index.return=TRUE)$ix
   
        selected_index <- c(selected_index, index[1])
        update_train_by_prediction(simulator, index[1])
        train_simulator(simulator)
   }
   return (selected_index)
}

source("screen_return_index_helper.R")
source("max_dist.R")

screen_return_index <- function(simulator) {
    # This method screen the left datapoints in the pool in order to sample the next batch based on the sampling method defined in the setting of the Simulator object.
    method <- simulator$setting[["method"]]
    if (method != "Random" && method != "Expert") {
        metric_fun <- switch(simulator$setting[["method"]], 
                            "EN" <- EN_metric, 
                            "MI" <- MI_metric, 
                            "COV"= COV_metric, 
                            "DIV"=diversity_metric, 
                            "Tan"=tanimoto_metric)
  
        if (simulator$setting[["adaptive"]] == 1) {
           return(screen_given_a_metric(simulator, metric_fun))
        } else {
           metric <- metric_fun(simulator)
           index=sort(metric, decreasing <- TRUE, index.return=TRUE)$ix
           return(index[1:simulator$setting[["batch_size"]]])
        }
    } else {
        index <- seq(1, nrow(simulator$pool), 1)
        return(index[1:simulator$setting[["batch_size"]]])
    }
}

simulate <- function(simulator){
    # This method is entry of starting a simulation called in the main.R. The simulation keeps running until the number of iterations defined is the setting is reache.
    iter_num <- simulator$setting[["iter_num"]]
    simulator$errors <- as.numeric()
    prepare_data(simulator)
    for (i in 1:iter_num) {
        # when exploration == 1, no exploration; otherwise the frequency of using OED is 1/simulator$setting["exploration"]
        if ((i%%simulator$setting[["exploration"]] != 0 | i == iter_num) & simulator$setting[["exploration"]] != 1) {
            cat("random sampling-----------------------")
            common_index <- 1
        } else {
           cat("OED---------------------")    
           indexes <- as.numeric()
           set.seed(i + simulator$setting[["random_seed"]])
           gene_ids <- sample(15:1137, 800, replace=FALSE)
           for (gene_id in gene_ids) {
               simulator$gene_id <- gene_id
               try(train_simulator(simulator))
               selected_index <- try(screen_return_index(simulator))
               if (isTRUE(class(selected_index) == "try-error")) next
               indexes <- c(indexes, selected_index)
               # all the genes are the same for the "Tani" metric
               if (simulator$setting[["method"]] == "Tan" | simulator$setting[["method"]] == "Random") {
                 cat("only one gene needed-----------------")
                 break
           }
        }
       common_index <- names(sort(table(indexes), decreasing=TRUE)[1])
       common_index <- as.numeric(common_index)
       }
       cat("common index: ", common_index, "\n") 
       update_train_pool(simulator, common_index)
       print("------------dimension of the training set")
       print(dim(simulator$train))
   } 
}
