# Define a class named Simulator.
# The main method used in main.R is the simulate method. Other methods of this class and modules in other files are helper functions 
# for the simulate method.


library(mlegp)
Simulator <- function(file_path_,setting_){
  setting = setting_
  input_path = file_path_
  dists = NA 
  gene_id = 15 
  res_path = NA
  res_name = NA
  res_path = NA
  data_all = NA
  train = NA
  train_backup = NA
  pool = NA
  pool_all  = NA
  benchmark = NA
  model = NA
  errors = NA
  structure(class="Simulator", environment())
}


source("prepare_data.R")

train_simulator <- function(simulator) {
    train = simulator$train[, c(1:14, simulator$gene_id)]
    print("training data gene id")
    print(dim(train))
    num = ncol(train)
    simulator$model = mlegp(train[,2:num-1], train[,num])
    
  }
  

source("update_train_pool.R")

simulate_index <- function(simulator, metric_fun){
   select_index = as.numeric()
   for (i in 1:simulator$setting[["add"]]) {
        metric = metric_fun(simulator)
        index=sort(metric, decreasing = TRUE,index.return=TRUE)$ix
   
        select_index = c(select_index,index[1])
        update_train_by_prediction(simulator, index[1])
        train_simulator(simulator)
   }
   return (select_index)
}


source("screen_index_helper.R")
source("max_dist.R")

screen_index <- function(simulator) {
  method = simulator$setting[["method"]]
  if (method != "Random" && method != "Expert") {
      metric_fun = switch(simulator$setting[["method"]],
                          "EN" = EN_metric,
                          "MI" = MI_metric,
                          "COV"= COV_metric,
                          "DIV"=diversity_metric,
                          "Tan"=tanimoto_metric)

      if (simulator$setting[["anti_batch"]]==1) {
         return(simulate_index(simulator,metric_fun))
      } else {
         metric = metric_fun(simulator)
         index=sort(metric, decreasing = TRUE,index.return=TRUE)$ix
         return(index[1:simulator$setting[["add"]]])
      }
  } else {
      index = seq(1,nrow(simulator$pool),1)
      return(index[1:simulator$setting[["add"]]])
  }
}


simulate <- function(simulator){
  iter_num = simulator$setting[["iter_num"]]
  simulator$errors = as.numeric()
  prepare_data(simulator)
  for (i in 1:iter_num) {
    # when exploration==1, no exploration; otherwise the frequency of using OED is 1/["exploration"]
    if ((i%%simulator$setting[["exploration"]]!=0 | i==iter_num) & simulator$setting[["exploration"]] != 1) {
      cat("random sampling-----------------------")
      common_index = 1
    } else {
     cat("OED---------------------")    
     indexes = as.numeric()
     set.seed(i + simulator$setting[["random_seed"]])
     gene_ids = sample(15:1137, 800, replace=FALSE)
     for (gene_id in gene_ids) {
        simulator$gene_id = gene_id
        try(train_simulator(simulator))
        select_index = try(screen_index(simulator))
        if (isTRUE(class(select_index)=="try-error")) next
        indexes = c(indexes, select_index)
        # all the genes are the same for the "Tani" metric
        if (simulator$setting[["method"]] == "Tan" | simulator$setting[["method"]] == "Random") {
          cat("only one gene needed-----------------")
          break
        }
     }
     common_index = names(sort(table(indexes), decreasing=TRUE)[1])
     common_index = as.numeric(common_index)
     }
     cat("common index: ", common_index,"\n") 
     update_train_pool(simulator,common_index)
     print("------------dimension of the training set")
     print(dim(simulator$train))
  } 
}


