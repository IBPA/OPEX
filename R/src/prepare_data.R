#' This module includes one wrapper function and four functions called by the wrapper. The input of the wrapper function is a Simulator object. The four functions in the wrapper, set up the path for storing the results after running a simulation.

set_res_path <- function(simulator) {
  #' create the directory for storing the results of a simulation based on the setting of the Simulator object.
  cols <- names(simulator$setting)[c(1, 2)]
  values <- unlist(simulator$setting)[c(1, 2)]
  folder <- paste0(cols, values, sep="", collapse <- "-")
  dir.create(file.path(out_data_path, folder), showWarnings <- FALSE)
  simulator$res_path <- file.path(out_data_path, folder, simulator$setting["method"])
  cat("create folder")
  cat(simulator$res_path)
  dir.create(simulator$res_path, showWarnings <- FALSE)
}

set_res_name <- function(simulator){
  # create the name of the folder for storing the results based on the setting of the simulator object.
  simulator$res_name <- paste("every", simulator$setting["batch_size"], "iter_num", simulator$setting["iter_num"] , sep <- "_")
}

load_data <- function(simulator) {
  # load and sort the data, the first 14 rows covers all the antispetics and biocides.
  gene_id <- simulator$setting[["gene_id"]]
  data <- read.csv(simulator$input_path, stringsAsFactors=FALSE)
  con_names <- data[, 1]
  data <- data[, -1]
  once_rows <- which(apply(data[, 1:14], 1, sum) == 1)
  
  #you can use a 2-d table to image this
  set.seed(simulator$setting[["random_seed"]]) # shuffle before splitting
  antiseptics <- sample(colnames(data)[1:10], 10, replace <- FALSE)
  antibiotics <- sample(colnames(data)[11:14], 4, replace <- FALSE)

  row_index <- sample(1:10, 10, replace <- FALSE)
  col_index <- c(1, 2, 3, 4, 3, 2, 1, 2, 4, 1)
  start_condition <- c(paste(antiseptics[row_index], antibiotics[col_index], sep="_"), "Control")
  selected <- match(start_condition, con_names)
  selected <- c(selected, once_rows)  
  data_all <- as.matrix(data)

  print(apply(data_all[selected, 1:14], 2, sum))
  cat("number of selected points", length(selected))
   
  left <- setdiff(sample(1:nrow(data_all)), selected)
  simulator$data_all <- data_all[c(selected, left), ]
}

split_data <- function(simulator){
  # split the 45 culture conditions into two parts, the starting training set and a pool from which the next dataset is sampled. The pool is also used for evaluating the prediction performance of the GP models.
  start_size <- simulator$setting[["start_size"]]
  simulator$train_backup <- simulator$data_all[1:start_size, ]
  print("number of columns")
  print(ncol(simulator$train_backup))
  print("starting data for training")
  print(simulator$train_backup[, 1:15])
  simulator$train <- simulator$train_backup
  print("data for pool")
  simulator$pool  <- simulator$data_all[(start_size+1):nrow(simulator$data_all), ]
  
  if (simulator$setting[["method"]] == "Expert") {
     expert_path <- file.path(dirname(simulator$input_path), paste(simulator$setting[["random_seed"]], "_expert.csv", sep=""))
     expert_file <- read.csv(expert_path)
     expert_order <- expert_file[ (simulator$setting[["start_size"]]+1):nrow(expert_file), 2]
     print(simulator$pool[, 1:15])
     simulator$pool <- simulator$pool[expert_order, ]
  }
  print(simulator$pool[, 1:15])
  simulator$benchmark  <- simulator$pool
  
}

prepare_data <- function(simulator){
  set_res_name(simulator)
  set_res_path(simulator) 
  load_data(simulator)
  split_data(simulator)
}

