#' This module has two functions:
#'   1) update_train_pool function is to update the training set and pool set
#       of a Simulator object after each round of sampling.
#'   2) The update_train_by_prediction is used in adaptive sampling where the 
#'      truth of the selected datapoints in a batch was replaced by a prediction 
#'      when selecting the next datapoint in the batch. Note that the prediction
#'      will be replaced before sampling datapoints in the next iteration.

source("add_noise.R")

#' This function removes the selected datpoints from simulator$pool and adds 
#' them to simulator$train.

update_train_pool <- function (simulator, selected_index) {
  add <- simulator$setting[["batch_size"]]
  noise_level <- simulator$setting[["noise"]]
  
  additional_data <- simulator$pool[selected_index, , drop = FALSE]

  # Select the top add rows and add them to train (caveat: when indexing one 
  # row, keep the dimension).
  if (simulator$setting[["noise"]] > 0 ) {
     additional_data <- add_noise(noise_level, additional_data)
  }
  simulator$train_backup <- t(data.frame(t(simulator$train_backup), 
                                         t(additional_data)))
  simulator$pool <- simulator$pool[-selected_index, , drop = FALSE]
  simulator$train <- simulator$train_backup # The train is updated by prediction
  
  # Update the benchmark
  simulator$benchmark <- simulator$pool
  # Save the train file
  train_file <- paste(simulator$res_name, "_train.csv", sep = "")
  write.csv(simulator$train, file.path(simulator$res_path, train_file))
  cat(simulator$res_path)  
}


#' Select the datapoints indexed by selected_index, make a prediction on this 
#' selected datapoint, finally add this datapoint to the training set.

update_train_by_prediction <- function(simulator, selected_index) {
   additional_data <- simulator$pool[selected_index, , drop = FALSE]
   # Replace the truth with prediction
   res <- predict_testset(simulator, additional_data, unfold = TRUE)
   additional_data[length(additional_data)] <- res$mean
   simulator$train <- t(data.frame(t(simulator$train), t(additional_data)))
}
