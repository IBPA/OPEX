# This module has two functions. The update_train_pool function is to update the training set and pool set of a Simulator object after each round of sampling. The update_train_by_prediction is used in adaptive sampling where the truth of the selected datapoints in a batch was replaced by a prediction when selecting the next datapoint in the batch. Note that the prediction will be replaced before sampling datapoints in the next iteration.

source("add_noise.R")

update_train_pool <- function (simulator,selected_index) {
  # This function removes the selected datpoints from simulator$pool and adds them to simulator$train.
  add = simulator$setting[["add"]]
  noise_level = simulator$setting[["noise"]]
  
  adddata = simulator$pool[selected_index,,drop=FALSE]
  # select the top add rows and add them to train. (caveat: when indexing one row,keep the dimension)
  if (simulator$setting[["noise"]] > 0 ) {
     adddata = add_noise(noise_level,adddata)
  }
  simulator$train_backup = t(data.frame(t(simulator$train_backup),t(adddata)))
  simulator$pool = simulator$pool[-selected_index,, drop=FALSE]
  simulator$train = simulator$train_backup #the train is updated by prediction
  
  # update the benchmark
  simulator$benchmark = simulator$pool
  #save the train file
  train_file = paste(simulator$res_name,"_train.csv",sep="")
  write.csv(simulator$train,file.path(simulator$res_path,train_file))
  cat(simulator$res_path)  
}


update_train_by_prediction <- function(simulator,selected_index) {
   # Select the datapoints indexed by selected_index, make a prediction on this selected datapoint, finally add this datapoint to the training set.
   adddata = simulator$pool[selected_index,,drop=FALSE]
   #replace the truth with prediction
   res = predict_testset(simulator,adddata, unfold = TRUE)
   adddata[length(adddata)] = res$mean
   simulator$train=t(data.frame(t(simulator$train),t(adddata)))
}


