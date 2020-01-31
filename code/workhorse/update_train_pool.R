source("add_noise.R")

update_train_pool <- function (simulator,select_index) {
 
  add = simulator$setting[["add"]]
  noise_level = simulator$setting[["noise"]]
  
  adddata = simulator$pool[select_index,,drop=FALSE]
  # select the top add rows and add them to train. (caveat: when indexing one row,keep the dimension)
  if (simulator$setting[["noise"]] > 0 ) {
     adddata = add_noise(noise_level,adddata)
  }
  simulator$train_backup = t(data.frame(t(simulator$train_backup),t(adddata)))
  simulator$pool = simulator$pool[-select_index,, drop=FALSE]
  simulator$train = simulator$train_backup #the train is updated by prediction
  
  # update the benchmark
  simulator$benchmark = simulator$pool
  #save the train file
  train_file = paste(simulator$res_name,"_train.csv",sep="")
  write.csv(simulator$train,file.path(simulator$res_path,train_file))
  cat(simulator$res_path)  
}


update_train_by_prediction <- function(simulator,select_index) {
   # newly add in v3
   # select_index is a number
   adddata = simulator$pool[select_index,,drop=FALSE]
   #replace the truth with prediction
   res = predict_testset(simulator,adddata, unfold = TRUE)
   adddata[length(adddata)] = res$mean
   simulator$train=t(data.frame(t(simulator$train),t(adddata)))
}


