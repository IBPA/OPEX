
library(mlegp)

set.seed(2018)

main <- function(){
   source("Simulator.R")
   args = commandArgs(TRUE)
   training_path = args[1]
   pool_path = args[2] 
   batch_size = as.integer(args[3])
   simulator = Simulator()
   load_data(simulator, training_path, pool_path)
   select_next_batch(simulator, batch_size)
   
} 

main()
