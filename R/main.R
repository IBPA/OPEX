#' Required script arguments: 
#     1) A string representing the name of a setting file 
#'    2) An integer denoting which setting in the setting file is used.

#' In this script, a Simulator object is created given the id of a setting 
#' defined in the setting file. Then a method of the object named simulate 
#' is called and the simulation is run.
#' For the implemenation of the method of the Simulator class, see Simulator.R 

library(mlegp)

set.seed(2018)
main <- function(){
   data_path = "../data"
   output_path = "../output"
   # Change the working directory to src, so the source code is visible.
   setwd("./src")
   source("Simulator.R")
   
   # Parse the argument passed from the command line.
   args = commandArgs(TRUE)
   setting_file = args[1]
   setting_id = as.integer(args[2])
   
   # Create a directory for storing the results for running the settings 
   # defined the setting_file.
   output_path = paste(output_path, sub("\\..*$", '', setting_file), sep = "/")
   dir.create(output_path, showWarnings = FALSE)
   
   # Read in the configuration file and slice one setting.
   settings = read.csv(paste("../../out_data/", 
                             setting_file, sep = ""), 
                             stringsAsFactors = FALSE)
   s = settings[setting_id, , drop = TRUE]
   
   # Parse the setting into a list.
   setting_ = list("random_seed" = s[["random_seed"]],
                    "exploration" = s[["exploration"]],      
                    "anti_batch" = s[["anti_batch"]],
                    "start_size" = s[["start_size"]],
                    "add" = s[["add"]],
                    "data" = s[["data"]],
                    "noise" = s[["noise"]],
                    "iter_num" = s[["iter_num"]],
                    "method" = s[["method"]])
   
   # Create the path for the input data.
   file_path =  file.path(data_path, paste(setting_[["data"]], ".csv",
                                           sep = ""))
   
   # Create an object of the Simulator class.
   simulator = Simulator(file_path,setting_)
   
   # Load the support data for expert sampling.
   dists = read.csv(file.path(data_path, "pairwise_similarity_bio.csv"), 
                    row.names=1)
   simulator$dists = dists
   
   # Run the simulator.
   simulate(simulator)
} 

main()
