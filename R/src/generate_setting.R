#' Generate a set of combinations of the nine variables and save it in a file
#' named by the 1st argument passed from the command line.
#' Example run: "Rscript generate_setting.R expert_sampling"

#' The generated file is a tabular file of nine columns, each of which 
#' represents a variable. This file is used by the main.R.

#' The mean of each variables are explained as follows:
#`   random_seed: The seed used when ranomly select the first ten culture
#'   exploration: The frequency of switching to random sampling. 
#'                conditions. e.g. exploration=k means in every k+1 iterations, 
#'                random sampling is used once.
#'   start_size: The number of culture conditions in the training set at the 
#'               begining of the simulation.
#'   batch_size: The number of samples added in each iteration.
#'   dataset_id: The id for the dataset used. Because we tested the method on 
#'               seven synthetic dataset and one RNA-seq dataset.
#'               dataset 1-7 are synthetic and dataset 8 is the RNA-seq.
#'   noise: The level of white noised added to the synthetic dataset. 
#'          The numbers represent the percentage of variance of the white noise
#'          compared to the target value.
#'   iter_num: The total number of iterations in one simulation.
#'   method: The name of the sampling approach used in each iteration. 
#'           "Tan", a short for tanimoto, represents expert sampling. 
#'           "Random", means random sampling. 
#'           "MI", denotes mutual information.
#'           "EN", denotes entropy.  

args <- commandArgs(trailingOnly = TRUE)
setting_file <- args[1]
setting <- list(random_seed = c(1:50),
                exploration = c(1, 2, 3),
                adaptive = c(0),
                start_size = c(15),
                batch_size = c(1, 3),
                dataset_id = 8,
                noise = 0,
                iter_num = 30,
                method = ("Tan",  "Random", "MI", "EN"),
                num_of_features = 14
               )
setting <- expand.grid(setting)
write.csv(setting, paste("../../out_data/", setting_file, sep = ""), 
          row.names = FALSE)
print(nrow(setting))
