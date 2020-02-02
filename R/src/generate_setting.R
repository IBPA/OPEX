#' This script generates a set of combinations of the nine variables and save it in a file named by the 1st argument passed from the command line. e.g. to run it, Rscript generate_setting.R expert_sampling

#' The generated file is a tabular file of nine columns, each of which represents a variable. This file is used by the main.R.

#' The mean of each variables are explained as follows:
#`   random_seed: the seed used when ranomly select the first ten culture conditions.
#'   exploration: the frequency of switching to random sampling. e.g. exploration=k means in every k+1 iterations, random sampling is used once.
#'   adaptive: 0 or 1. 1 means adaptive sampling is used. For the explaination of adaptive sampling, see the Supplementary material of our paper. 
#'   start_size: the number of culture conditions in the training set at the begining of the simulation.
#'   batch_size: the number of samples added in each iteration.
#'   dataset_id: the id for the dataset used. Because we tested the method on seven synthetic dataset and one RNA-seq dataset. dataset 1-7 are synthetic and dataset 8 is the RNA-seq.
#'   noise: the level of white noised added to the synthetic dataset. The numbers represent the percentage of variance of the white noise compared to the target value.
#'   iter_num: the total number of iterations in one simulation.
#'   method: the name of the sampling approach used in each iteration. Tan, a short for tanimoto, represents expert sampling. Random means random sampling. MI and EN denotes mutual information and entropy respectively.
     

args <- commandArgs(trailingOnly=TRUE)
setting_file <- args[1]

setting <- list(
               random_seed=c(51:100), 
               exploration=c(1), 
               adaptive=c(0), 
               start_size=c(15), 
               batch_size=c(1, 3), 
               dataset_id=8, 
               noise=0, 
               iter_num=30, 
               method="Tan", # It can be one of these options, "Tan", "Random", "MI", and "EN"
               )
setting <- expand.grid(setting)
write.csv(setting, paste("../../out_data/", setting_file, sep=""), row.names <- FALSE)
print(nrow(setting))
