library(mlegp)

setwd("./workhorse")
source("../setpath.R")
source("OOPGP.R")

args = commandArgs(TRUE)
setting_file = args[1]
setting_id = as.integer(args[2])

out_data_path = paste(out_data_path, sub("\\..*$",'',setting_file),sep="/")
dir.create(out_data_path, showWarnings = FALSE)

# read in the configuration file and slice one setting
settings = read.csv(paste("../../out_data/",setting_file,sep=""), stringsAsFactors=FALSE)
s = settings[setting_id,,drop=TRUE]

# parse the setting into a list
setting_ = list(
                 "random_seed" = s[["random_seed"]],
                 "exploration"=s[["exploration"]],      
                 "anti_batch" = s[["anti_batch"]],
                 "start_size" = s[["start_size"]],
                 "add"= s[["add"]],
                 "data"=s[["data"]],
                  "noise"=s[["noise"]],
                  "iter_num" = s[["iter_num"]],
                  "method"= s[["method"]]
                   )

# create the path for the input data
file_path =  file.path(data_path, paste(setting_[["data"]],".csv",sep=""))

# create an object of the Simulator class
simulator = Simulator(file_path,setting_)

# load the support data for expert sampling
dists = read.csv(file.path(data_path, "pairwise_similarity_bio.csv"), row.names=1)
simulator$dists = dists

# run the simulator
simulate(simulator)


