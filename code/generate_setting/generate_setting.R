args = commandArgs(trailingOnly=FALSE)
print(args[4])
setting_file = paste(gsub(".R$", "", gsub("--file=", "", args[4])), ".csv", sep="")
print(setting_file)

setting = list(
               random_seed = c(51:100),
               exploration=c(1),
               anti_batch = c(0),
               start_size = c(15),
               add=c(1, 3),
               data=14,
               noise=0,
               iter_num = 30, # this depends on the start_size
               method = c("Tan", "Random")
               )
setting = expand.grid(setting)
#setting["iter_num"] = 24/setting["add"]
write.csv(setting, paste("../../out_data/",setting_file,sep=""), row.names = FALSE)
print(nrow(setting))
