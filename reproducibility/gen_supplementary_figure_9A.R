#' Reproduce Supplementary Figure 9A
#' Inputs: "./data/simulation_runs_prediction_MAEs.csv"
#'         "./data/simulation_runs_iterations_prediction_MAE_LT_144_EN.csv"
#' Output: "./figures/supplementary_figure_9A.pdf"
#'          

library(dplyr)
library(ggplot2)
library(scales)
require(cowplot)
library(gridExtra)
library(reshape2)

#' Get the personalized theme used throughout the script
get_custom_theme <- function() {
  font_ratio <- 1.25
  return(theme(panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               legend.box.background = element_blank(),
               plot.title = element_text(hjust = 0.5),
               legend.title=element_blank(),
               legend.key.width = unit(3, "line"),
               legend.key = element_blank(),
               legend.position = "none",
               strip.background = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.border = element_rect(fill=NA, size=1),
               legend.key.size = unit(0.5, "cm"),
               axis.text = element_text(size=7*font_ratio),
               axis.title = element_text(size=8*font_ratio, face='bold'),
               text = element_text(family="Helvetica", size=7*font_ratio)))
}

#' Calculate the mean and confidence intervals for a given iteration.
my_t_test <- function(x){
  summary <- t.test(x, mu=mean(x), conf.level = 0.95)
  names(summary$estimate) <- NULL
  list(mean=summary$estimate[1], l=summary$conf.int[1], h=summary$conf.int[2])
}

#' Calculate the mean and condifence intervals for all iterations.
get_summary <- function(df_Diff, method){
  summ <- apply(df_Diff, 2, my_t_test)
  df_summ <- bind_rows(summ)
  df_summ$iteration <- c(1:num_iter)
  df_summ$method <- method
  
  return(df_summ)
}

# General settings
colors_assigned <- c("OED"="#3bb273","replicate"="#4d9de0" ,"Replicate"="#4d9de0", 
                     "Random"="#e1bc29", "Expert"="#e15554", "Imbalance" = "#000000",
                     "-1"="#e15554", "1"="#4d9de0")
OED_Method_Name_Other = "MI"
OED_Method_Name = "EN"
num_iter <- 30
set.seed(1)
percent_format(digits = 0)

# 1) Read the data
df <- read.table(file.path("./data/simulation_runs_prediction_MAEs.csv"), sep = ",", header = TRUE )

# 2) Calculate mean and confidence intervals per-iteration for each method
df_summ_Random <- get_summary(df[df$method == "Random", c(1:num_iter)], "Random")
df_summ_OED <- get_summary(df[df$method == OED_Method_Name, c(1:num_iter)], "OED")
df_summ_Expert <- get_summary(df[df$method == "Expert", c(1:num_iter)], "Expert")
df_summ_Replicate <- get_summary(df[df$method == "replicate", c(1:num_iter)], "Replicate")
df_summ <- rbind(df_summ_OED, df_summ_Expert, df_summ_Replicate, df_summ_Random)

# 3) Plot the summary in figure_2B_p1.pdf
gPlot_p1 <- ggplot(df_summ, aes(x=iteration, y=mean, ymin=l, ymax=h, fill=method, colour=method))+
  geom_ribbon(alpha=0.3, colour = NA)+
  geom_line()+
  geom_point(size = 0.7)+
  scale_x_continuous("Number of Iterations", breaks = seq(0, num_iter, by = 5), limits = c(0, 31))+
  scale_y_continuous("Average MAE")+
  scale_fill_manual(values = colors_assigned)+
  scale_colour_manual(values = colors_assigned)+
  get_custom_theme()
print(gPlot_p1)

# 4) Create data summary relating to the iteration with maximum gap in MAE 
#       between the random and OED methods (iteration 27)
df_oed_means <- df_summ[df_summ$method == "OED", c("iteration", "mean")]
df_rnd_means <- df_summ[df_summ$method == "Random",c("iteration", "mean")]
idx_max_gap <- df_oed_means[which.max(df_rnd_means$mean - df_oed_means$mean),]$iteration
df_max_iter <- df[df$method != OED_Method_Name_Other ,c(idx_max_gap, ncol(df))]
df_max_iter$method <- factor(df_max_iter$method)
levels(df_max_iter$method)[levels(df_max_iter$method)==OED_Method_Name] <- "OED"
df_max_iter$method <- factor(df_max_iter$method, levels=c("replicate", "OED", "Random", "Expert"))
colnames(df_max_iter) <- c("MAE", "method")

# 5) Plot the boxplot for MAEs at iteration with maximum gap
gPlot_p2 <- ggplot(df_max_iter, aes(y=MAE, x=method, fill=method))+
  geom_boxplot(width=.5, outlier.size = .3, lwd = .3)+
  scale_y_continuous(limits = quantile(df_max_iter$MAE, c(0.0, 0.9)), breaks = c(0.1, 0.2))+
  #scale_y_continuous(limits = c(mae_min, mae_max))+
  coord_flip()+
  scale_fill_manual(values = colors_assigned)+
  get_custom_theme()
print(gPlot_p2)

# 6) Find number of iterations necessary to achieve error of OED at iteration 15 (MAE=0.144)
df_iter <- read.table(file.path("./data/simulation_runs_iterations_prediction_MAE_LT_144_EN.csv"), sep = ",", header = TRUE )
df_iter$Replicate <- 1
df_iter_m <- melt(df_iter, measure.vars = colnames(df_iter), value.name = "Iteration", variable.name = "Method")
df_iter_m$Method <- factor(df_iter_m$Method, levels=c("Replicate", "OED", "Random", "Expert"))
# Calculate pvalue for : OED reaches error before Random
t.test(df_iter$OED, df_iter$Random, alternative = "less", paired = TRUE)

# 7) Plot the boxplot for number of iterations necessary to achive error of OED at iteration 15 (MAE=0.144)
gPlot_p3 <- ggplot(df_iter_m, aes(y=Iteration, x=Method, fill=Method))+
  geom_boxplot(width=.5, outlier.size = .3, lwd = .3)+
  scale_y_continuous(na.value = 31, breaks = seq(0, num_iter, by = 5), limits = c(0, 31))+
  scale_fill_manual(values = colors_assigned)+
  coord_flip()+
  get_custom_theme()
print(gPlot_p3)  

gCombA <- plot_grid(gPlot_p3, gPlot_p2, ncol=2, nrow = 1, rel_widths = c(1,1))
gComb <- plot_grid(gCombA, gPlot_p1, ncol=1, nrow = 2, rel_heights = c(1, 2))
print(gComb)
ggsave("./figures/supplementary_figure_9A.pdf", gComb, dpi = 600, width=5 , height =  3.5)
