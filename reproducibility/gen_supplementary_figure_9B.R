#' Reproduce Supplementary Figure 9B
#' Input: "./data/simulation_runs_prediction_MAEs.csv"
#' Output: "./figures/supplementary_figure_9B.pdf"
#'          

library(dplyr)
library(ggplot2)
library(scales)
require(cowplot)

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
  summary <- t.test(x, conf.level = 0.95)
  names(summary$estimate) <- NULL
  list(mean=summary$estimate[1], l=summary$conf.int[1], h=summary$conf.int[2], pvalue=summary$p.value)
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
OED_Method_Name = "EN"
num_iter <- 30
set.seed(1)
percent_format(digits = 0)

# 1) Read the data
df <- read.table(file.path("./data/simulation_runs_prediction_MAEs.csv"), sep = ",", header = TRUE )

# 2) Calculate mean, confidence intervals and p-value of relative difference to random per-iteration for each method
df_Random <- df[df$method == "Random", c(1:num_iter)]
df_OED_Diff <- (df_Random - df[df$method == OED_Method_Name, c(1:num_iter)])/df_Random
df_Expert_Diff <- (df_Random - df[df$method == "Expert", c(1:num_iter)])/df_Random
df_Replicate_Diff <- (df_Random - df[df$method == "replicate", c(1:num_iter)])/df_Random

df_summ_OED <- get_summary(df_OED_Diff, "OED")
df_summ_Expert <- get_summary(df_Expert_Diff, "Expert")
df_summ_Replicate <- get_summary(df_Replicate_Diff, "Replicate")
df_summ <- rbind(df_summ_OED, df_summ_Expert,  df_summ_Replicate)

# 3) Plot the summary (main part of the figure)
gPlot_p1 <- ggplot(df_summ, aes(x=iteration, y=mean, ymin=l, ymax=h, fill=method, colour=method))+
  geom_ribbon(alpha=0.3, colour = NA)+
  geom_line()+
  geom_point(size = 0.7)+
  scale_y_continuous("Improvement in MAE Compared\n To Random Sampling", labels = percent)+
  scale_x_continuous(NULL, breaks = NULL)+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_fill_manual(values = colors_assigned)+
  scale_colour_manual(values = colors_assigned)+
  get_custom_theme()
print(gPlot_p1)

# 4) Calculate mean, confidence intervals and p-value of difference to random per-iteration for each method
df_Random <- df[df$method == "Random", c(1:num_iter)]
df_OED_Diff <- (df_Random - df[df$method == OED_Method_Name, c(1:num_iter)])
df_Expert_Diff <- (df_Random - df[df$method == "Expert", c(1:num_iter)])
df_Replicate_Diff <- (df_Random - df[df$method == "replicate", c(1:num_iter)])

df_summ_OED <- get_summary(df_OED_Diff, "OED")
df_summ_Expert <- get_summary(df_Expert_Diff, "Expert")
df_summ_Replicate <- get_summary(df_Replicate_Diff, "Replicate")
df_summ <- rbind(df_summ_OED, df_summ_Expert, df_summ_Replicate)

# 5) Calculate corresponding p-values
gPlot_p2 <- ggplot(df_summ, aes(x=iteration, y=log(pvalue, base = 10), colour=method))+
  geom_line()+
  geom_point(size = 0.7)+
  scale_x_continuous("Number of Iterations", breaks = seq(0, num_iter, by = 5))+
  scale_y_continuous("p-value", breaks=c(0, -80, -160))+
  scale_fill_manual(values = colors_assigned)+
  scale_colour_manual(values = colors_assigned)+
  get_custom_theme()
print(gPlot_p2)

# 6) Save
gComb <- plot_grid(gPlot_p1, gPlot_p2, ncol=1, align="v", rel_heights = c(2,1))
print(gComb)
ggsave("./figures/supplementary_figure_9B.pdf", gComb, dpi = 600, width=6.2 , height =  3.5)
