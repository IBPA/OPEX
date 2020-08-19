#' Reproduce Figure 3A
#' Input: "./data/Cross_Resistance.csv"
#' Output: "./figures/figure_3A.pdf"
#'          

library(Rtsne)
library(ggplot2)
library(ggrepel)

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

# General settings
set.seed(2)

# 1) Read the data
df <- read.table(file.path("./data/Cross_Resistance.csv"), sep = ",", header = TRUE )

# 2) Run t-SNE
tsne <- Rtsne(df[,c(6:ncol(df))], dims = 2, perplexity=10, verbose=TRUE, max_iter = 10000)

# 3) Append condition data to t-SNE projections
df_tsne_data <- data.frame(tsne$Y)
colnames(df_tsne_data) <- c("Dim1", "Dim2")
df_new <- df[,c(1:5)]
df_new <- cbind(df_new, df_tsne_data)

# 4) Plot t-SNE projection
gPlot <- ggplot(df_new, aes(x=Dim1, y=Dim2, fill=Fitness, colour=Biocide_Group, label=Antibiotic))+
  geom_text_repel(aes(label=Biocide),  box.padding=0.7, size=2.5)+
  geom_point(size=4, shape = 21, stroke = 1.5)+
  geom_text(colour="black", size=2.5)+
  scale_fill_gradient(low = "#FFFFFF", high="#A8AFB9")+
  xlab("t-SNE 1")+
  ylab("t-SNE 2")+
  get_custom_theme()
print(gPlot)

# 5) Save
ggsave("./figures/figure_3A.pdf", gPlot, dpi = 600, width=6.2 , height =  3.5)
