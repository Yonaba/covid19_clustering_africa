setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(rstatix)
library(ggplot2)

# Performs ANOVA + Post-hoc Tukey's HSD test
kruskal.wilcoxon <- function(df, var) {
  df.k <- df[df$ind == var,]
  k.t <- kruskal_test(values ~ clusters, data = df.k)
  w.t <- wilcox_test(values ~ clusters, p.adjust.method = "bonferroni", data = df.k)
  print(k.t)
  print("-------------")
  print(w.t)
}

df <- read.csv("data_completion/clustering.csv", header = T, sep = ",", dec = ".")
clusters <- df$clusters
data <- colnames(subset(df, select = -c(Code_ISO, clusters, conf_pm,death_pm)))
cluster.vars <- data

#data.df <- read.csv("data_completion/4_data_final.csv", header = T, sep = ",", dec = ".")
data <- df[, data]

clusters <- rep(clusters, ncol(data))
data.stack <- stack(data)
data.stack <- data.frame(data.stack, clusters)
data.stack$clusters <- as.factor(clusters)

ggplot(data.stack, aes(x = ind, y = values, fill = clusters)) + 
  scale_y_continuous(trans = "log10") +
  geom_boxplot(outlier.shape = 10) + ylab("") + xlab("") +
  facet_wrap(~ind, scale="free") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.size = unit(1.75, "cm"),
        strip.text.x = element_text(size = 12))

ggsave(filename = "pub/cluster_by_variable.png", dpi = 500, scale = 1.75, 
       width = 20, height = 15, unit = "cm")

for (param in cluster.vars) {
  print(paste(param))
  kruskal.wilcoxon(data.stack, param)
}


