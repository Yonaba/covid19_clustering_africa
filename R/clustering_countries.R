setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(FactoMineR)
library(factoextra)
library(clValid)
library(NbClust)
library(hopkins)
library(scales)
library(ggdendro)

rescale.df <- function(df, minv, maxv) {
  df.out <- data.frame(matrix(nrow = nrow(df), ncol = 0))
  for (col in colnames(df)) {
    rcol <- rescale(df[,col], to = c(minv, maxv))
    df.out <- cbind(df.out, rcol)
  }
  colnames(df.out) <- colnames(df)
  rownames(df.out) <- rownames(df)
  return (df.out)
}

best_var <- read.csv("pub/sbf_result.csv", header = T, sep = ",", dec = ".")$Variable
df <- read.csv("data_completion/4_data_final.csv", header = T, sep = ",", dec = ".")
rownames(df) <- df$X
df$X <- NULL

data <- df[,(colnames(df) %in% best_var)]

orig.data <- data               
data <- rescale.df(data, 0, 1)

method <- "ward.D2"
distance <- "canberra"
  
# selection of NUM_PCS
# pca <- prcomp(data, scale = F)
# cum <- cumsum(pca$sdev^2/sum(pca$sdev^2))
# cum
# 
# # Running PCA
# NUM_PCS <-  2 #Number of PCs
# pca.df <- data.frame(data, df$conf_dec20, df$deaths_dec20)
# #colnames(pca.df)[ncol(pca.df)] <- "cc"
# res.pca <- PCA(scale(pca.df), quanti.sup = (NCOL(pca.df)-1):NCOL(pca.df), 
#                ncp = NUM_PCS, axes = c(1,2))
# res.pca$eig #Visualize Eigenvalues to select number of PCs

# If > 0.5, data should not be clustered
hopkins(data)

# # Clustering
# # Elbow method
# fviz_nbclust(data, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")
# 
# # Silhouette method
# fviz_nbclust(data, kmeans, method = "silhouette")+
#   labs(subtitle = "Silhouette method")
# 
# # Gap statistic
# # nboot = 50 to keep the function speedy. 
# # recommended value: nboot= 500 for your analysis.
# # Use verbose = FALSE to hide computing progression.
# set.seed(125)
# fviz_nbclust(data, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
#   labs(subtitle = "Gap statistic method")

# distances = c("euclidean", "manhattan", "canberra", "minkowski")
# agg_methods = c("ward.D", "ward.D2", "complete", "average")
# 
# coph.df <- data.frame(matrix(nrow = 0, ncol = 3))
# for (d in distances) {
#   for (m in agg_methods) {
#     hc <- hclust(dist(data, method = d), method = m)
#     hh <- cophenetic(hc)
#     coph.df[nrow(coph.df)+1,] <- c(d, m, cor(dist(data, method = d), hh))
#     #print(paste(d,m,cor(dist(data, method = d), hh)))
#   }
# }
# colnames(coph.df) <- c("dist", "meth", "coph")
# coph.df <- coph.df[order(coph.df$coph, decreasing = T),]
# 
# CLUST <- NbClust(data = data, method = "ward.D2", distance = "canberra", index = "all")
# CLUST <- data.frame(CLUST$Best.nc)
# K_CLUST <- as.numeric(tail(names(sort(table(as.numeric(CLUST["Number_clusters",])))),1))

# K_CLUST = 4  
# # Evaluate DunnIndex
# distances = c("euclidean", "manhattan", "canberra", "minkowski")
# agg_methods = c("ward.D", "ward.D2", "complete", "average")
# 
# dunn.df = data.frame(matrix(ncol = length(distances), nrow = length(agg_methods)))
# rownames(dunn.df) <- agg_methods
# colnames(dunn.df) <- distances
# 
# for (distance in distances) {
#   for (method in agg_methods) {
#     print(paste("Processing",distance, method))
#     CLUST <- NbClust(data = data, method = method, distance = distance, index = "all")
#     CLUST <- data.frame(CLUST$Best.nc)
#     K_CLUST <- as.numeric(tail(names(sort(table(as.numeric(CLUST["Number_clusters",])))),1))
#     print(paste(distance, method, K_CLUST))
#     # hca = hcut(
#     #   data,
#     #   k = K_CLUST,
#     #   isdiss = inherits(data, "dist"),
#     #   hc_func = c("hclust"),
#     #   hc_method = method,
#     #   hc_metric = distance,
#     #   stand = TRUE,
#     #   graph = TRUE,
#     # )
#     
#     # d = dist(data, method=distance)
#     # dunn_index = dunn(d, clusters = hca[["cluster"]])
#     # dunn.df[method,distance] = dunn_index
#   }
# }
# 
# View(dunn.df)

K_CLUST = 3

# Clustering with selected distance/method
hca = hcut(
  data,
  k = K_CLUST,
  isdiss = inherits(data, "dist"),
  hc_func = c("hclust"),
  hc_method = method,
  hc_metric = distance,
  stand = TRUE,
  graph = TRUE,
)

# Visualize clusters : set axes to change perspective
clust<-fviz_cluster(hca, ellipse.type = "convex", axes = c(1,2))
#clust

dev.off()
# Visualize dendrogram
fviz_dend(hca, rect = F, rect_fill = F, main = "", ylab = "Cophenetic distance (height)",
          k_colors = c("red","green3","blue"), lwd = 1) 
ggsave(filename = "pub/dendrogram.png", dpi = 500, scale = 2.5, width = 10, height = 7, unit = "cm")

#cophenetic(clust)

clusters <- cutree(hca, k = K_CLUST)
df.out <- data.frame(rownames(df), clusters, orig.data, df$conf_pm, df$death_pm)
colnames(df.out)[ncol(df.out)-1] <- "conf_pm"
colnames(df.out)[ncol(df.out)] <- "death_pm"
colnames(df.out)[1] <- "Code_ISO"
write.csv(df.out, file = "data_completion/clustering.csv", row.names = F)

