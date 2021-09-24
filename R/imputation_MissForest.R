setwd("D:/MOD_COVID19/CLUST/data_completion/")
Sys.setenv(TZ = "UTC")

library(missForest)

df <- read.csv("1_dataset_filtered.csv", header = T, sep = ",", dec = ".")
data <- df[,2:(ncol(df)-2)]
data.imp <- missForest(data, verbose = TRUE)
print(paste("NRMSE (Imputation Error):",data.imp$OOBerror))
#View(data.imp$ximp)
cmp <- data.frame(df[,1],data.imp$ximp, df$conf_pm, df$death_pm)
colnames(cmp) <- colnames(df)
write.csv(cmp, file = "2_dataset_imputed.csv", row.names = F)
