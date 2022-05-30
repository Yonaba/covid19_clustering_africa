setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(dplyr)
library(car)

#df <- read.csv("data_completion/2_dataset_imputed.csv", header = T, sep = ",", dec = ".")
df <- read.csv("data_completion/3_data_final.csv", header = T, sep = ",", dec = ".")
prev <- read.csv("data_completion/prev_data.csv", header = T, sep = ",", dec = ".")
dates <- c("Sep20", "Mar21", "Sep21", "Mar22")

colnames(df)[1] <- "Code_ISO"
rownames(df) <- df$Code_ISO
df$Code_ISO <- NULL

df <- select(df, -c(conf_pm, death_pm))
vif.all <- data.frame(matrix(nrow=ncol(df), ncol=0))

vif_thsld <- 10

for (i in 1:4) {
  #i <- 2
  d <- dates[i]
  conf_pm <- prev[,i]
  death_pm <- prev[,i+4]
  
  conf <- data.frame(df, conf_pm)
  death <- data.frame(df, death_pm)
  
  conf.model <- lm(conf_pm ~., data = conf)
  death.model <- lm(death_pm ~., data = death)
  
  print(paste0("Date of ANalysis ", d))
  print(summary(conf.model))
  print(summary(death.model))
  
  vif.conf <- vif(conf.model)
  vif.death <- vif(death.model)
  vif.df <- data.frame(vif.conf, vif.death)
  vif.all <- data.frame(vif.all, vif.df)
}

colnames(vif.all) <- paste0(rep(c("conf_", "death_"),2),rep(dates, each=2))
vif.all <- vif.all[order(vif.all$conf_Mar22, decreasing = F),]

write.csv(vif.all, file = "pub/vif_result.csv", row.names = T)

vif_var <- rownames(vif.all[vif.all$conf_Mar22 <= vif_thsld,])

df.vif <- df[,(colnames(df) %in% vif_var)]
df.vif <- data.frame(df.vif, conf_pm, death_pm)

write.csv(df.vif, file = "data_completion/4_data_final.csv", row.names = T)
