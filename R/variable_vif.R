setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(dplyr)
library(car)

df <- read.csv("data_completion/2_dataset_imputed.csv", header = T, sep = ",", dec = ".")
rownames(df) <- df$Code_ISO
df$Code_ISO <- NULL

conf <- select(df, -death_pm)
death <- select(df, -conf_pm)

conf.model <- lm(conf_pm ~., data = conf)
death.model <- lm(death_pm ~., data = death)

summary(conf.model)
summary(death.model)

vif.conf <- vif(conf.model)
vif.death <- vif(death.model)

vif.df <- data.frame(vif.conf, vif.death)
vif.df <- vif.df[order(vif.df$vif.conf, decreasing = F),]
write.csv(vif.df, file = "pub/vif_result.csv", row.names = T)

barplot(vif(conf.model), main = "Conf_pm VIF", horiz = TRUE, col = "steelblue",
        xlim = c(0,20))
abline(v = 5, lwd = 3, lty = 2)

barplot(vif(death.model), main = "Death_pm VIF", horiz = TRUE, col = "steelblue",
        xlim = c(0,20))
abline(v = 5, lwd = 3, lty = 2)
