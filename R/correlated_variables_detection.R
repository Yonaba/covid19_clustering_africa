setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

df <- read.csv("data_completion/2_dataset_imputed.csv", header = T, sep = ",", dec = ".")
rownames(df) <- df$Code_ISO
df$Code_ISO <- NULL

# NearZeroVariance Check
library(caret)
library(rstatix)
x <- df[,c(1:(ncol(df)-2))]
nearZeroVar(x, names = T)

# Correlated variables
library(ggcorrplot)
cor.df <- cor(df, method = "spearman")
cor.pmat <- cor_pmat(df, method = "spearman", alternative = "two.sided", conf.level = 0.95)
cor.pmat <- round(cor.pmat, digits = 4)
diag(cor.pmat) <- ""
cor.pmat[cor.pmat == "0"] <- "<0.001"
write.csv(cor.df, file = "pub/cor_mat_var.csv", row.names = T)
write.csv(cor.pmat, file = "pub/cor_pmat_var.csv", row.names = T)

ggcorrplot(cor.df, type = "lower",legend.title = "rho (Spearman)\n")
ggsave(filename = "pub/corrplot_variables.png", dpi = 500, scale = 1.7, width = 20, height = 20, unit = "cm")

corr_var <- findCorrelation(cor(x, method = "spearman"), cutoff = 0.8, names = T, exact = T, verbose = T)
print(paste("Variables redondantes : ", paste(corr_var, collapse=" / ")))

keep_var <- c("arriv","gini","alphab","dens_pop", "urb_pop", "life_exp", "prev_malaria","alcohol_cons", 
              "med_1000", "lack_hygien", "immuniz_dtp1", "immuniz_bcg", "pm25",
              "epi","lat_abs", "ws2m_avg", "insol_avg")

df.data <- df[,(names(df) %in% keep_var)]
df.data$conf_pm <- df$conf_pm
df.data$death_pm <- df$death_pm

corr_var2 <- findCorrelation(cor(df.data, method = "spearman"), cutoff = 0.8, names = T, exact = T)
print(paste("Variables redondantes : ", paste(corr_var2, collapse=" / ")))
# cor.df2 <- cor(df.data, method = "spearman")

write.csv(df.data, file = paste0("data_completion/3_data_final.csv"), row.names = T)
