setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

# https://datasciencebeginners.com/2018/11/26/functions-and-packages-for-feature-selection-in-r/#Example_3_-_Using_information_gain_for_variable_selection
# https://www.machinelearningplus.com/machine-learning/feature-selection/

library(caret)
library(gam)
library(scales)

rescale.df <- function(df, minv, maxv) {
  df.out <- data.frame(matrix(nrow = nrow(df), ncol = 0))
  for (col in colnames(df)) {
    rcol <- rescale(df[,col], to = c(minv, maxv))
    df.out <- cbind(df.out, rcol)
  }
  colnames(df.out) <- colnames(df)
  return (df.out)
}

set.seed(123)
df <- read.csv("data_completion/4_data_final.csv", header = T, sep = ",", dec = ".")

rownames(df) <- df$X
df$X <- NULL

outliers <- c("MUS", "SYC", "CPV")
row.names.remove.conf <- outliers
row.names.remove.death <- outliers

df.data.conf <- df[!(row.names(df) %in% row.names.remove.conf), ]
df.data.death <- df[!(row.names(df) %in% row.names.remove.death),]

x_conf <- rescale.df(df.data.conf[,1:(ncol(df.data.conf)-2)],0,1)
x_death <- rescale.df(df.data.death[,1:(ncol(df.data.death)-2)],0,1)

cases <- read.csv("data_completion/cases_pm.csv", header = T, sep = ",", dec = ".")
deaths <- read.csv("data_completion/deaths_pm.csv", header = T, sep = ",", dec = ".")
d <- 1

y_conf <- cases[,d]
y_death <- deaths[,d]

y_conf <- y_conf[-which(rownames(df) %in% outliers)]
y_death <- y_death[-which(rownames(df) %in% outliers)]

#y_conf2 <- df.data.conf$conf_pm
#y_death2 <- df.data.death$death_pm

filterCtrl <- sbfControl(functions = lmSBF, method = "repeatedcv", p = 0.75, repeats = 15)

filter_conf <- sbf(x_conf, y_conf, sbfControl = filterCtrl, max = 15)
filter_death <- sbf(x_death, y_death, sbfControl = filterCtrl, max = 15)
filter_conf
filter_death

rank.optVar <- function(filtered, x, y) {
  vars <- filtered$optVariables
  print(vars)
  corr <- corr.p <- c()
  for (v in vars) {
    corr <- append(corr, cor(as.numeric(x[,v]), y, method = "spearman"))
    corr.p <- append(corr.p, cor.test(as.numeric(x[,v]), y, method = "spearman")$p.value)
  }
  sig <- (corr.p < 0.05)
  out <- data.frame(corr,corr.p, sig)
  rownames(out) <- vars
  colnames(out) <- c("Corr_Spearman","p.value","sig")
  out <- out[order(abs(out$Corr_Spearman),decreasing = T),] 
  return (out)
}

out_conf <- rank.optVar(filter_conf, x_conf, y_conf)
out_death <- rank.optVar(filter_conf, x_death, y_death)

df.out <- data.frame(rownames(out_conf), out_conf$Corr_Spearman, out_conf$p.value,
                     rownames(out_death), out_death$Corr_Spearman, out_death$p.value)
colnames(df.out) <- rep(c("Variable", "Corr", "pvalue"),2)

View(df.out)

#write.csv(df.out, file = "pub/sbf_result.csv", row.names = F)
