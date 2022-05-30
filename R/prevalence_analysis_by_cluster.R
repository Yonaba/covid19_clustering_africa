setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(rstatix)
library(ggplot2)
library(ggsignif)
library(COVID19)

s_date <- "2020-01-01"
e_date <- "2022-03-31"

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
pop_tot <- read.csv("data_completion/pop_tot.csv", header = T, sep = ",", dec = ".")$Pop_Tot
df <- data.frame(df, pop_tot)

clusters <- df$clusters
data <- subset(df, select = c(conf_pm,death_pm))
data$cfr <- (df$death_pm / df$conf_pm) * 100
data$mort <- (df$death_pm / pop_tot) * 1e6

# dates <- data.frame(matrix(ncol = 3, nrow = 0))
# for (iso_code in df$Code_ISO) {
#   print(iso_code)
#   
#   #r?cup?rer les donn?es concernant les cas cumul?s
#   covdata <- covid19(country = iso_code, level = 1, 
#                   start = s_date, end = e_date)
#   
#   conf.index <- as.numeric(which(covdata$confirmed>0)[1])
#   first.conf.date <- as.numeric(covdata$date[conf.index] - as.Date(s_date))+1
#   
#   death.index <- as.numeric(which(covdata$deaths>0)[1])
#   first.death.date <- ifelse(is.na(death.index),
#                              NA,
#                              as.numeric(covdata$date[death.index] - as.Date(s_date))+1)
#   
#   dates[nrow(dates)+1,] <- c(iso_code,covdata, first.death.date)
# }

#colnames(dates) <- c("Code_ISO","Conf_Date","Death_Date")
#data$order_conf <- order(dates$Conf_Date)
#data$order_death <- order(dates$Death_Date)

clusters <- rep(clusters, ncol(data))
data.stack <- stack(data)
data.stack <- data.frame(data.stack, clusters)
data.stack$clusters <- as.factor(clusters)

ggplot(data.stack, aes(x = ind, y = values, fill = clusters)) + 
  geom_boxplot(outlier.shape = 10) + scale_y_continuous(trans = "log10") +
  ylab("") + xlab("") +
  facet_wrap(~ind, scale="free", labeller = 
               labeller(ind = c(
                 "conf_pm" = "a) Confirmed cases (pm)",
                 "death_pm" = "b) Deaths (pm)",
                 "mort" = "d) Mortality rate (pm)",
                 "cfr" = "c) Case fatality rate (%)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.size = unit(1.75, "cm"),
        strip.text.x = element_text(size = 12))

#ggsave(filename = "pub/prevalence_by_cluster.png", dpi = 500, scale = 1.75)

kruskal.wilcoxon(data.stack, "conf_pm")
kruskal.wilcoxon(data.stack, "death_pm")
kruskal.wilcoxon(data.stack, "mort")
kruskal.wilcoxon(data.stack, "cfr")

