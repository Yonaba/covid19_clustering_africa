setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")
library(dplyr)

df <- read.csv("data_completion/3_data_final.csv", header = T, sep = ",", dec = ".")
rownames(df) <- df$X
df$X <- NULL

#Fitting a linear model
lm_mod_conf <- lm(conf_pm ~ ., data=select(df, -death_pm))
lm_mod_dth <- lm(death_pm ~ ., data=select(df, -conf_pm))
summary(lm_mod_conf)
summary(lm_mod_dth)

# Outlier detection by cook distance
# source: http://r-statistics.co/Outlier-Treatment-With-R.html

res <- 500
factor <- res/72

png("pub/outlier_detection.png", units = "px", 
    width = 614 * factor, height = 361 * factor, 
    res = res,
    restoreConsole = TRUE)

#dev.off()
par(mfrow=c(1,2))

cooksd_conf <- cooks.distance(lm_mod_conf)
plot(cooksd_conf, pch="*", cex=2, main="a) Outliers on confirmed cases", 
     ylab = "Cook's distance (D)")
abline(h = 4*mean(cooksd_conf, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd_conf)+1, y=cooksd_conf, 
     labels=ifelse(cooksd_conf>4*mean(cooksd_conf, na.rm=T),names(cooksd_conf),""), col="red") 

cooksd_deaths <- cooks.distance(lm_mod_dth)
plot(cooksd_deaths, pch="*", cex=2, main="b) Outliers on deaths",
     ylab = "Cook's distance (D)")
abline(h = 4*mean(cooksd_deaths, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd_deaths)+1, y=cooksd_deaths, 
     labels=ifelse(cooksd_deaths>4*mean(cooksd_deaths, na.rm=T),names(cooksd_deaths),""), col="red")

dev.off()

filtered <- cooksd_conf
out.cook <- names(filtered)[(filtered > 4*mean(filtered, na.rm=T))]
out.cook <- as.character(c("CPV", "MUS", "SYC"))
df.out.cook <- data.frame(df[out.cook,])

df.n <- colMeans(df[!(rownames(df) %in% out.cook),])
df.mx <- as.numeric(sapply(df[!(rownames(df) %in% out.cook),], max, simplify = T))
df.mn <- as.numeric(sapply(df[!(rownames(df) %in% out.cook),], min, simplify = T))

df.out.cook <- rbind(df.out.cook, df.mn, df.n, df.mx)
rownames(df.out.cook)[length(out.cook) + 1] <- "Min"
rownames(df.out.cook)[length(out.cook) + 2] <- "Average"
rownames(df.out.cook)[length(out.cook) + 3] <- "Max"
View(df.out.cook)
write.csv(df.out.cook, file = "pub/df.out.cook.csv", row.names = T)
# Other Influence measures
# https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html
# library(olsrr)
# 
# out.1 <- ols_plot_cooksd_bar(lm_mod_conf)
# rownames(df[which(out.1$data$color=="outlier"),])
# 
# out.2 <- ols_plot_dffits(lm_mod)
# rownames(df[which(out.2$data$color=="outlier"),])
# 
# out.3 <- ols_plot_resid_stand(lm_mod)
# rownames(df[which(out.3$data$color=="outlier"),])
# 
# out.4 <- ols_plot_resid_lev(lm_mod)
# rownames(df[which(out.4$data$color=="outlier"),])
# 
# out.5 <- ols_plot_hadi(lm_mod)
# rownames(df[which(out.5$data$hdi>5),])
# 
