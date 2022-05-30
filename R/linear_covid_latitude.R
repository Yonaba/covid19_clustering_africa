setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")
library(ggplot2)
library(hrbrthemes)
library(gridExtra)


lats <- read.csv("data_completion/coord_africa.csv", header = T, sep = ",", dec = ".")$Latitude
lats <- abs(lats)
conf_pm <- read.csv("data_completion/4_data_final.csv", header = T, sep = ",", dec = ".")$conf_pm
death_pm <- read.csv("data_completion/4_data_final.csv", header = T, sep = ",", dec = ".")$death_pm

df <- data.frame(lats, log(conf_pm), log(death_pm))
colnames(df) <- c("lats", "conf_pm", "death_pm")

r2_conf <- summary(lm(conf_pm ~ lats, df))$r.squared
r2_death <- summary(lm(death_pm ~ lats, df))$r.squared

pv_conf <- cor.test(lats, conf_pm, alternative = "two.sided", method = "spearman")$p.value
pv_death <- cor.test(lats, death_pm, alternative = "two.sided", method = "spearman")$p.value

p1 <- ggplot(df, aes(x=lats, y=conf_pm)) +
  annotate("text", x = 30, y = 6, label = paste0("y = ",round(lm(conf_pm ~lats, df)$coefficients[2], 3),
                          "x +", round(lm(conf_pm ~ lats, df)$coefficients[1], 3)), fontface = 2) +
  annotate("text", x = 30, y = 5.5, label = paste0("R²: ",round(r2_conf, 3)), fontface = 4) +  
  annotate("text", x = 30, y = 5, label = paste0("p-value: ",round(pv_conf, 3)), fontface = 4) +   
  annotate("text", x = 0, y = 12.5, label = "a)", fontface = 2) +
  geom_point() +
  labs(x = "Absolute latitude (degrees)", y = "log(Cumulative cases per million)") + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum_pub()

p2 <- ggplot(df, aes(x=lats, y=death_pm)) +
  annotate("text", x = 28, y = 1, label = paste0("y = ",round(lm(death_pm ~ lats, df)$coefficients[2], 3),
                                                   "x + ", round(lm(death_pm ~ lats, df)$coefficients[1], 3)), fontface = 2) +
  annotate("text", x = 30, y = 0.55, label = paste0("R²: ",round(r2_death, 3)), fontface = 4) +   
  annotate("text", x = 30, y = 0.1, label = paste0("p-value: ",round(pv_death, 3)), fontface = 4) +    
  annotate("text", x = 0, y = 7.8, label = "b)", fontface = 2) +
  geom_point() +
  labs(x = "Absolute latitude (degrees)", y = "log(Cumulative deaths per million)") + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum_pub()

grob.plot <- grid.arrange(p1, p2, nrow = 1)
ggsave(filename = "pub/linear_latitude_cases_deaths.png", grob.plot, dpi = 500, scale = 1.5,
       width = 16, height = 8, units = "cm")
