setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(COVID19)
library(ggplot2)

s_date <- "2020-01-01"
e_date <- "2022-03-31"
N_DAYS <- as.numeric((as.Date(e_date) - as.Date(s_date)) + 1)

countries <- read.csv("data_completion/coord_africa.csv",header = T, sep = ",", dec = ".")
rownames(countries) <- countries$Code_ISO

df.out <- data.frame(matrix(nrow=0, ncol=2))
for (iso_code in rownames(countries)) {
  #iso_code <- "DZA"
  print(iso_code)
  data <- data.frame(covid19(country = iso_code, level = 1, 
                  start = s_date, end = e_date, verbose = F))
  conf.index <- as.numeric(which(data$confirmed>0)[1])
  first.conf.date <- as.numeric(data$date[conf.index] - as.Date(s_date))+1
  
  death.index <- as.numeric(which(data$deaths>0)[1])
  first.death.date <- ifelse(is.na(death.index),
                             NA,
                             as.numeric(data$date[death.index] - as.Date(s_date))+1)
  

  df.out[nrow(df.out)+1,] <- c(first.conf.date, first.death.date)
}
lats <- rep(countries$Latitude,ncol(df.out))
labels <- as.factor(rep(rownames(countries), ncol(df.out)))

df.out <- stack(df.out)
origin.labels <- as.factor(c(rep("1st Confirmed case",  length(rownames(countries))), 
                             rep("1st death", length(rownames(countries)))))
df.out <- data.frame(labels, lats, df.out$values, origin.labels)
colnames(df.out) <- c("Iso_Code","Latitude","Value","Origin")
df.out <- df.out[order(df.out$Latitude, decreasing = T),]
df.out$dates <- as.Date(df.out$Value, origin=as.Date(s_date))

dev.off()
ggplot(df.out,aes(x= dates, y = reorder(Iso_Code, Latitude))) +
  geom_line(aes(group = Iso_Code))+
  geom_point(aes(color=Origin), size=3) +
  #theme_classic(10) + 
  theme(legend.position="top") +
  xlab("") + ylab("Countries (from North to South latitudes)") +
  labs(title = "",
       subtitle = "",
       caption = "Source : JHU/CSSE") + 
  scale_colour_discrete("") +
  theme(legend.key.size = unit(1.5, "cm"))

ggsave(filename = "pub/timeline_covid.png", dpi = 500, scale = 1.75,
       width = 10, height = 15.5, units = "cm")
