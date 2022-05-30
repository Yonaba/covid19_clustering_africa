setwd("D:/MOD_COVID19/CLUST/")
Sys.setenv(TZ = "UTC")

library(zoo)
library(data.table)
library(COVID19)
library(ggplot2)
library(gridExtra)
library(hrbrthemes)

s_date <- c("2020-01-01")
e_date <- c("2022-03-31")
N_DAYS <- as.numeric((as.Date(e_date) - as.Date(s_date)) + 1)

countries <- read.csv("data_completion/coord_africa.csv", header = T, sep = ",", dec = ".")$Code_ISO
df.cc <- data.frame(matrix(nrow = N_DAYS, ncol = 0))
df.dd <- data.frame(matrix(nrow = N_DAYS, ncol = 0))

for (iso_code in countries) {
  print(iso_code)

  covid.data <- covid19(country = iso_code, level = 1, 
                        start = s_date, end = e_date, verbose = F)
  
  sindex_cc <- which.min(is.na(covid.data$confirmed))
  sindex_dd <- which.min(is.na(covid.data$deaths))
  
  
  covid_cc <- covid.data$confirmed[sindex_cc:length(covid.data$confirmed)]
  covid_dd <- covid.data$deaths[sindex_dd:length(covid.data$deaths)]
  
  covid_cc <- floor(na.approx(covid_cc))
  covid_dd <- floor(na.approx(covid_dd))
  
  covid_cc <- append(rep(0,N_DAYS-length(covid_cc)), covid_cc)
  covid_dd <- append(rep(0,N_DAYS-length(covid_dd)), covid_dd)
  
  df.cc[,iso_code] <- covid_cc
  df.dd[,iso_code] <- covid_dd
  
}

inds <- seq(as.Date(s_date),as.Date(e_date), by = "day")
df.out <- data.frame(inds, rowSums(df.cc, na.rm = T), rowSums(df.dd, na.rm = T))
colnames(df.out) <- c("dates", "cases","deaths")

dev.off()
coef <- 0.05
p1 <- ggplot() +
  geom_area(data = df.out,aes(x = dates, y = cases), color = "blue", fill = "blue") + 
  geom_area(data = df.out,aes(x = dates, y = deaths/coef), color = "red", fill = "red") +
  xlab("") +
  scale_y_continuous(
    name = "Cumulative confirmed cases",
    sec.axis = sec_axis(~.*coef, name="Cumulative deaths")
  ) + theme_ipsum() +
  theme(
    axis.title.y = element_text(color = "blue", size = 18),
    axis.title.y.right = element_text(color = "red", size = 18)
  ) +
  labs(title = "a) Cumulative confirmed cases and deaths in Africa",
     subtitle = "Period : January 1, 2020 to March 31, 2022",
     caption = "Source: JHU/CSSE") +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=12)) +
  theme(legend.position = "none")

df.out$new_cases <- append(df.out$cases[1], diff(df.out$cases))
df.out$new_deaths <- append(df.out$deaths[1], diff(df.out$deaths))

coef <- 0.05
p2 <- ggplot() +
  stat_smooth(data = df.out, mapping = aes(x = dates, y = new_cases), fill = "blue", alpha = 1, span = 1/7, geom = 'area', method = 'loess') +
  stat_smooth(data = df.out, mapping = aes(x = dates, y = new_deaths/coef), fill = "red", alpha = 1, span = 1/7, geom = 'area', method = 'loess') + 
  #geom_area(data = df.out,aes(x = dates, y = new_cases), color = "blue", fill = "blue") + 
  #geom_area(data = df.out,aes(x = dates, y = new_deaths/coef), color = "red", fill = "red") +
  xlab("") +
  scale_y_continuous(
    name = "Daily confirmed cases",
    sec.axis = sec_axis(~.*coef, name="Daily deaths")
  ) + theme_ipsum() +
  theme(
    axis.title.y = element_text(color = "blue", size = 18),
    axis.title.y.right = element_text(color = "red", size = 18)
  ) +
  labs(title = "b) Daily confirmed cases and deaths in Africa",
       subtitle = "Period : January 1, 2020 to March 31, 2022",
       caption = "Source: JHU/CSSE") +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=12)) +
  theme(legend.position = "none")

grob <- grid.arrange(p1, p2, nrow = 1)
ggsave(filename = "pub/covid_cases_africa.png", grob, dpi = 500, scale = 3, 
       width = 25, height = 8, unit = "cm")
