setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(COVID19)
library(zoo)
library(xts)
library(ggplot2)
library(viridis)

cumulative <- F
var_type <- "confirmed" # or "confirmed"

s_date <- "2020-01-01"
e_date <- "2022-03-31"
N_DAYS <- as.numeric((as.Date(e_date) - as.Date(s_date)) + 1)

df <- read.csv("data_completion/coord_africa.csv", header = T, sep = ",", dec = ".")
countries <- rownames(df) <- df$Code_ISO
regions <- unique(df$Regions)

df.cases <- sapply(regions,function(x) NULL)
for (region in regions) {
  #region <- "AN"
  reg_countries <- df[df$Regions==region,"Code_ISO"]
  df.cases[[region]] <- data.frame(matrix(nrow=N_DAYS, ncol = length(reg_countries)))
  colnames(df.cases[[region]]) <- reg_countries
}

for (iso_code in countries) {
  print(iso_code)
  region <- df[iso_code,"Regions"]
  data <- covid19(country = iso_code, level = 1, 
                  start = s_date, end = e_date, verbose = F)
  
  start_index <- which.min(is.na(data[[var_type]]))
  start_date <- data$date[start_index]
  end_date <- data$date[length(data$date)]
  covid_cases <- (data[[var_type]])[start_index:length(data[[var_type]])]
  covid_cases <- floor(na.approx(covid_cases))
  if (!cumulative) covid_cases <- append(covid_cases[1], diff(covid_cases))
  cc <- append(rep(NA,N_DAYS-length(covid_cases)), covid_cases)
  df.cases[[region]][[iso_code]] <- cc
}

df.sum <- data.frame(matrix(nrow = N_DAYS, ncol = 0))
for (region in names(df.cases)) {
  sums <- rowSums(df.cases[[region]], na.rm = T)
  df.sum <- cbind(df.sum, sums)
}
colnames(df.sum) <- names(df.cases)
dates <- seq(as.Date(s_date),as.Date(e_date), by = "day")

dates <- rep(dates, length(df.sum))
df.sum <- data.frame(dates, stack(df.sum))
colnames(df.sum) <- c("Dates","Cases","Regions")

dev.off()
df.sum$Regions <- factor(df.sum$Regions, levels=c("AA","AN","AE","AO","AC"))  

fname <- ""
if (!cumulative) {
  ggplot(df.sum, aes(x=Dates, y=Cases, fill=Regions)) + 
    stat_smooth(data = df.sum, alpha = 0.6, span = 1/7, geom = 'area', method = 'loess') + 
    xlab("") + ylab(ifelse(var_type == "confirmed", 
                           "Daily confirmed cases", 
                           "Daily deaths")) + 
    ylim(0,ifelse(var_type == "confirmed",25000,600)) +
    labs(title = ifelse(var_type == "confirmed", 
                        "Daily new cases per region in Africa",
                        "Daily deaths per region in Africa"),
         subtitle = "Period : January 1, 2020 to March 31, 2022",
         caption = "Source : JHU/CSSE")
  fname <- ifelse(var_type == "confirmed",
                  "pub/new_cases_by_regions.png",
                  "pub/new_deaths_by_regions.png")
} else {
  ggplot(df.sum, aes(x=Dates, y=Cases, fill=Regions)) + 
    geom_area() +
    xlab("") + ylab(ifelse(var_type == "confirmed", 
                           "Cumulative confirmed cases", 
                           "Cumulative deaths")) +
    labs(title = ifelse(var_type == "confirmed",
                        "Cumulative confirmed cases per region in Africa",
                        "Cumulative deaths per region in Africa"),
         subtitle = "Period : January 1, 2020 to March 31, 2022",
         caption = "Source : JHU/CSSE")
  fname <- ifelse(var_type == "confirmed",
                  "pub/cum_cases_by_regions.png",
                  "pub/cum_deaths_by_regions.png")  
}

ggsave(filename = fname, dpi = 500, scale = 1.75, width = 13, height = 8, unit = "cm")
