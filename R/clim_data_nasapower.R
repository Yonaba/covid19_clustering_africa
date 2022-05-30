setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(nasapower)
library(zoo)

cli_params <- c("WS2M", "RH2M", "T2MDEW","T2M_MAX",
                 "T2M_MIN", "T2M","ALLSKY_SFC_SW_DWN")

start_date <- "2020-01-01"
end_date <- "2022-03-31"
SLEEP_TIME_BETWEEN_REQUESTS <- 10

countries <- read.csv("data_completion/coord_africa.csv",header = T, sep = ",", dec = ".")

df.out <- data.frame(matrix(nrow = 0, ncol = length(cli_params)+1))
df.names <- c("ws2m", "rh2m", "tdew", "tmax", "tmin", "tmoy", "insol", "ah")

for (i in 1:length(countries$Code_ISO)) {
  #i <- "5"
  #i <- 5
  country <- countries[i,"Code_ISO"]
  print(paste("Processing:", country))
  cli_df <- get_power(
    community = "ag",
    lonlat = c(countries[i,"Longitude"], countries[i,"Latitude"]),
    pars = cli_params,
    dates = c(start_date, end_date),
    temporal_api = "daily")
  df <- data.frame(cli_df[,7:ncol(cli_df)])
  colnames(df)[1] <- "Date"
  
  for (col in colnames(df)) {
    if (col != "Date") {
      if (is.na(df[1, col])) df[1, col] <- mean(df[,col], na.rm=T)
      if (is.na(df[nrow(df), col])) df[nrow(df), col] <- mean(df[,col], na.rm=T)
      df[,col] <- na.approx(df[,col], na.rm = T)
    }
  }
  df$AH <- (6.112 * exp((17.67*df$T2M)/(df$T2M + 243.5)) * df$RH2M * 2.1674) / (df$T2M + 273.15)
  stats <- as.numeric(colMeans(df[,2:ncol(df)], na.rm = F))
  #stats <- append(sum(df$PRECTOTCORR), stats)
  
  df.out[nrow(df.out)+1,] <- stats
  
  Sys.sleep(SLEEP_TIME_BETWEEN_REQUESTS)
}

rownames(df.out) <- countries$Code_ISO
colnames(df.out) <- df.names

write.csv(df.out, file = "data_completion/climate_nasapower.csv", row.names = T)

print("finished")
