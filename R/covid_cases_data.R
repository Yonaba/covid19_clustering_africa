setwd("D:/MOD_COVID19/CLUST/")
Sys.setenv(TZ = "UTC")


library(COVID19)
dates <- c("2021-08-31")

countries <- read.csv("data_completion/coord_africa.csv", header = T, sep = ",", dec = ".")$Code_ISO
df <- data.frame(matrix(nrow = 0, ncol = 3))
#colnames(df) <- dates

for (iso_code in countries) {
  print(iso_code)
  cc <- c()
  for (d in dates) {
    data <- covid19(country = iso_code, level = 1, 
                  start = "2020-01-01" , 
                  end = d)
    #val <- (data[[nrow(data),"confirmed"]] / data[[nrow(data),"population"]])
    cc <- c(as.numeric(data[[nrow(data),"confirmed"]]),
            as.numeric(data[[nrow(data),"deaths"]]), 
            as.numeric(data[[nrow(data),"population"]]))
  }
  df[nrow(df)+1,] <- cc
}
colnames(df) <- c("confirmed","dec","pop")
rownames(df) <- countries
#View(df)
write.csv(df, file="data_completion/covid_cases_data.csv",row.names = T)
