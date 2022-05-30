setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(ggplot2)
library(viridis)
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(gridExtra)

world <- ne_countries(scale = "medium", returnclass = "sf")

df <- read.csv("data_completion/clustering.csv", header = T, sep = ",", dec = ".")
countries <- df$Code_ISO

df.conf <- read.csv("data_completion/cases_pm.csv", header = T, sep = ",", dec = ".")
df.deaths <- read.csv("data_completion/deaths_pm.csv", header = T, sep = ",", dec = ".")
rownames(df.conf) <- rownames(df.deaths) <- df.conf$Code_ISO

df.conf["ESH",] <- df.conf["MAR",]
df.deaths["ESH",] <- df.deaths["MAR",]
df.conf["ESH",]$Code_ISO <- "ESH"
df.deaths["ESH",]$Code_ISO <- "ESH"

dates <- c("Sep20", "Mar21", "Sep21", "Mar22")
colnames(df.conf) <- colnames(df.deaths) <- c(dates, "Code_ISO")
colnames(df.conf)[5] <- colnames(df.deaths)[5] <-"iso_a3"

conf.africa <- merge(world, df.conf, by = "iso_a3")
deaths.africa <- merge(world, df.deaths, by = "iso_a3")

dev.off()

max(df.conf$Mar22)
max(df.deaths$Mar22)
breaks.conf <- c(50, 20000,100000,425000)
breaks.deaths <- c(10, 500, 3600)
legend.y <- 0.3

p1 <- ggplot(data = conf.africa) +
  geom_sf(aes(fill = Sep20)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotate("text", x=-20, y=35, label= "a)", size = 6, fontface = "bold") + 
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "a) Cumulative cases - (09/30/2020)") +
  theme(legend.position = c(0.12, legend.y), legend.background = element_rect(fill="lightgrey"))

p2 <- ggplot(data = conf.africa) + 
  geom_sf(aes(fill = Mar21)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) +  
  annotate("text", x=-20, y=35, label= "b)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "b) Cumulative cases - (03/31/2021)") +
  theme(legend.position = c(0.14, legend.y), legend.background = element_rect(fill="lightgrey"))

p3 <- ggplot(data = conf.africa) + 
  geom_sf(aes(fill = Sep21)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) +  
  annotate("text", x=-20, y=35, label= "c)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "c) Cumulative cases - (09/30/2021)") +  
  theme(legend.position = c(0.14, legend.y), legend.background = element_rect(fill="lightgrey"))

p4 <- ggplot(data = conf.africa) + 
  geom_sf(aes(fill = Mar22)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotate("text", x=-20, y=35, label= "d)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "d) Cumulative cases - (03/31/2022)") +
  theme(legend.position = c(0.16, legend.y), legend.background = element_rect(fill="lightgrey"))
  

#grob.cases <- grid.arrange(p1, p2, p3, p4, nrow = 2)
#ggsave(filename = "pub/cloropleth_cases.png", grob.cases, dpi = 500, scale = 1.75)

#dev.off()
p11 <- ggplot(data = deaths.africa) +
  geom_sf(aes(fill = Sep20)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.deaths, trans = "log", labels = breaks.deaths,
                        limits = c(head(breaks.deaths,1), tail(breaks.deaths,1))) + 
  annotate("text", x=-20, y=35, label= "e)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "e) Cumulative deaths - (09/30/2020)") +
  theme(legend.position = c(0.12, legend.y), legend.background = element_rect(fill="lightgrey"))

p22 <- ggplot(data = deaths.africa) + 
  geom_sf(aes(fill = Mar21)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.deaths, trans = "log", labels = breaks.deaths,
                        limits = c(head(breaks.deaths,1), tail(breaks.deaths,1))) + 
  annotate("text", x=-20, y=35, label= "f)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "f) Cumulative deaths - (03/31/2021)") +
  theme(legend.position = c(0.14, legend.y), legend.background = element_rect(fill="lightgrey"))

p33 <- ggplot(data = deaths.africa) + 
  geom_sf(aes(fill = Sep21)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.deaths, trans = "log", labels = breaks.deaths,
                        limits = c(head(breaks.deaths,1), tail(breaks.deaths,1))) + 
  annotate("text", x=-20, y=35, label= "g)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "g) Cumulative deaths - (09/30/2021)") +
  theme(legend.position = c(0.14, legend.y), legend.background = element_rect(fill="lightgrey"))

p44 <- ggplot(data = deaths.africa) + 
  geom_sf(aes(fill = Mar22)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.deaths, trans = "log", labels = breaks.deaths,
                        limits = c(head(breaks.deaths,1), tail(breaks.deaths,1))) + 
  annotate("text", x=-20, y=35, label= "h)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "h) Cumulative deaths - (03/31/2022)") +
  theme(legend.position = c(0.16, legend.y), legend.background = element_rect(fill="lightgrey"))

#grob.deaths <- grid.arrange(p11, p22, p33, p44, nrow = 2)
#ggsave(filename = "pub/cloropleth_deaths.png", grob.deaths, dpi = 500, scale = 1.75)

grob.all <- grid.arrange(p1, p2, p3, p4, p11, p22, p33, p44, nrow = 2)
ggsave(filename = "pub/cloropleth_maps.png", grob.all, dpi = 500, scale = 1.75,
       width = 25, height = 15, units = "cm")
       