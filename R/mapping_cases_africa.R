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

dates <- c("Mar20", "Jun20", "Jan21", "Aug21")
colnames(df.conf) <- colnames(df.deaths) <- c(dates, "Code_ISO")
colnames(df.conf)[5] <- colnames(df.deaths)[5] <-"iso_a3"

conf.africa <- merge(world, df.conf, by = "iso_a3")
deaths.africa <- merge(world, df.deaths, by = "iso_a3")

dev.off()

max(df.conf$Aug21)
max(df.deaths$Aug21)
breaks.conf <- c(1, 30,1000,50000)
breaks.deaths <- c(1,10,100,1500)
legend.y <- 0.3

p1 <- ggplot(data = conf.africa) +
  geom_sf(aes(fill = Mar20)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotate("text", x=-20, y=35, label= "a)", size = 6, fontface = "bold") + 
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "a) Cumulative cases - (03/31/2020)") +
  theme(legend.position = c(0.12, legend.y), legend.background = element_rect(fill="lightgrey"))

p2 <- ggplot(data = conf.africa) + 
  geom_sf(aes(fill = Jun20)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) +  
  annotate("text", x=-20, y=35, label= "b)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "b) Cumulative cases - (06/30/2020)") +
  theme(legend.position = c(0.14, legend.y), legend.background = element_rect(fill="lightgrey"))

p3 <- ggplot(data = conf.africa) + 
  geom_sf(aes(fill = Jan21)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) +  
  annotate("text", x=-20, y=35, label= "c)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "c) Cumulative cases - (01/31/2021)") +  
  theme(legend.position = c(0.14, legend.y), legend.background = element_rect(fill="lightgrey"))

p4 <- ggplot(data = conf.africa) + 
  geom_sf(aes(fill = Aug21)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotate("text", x=-20, y=35, label= "d)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "d) Cumulative cases - (08/31/2021)") +
  theme(legend.position = c(0.16, legend.y), legend.background = element_rect(fill="lightgrey"))
  

#grob.cases <- grid.arrange(p1, p2, p3, p4, nrow = 2)
#ggsave(filename = "pub/cloropleth_cases.png", grob.cases, dpi = 500, scale = 1.75)

#dev.off()
p11 <- ggplot(data = deaths.africa) +
  geom_sf(aes(fill = Mar20)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.deaths, trans = "log", labels = breaks.deaths,
                        limits = c(head(breaks.deaths,1), tail(breaks.deaths,1))) + 
  annotate("text", x=-20, y=35, label= "e)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "e) Cumulative deaths - (03/31/2020)") +
  theme(legend.position = c(0.12, legend.y), legend.background = element_rect(fill="lightgrey"))

p22 <- ggplot(data = deaths.africa) + 
  geom_sf(aes(fill = Jun20)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.deaths, trans = "log", labels = breaks.deaths,
                        limits = c(head(breaks.deaths,1), tail(breaks.deaths,1))) + 
  annotate("text", x=-20, y=35, label= "f)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "f) Cumulative deaths - (06/30/2020)") +
  theme(legend.position = c(0.14, legend.y), legend.background = element_rect(fill="lightgrey"))

p33 <- ggplot(data = deaths.africa) + 
  geom_sf(aes(fill = Jan21)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.deaths, trans = "log", labels = breaks.deaths,
                        limits = c(head(breaks.deaths,1), tail(breaks.deaths,1))) + 
  annotate("text", x=-20, y=35, label= "g)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "g) Cumulative deaths - (01/31/2021)") +
  theme(legend.position = c(0.14, legend.y), legend.background = element_rect(fill="lightgrey"))

p44 <- ggplot(data = deaths.africa) + 
  geom_sf(aes(fill = Aug21)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.deaths, trans = "log", labels = breaks.deaths,
                        limits = c(head(breaks.deaths,1), tail(breaks.deaths,1))) + 
  annotate("text", x=-20, y=35, label= "h)", size = 6, fontface = "bold") +
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "h) Cumulative deaths - (08/31/2021)") +
  theme(legend.position = c(0.16, legend.y), legend.background = element_rect(fill="lightgrey"))

#grob.deaths <- grid.arrange(p11, p22, p33, p44, nrow = 2)
#ggsave(filename = "pub/cloropleth_deaths.png", grob.deaths, dpi = 500, scale = 1.75)

grob.all <- grid.arrange(p1, p2, p3, p4, p11, p22, p33, p44, nrow = 2)
ggsave(filename = "pub/cloropleth_maps.png", grob.all, dpi = 500, scale = 1.75,
       width = 22, height = 12, units = "cm")
       