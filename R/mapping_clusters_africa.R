setwd("D:/MOD_COVID19/CLUST")
Sys.setenv(TZ = "UTC")

library(ggplot2)
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

world <- ne_countries(scale = "medium", returnclass = "sf")

df <- read.csv("data_completion/clustering.csv", header = T, sep = ",", dec = ".")
colnames(df)[1] <- "iso_a3"
rownames(df) <- df$iso_a3
df["ESH",] <- df["MAR",]
df["ESH",1] <- "ESH"

africa <- merge(world, df, by = "iso_a3")
africa$Clusters <- as.factor(africa$clusters)

dev.off()
ggplot(data = africa) +
  geom_sf(aes(fill = Clusters)) +
  annotation_scale(location = "bl", line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  theme(legend.position = c(0.1, 0.15), 
        legend.background = element_rect(fill="lightgrey"),
        legend.key.size = unit(1, "cm")) +
  labs(title = "")

ggsave(filename = "pub/cluster_map.png", dpi = 500, scale = 1.75)

