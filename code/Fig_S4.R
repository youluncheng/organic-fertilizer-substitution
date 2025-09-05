
library(tidyverse)
library(data.table)
site <- readxl::read_xlsx('...Data/FigS4.xlsx',sheet = x1)
site <- as.data.table(site)
site[crop_type=='wheat', crop_type := 'Wheat']
site[crop_type=='maize', crop_type := 'Maize']
site[crop_type=='rice', crop_type := 'Rice']


names(site)[names(site)=="crop_type"] <- "Crop type"
site$`Crop type` <- factor(site$`Crop type`, levels = c('Wheat','Maize','Rice'))

library(sf)

china_map <- st_read("...Data/provincial_boundary.shp")

library(ggplot2)

p1 <- ggplot() +
  geom_sf(data = china_map, color = "#838B8B", fill = "#E0EEEE", size = 0.1) +
  geom_point(data = site,
             aes(x = lon, y = lat, color = `Crop type`, shape = `Crop type`),
             alpha = 1, size = 1) +
  scale_color_manual(values = c("Wheat" = "indianred3",
                                "Maize" = "seagreen3",
                                "Rice" = "royalblue3")) +
  scale_shape_manual(values = c("Wheat" = 15, 
                                "Maize" = 17,  
                                "Rice" = 16)) + 
  theme_bw() +
  theme(panel.grid =element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.1, 0.3),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 12, color = "black"),
        legend.background = element_blank())

