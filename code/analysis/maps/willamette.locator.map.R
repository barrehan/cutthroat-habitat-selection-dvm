setwd("C:/Users/barrehan/Documents/GitHub/cwa.habitat.selection.dvm")

# for loading our data
library(jsonlite)
library(rgdal)
library(sf)
# for plotting
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
library(usmap)
# for data wrangling
library(dplyr)

oregon <- read_sf("map.files/GOVTUNIT_Oregon_State_Shape/Shape/GU_StateOrTerritory.shp")
watershed <- read_sf("map.files/NHD_H_1709_HU4_Shape/Shape/WBDHU4.shp")
willamette_river <- read_sf("map.files/Mainstem_Willamette/Willamette_Mai_ExportFeature.shp")
willamette_tribs <- read_sf("map.files/Willamette_Tribs/Willamette_Main_Tribs.shp")
alcoves <- read.csv("map.files/alcove_sites.csv")
alcoves <- alcoves[alcoves$map_label == "Blue Ruin" | alcoves$map_label == "Norwood",]
city <- read.csv("map.files/city.gauge.locs.csv")
city <- city[city$map_label == "Corvallis" | city$map_label == "Eugene",]

crs <- 4326
ply <- st_polygon(list(as.matrix(data.frame(lat = c(44, 44, 44.6, 44.6, 44), lon = c(-123.05,-123.45,-123.45, -123.05, -123.05), crs = crs))))
points <-  st_as_sf(alcoves, coords = c("long", "lat"), crs = crs)
locs <- st_as_sf(city, coords = c("longitude", "latitude"), crs = crs)

# Oregon map with watershed and study area delimination
usa <-map_data('state')
states <- c("washington", "oregon", "california", "idaho", "nevada", "utah", "arizona", "montana", "wyoming", "colorado", "new mexico")
sub.state <- usa[usa$region %in% states,]

ggplot()+
  geom_polygon(data = sub.state, aes(x=long, y=lat, group = group), color = "black", fill = "white", lwd = .7)+
  geom_sf(data = oregon, color = "black", fill = "white", lwd = 1)+
  geom_sf(data = watershed, color = "black", fill = "white", lwd = 1)+
  geom_sf(data = willamette_river, color = "#1A3D82", lwd= 1)+
  geom_rect(aes(xmin = -123.425, xmax = -123.0, ymin = 44, ymax = 44.6), color = "#B14311", lwd = 1, fill = NA)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())


# Zoom in on study area


ggplot() +
  geom_sf(data = willamette_river, color = "#1A3D82", lwd = 1.5) +
  geom_sf(data = willamette_tribs, color = "#1A3D82") +
  geom_sf(data = points, shape = 21, fill = "#B14311", size = 2.25) +
  geom_sf_text(data = points, aes(label = map_label), 
               family = "serif", size = 4.5, 
               nudge_x = c(0.08, 0.075), 
               nudge_y = c(0, 0)) +  # Adjust x nudges for alcove labels
  geom_sf(data = locs, shape = 24, aes(label = map_label), 
          fill = "black", size = 2.25) +
  geom_sf_text(data = locs, aes(label = map_label), 
               family = "serif", size = 4.5, 
               nudge_x = c(-0.065, -0.06), 
               nudge_y = c(0, 0)) +  # Adjust x nudges for city labels
  coord_sf(xlim = c(-123.015, -123.425), ylim = c(44, 44.6), crs = crs) +
  ggspatial::annotation_scale(location = "bl",
                              bar_cols = c("black", "white"),
                              text_family = "serif",
                              pad_x = unit(0.3, "in"),
                              pad_y = unit(0.2, "in")) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                    pad_x = unit(0.47, "in"), pad_y = unit(0.36, "in"),
                                    style = ggspatial::north_arrow_nautical(
                                      fill = c("grey40", "white"),
                                      line_col = "black",
                                      text_family = "serif")) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())



