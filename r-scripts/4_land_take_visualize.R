library(tidyverse)
library(gridExtra)
library(ggpubr)
library(sf)
library(jpeg)
library(ggmap)


# Import country borders used - GISCO
# https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries
countries_sf <- st_read('./DATA/From_GEE/countries.geojson') %>%
  #filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  dplyr::select(country = ISO3_CODE)%>%
  st_transform(st_crs(3035))
countries_sf

# Import images of some example land takes
img1 <- readJPEG('./data/a14-3488168855972_46-22524504038693_2018.jpg') 
img2 <- readJPEG('./data/a14-3488168855972_46-22524504038693_2023.jpg')

img3 <- readJPEG('./data/a8-1065339242813_58-15356960204498_2018_nature.jpg')
img4 <- readJPEG('./data/a8-1065339242813_58-15356960204498_2023_nature.jpg')


# Plot the images with ggplot
imgPlot1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  background_image(img1) +
  coord_equal() +
  annotate("text", x = 1.5, y = 9.5, label = "2018", color='white', size=4) +
  theme(axis.title = element_blank()) +
  xlim(0,10) +
  ylim(0,10) +
  theme(axis.text = element_blank(),
        plot.margin = margin(t=0,r=0,b=0,l=0,unit='cm'),
        axis.ticks = element_blank()) +
  labs(title = 'B)')
imgPlot1

imgPlot2 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  background_image(img2) +
  coord_equal()+
  annotate("text", x = 1.5, y = 9.5, label = "2023", color='white', size=4) +
  theme(axis.title = element_blank()) +
  xlim(0,10) +
  ylim(0,10) +
  theme(axis.text = element_blank(),
        plot.margin = margin(t=0,r=0,b=0,l=0,unit='cm'),
        axis.ticks = element_blank()) +
  labs(title = ' ')
imgPlot2

imgPlotRow1 <- grid.arrange( imgPlot1, imgPlot2, ncol=2,
                                 padding = unit(0, "line"),widths=c(1,1), newpage = T)
imgPlotRow1

imgPlot3 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  background_image(img3) +
  coord_equal()+
  annotate("text", x = 1.5, y = 9.5, label = "2018", color='white', size=4) +
  theme(axis.title = element_blank()) +
  xlim(0,10) +
  ylim(0,10) +
  theme(axis.text = element_blank(),
        plot.margin = margin(t=0,r=0,b=0,l=0,unit='cm'),
        axis.ticks = element_blank()) +
  labs(title = 'C)')
imgPlot3

imgPlot4 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  background_image(img4) +
  coord_equal() +
  annotate("text", x = 1.5, y = 9.5, label = "2023", color='white', size=4) +
  theme(axis.title = element_blank()) +
  xlim(0,10) +
  ylim(0,10) +
  theme(axis.text = element_blank(),
        plot.margin = margin(t=0,r=0,b=0,l=0,unit='cm'),
        axis.ticks = element_blank()) +
  labs(title = ' ')
imgPlot4

imgPlotRow2 <- grid.arrange( imgPlot3, imgPlot4, ncol=2,
                             padding = unit(0, "line"),widths=c(1,1), newpage = T)

imgPlotRows <- grid.arrange( imgPlotRow1, imgPlotRow2, ncol=2,
                             padding = unit(0, "line"),widths=c(1,1), newpage = T)


# Import grid with land take areas
grid_sf <- st_read('./data/From_GEE/grid50km.geojson')  %>% 
  st_transform(st_crs(3035))
grid_sf %>%
  ggplot() + geom_sf()

gridAreas <- read_csv('./data/From_GEE/areas_grid_v3.csv')%>%
  mutate(stratum = ifelse(stratum == 5, 'ltNature',
                          ifelse(stratum == 6, 'ltCropland', NA))) %>%
  drop_na(stratum) %>%
  mutate(area = area/1000000) %>%
  dplyr::select(id, stratum, area) %>%
  pivot_wider(values_from = area, names_from=stratum) %>%
  mutate(ltCropland = ifelse(is.na(ltCropland), 0, ltCropland))
gridAreas
sum(gridAreas$ltNature)
sum(gridAreas$ltCropland, na.rm=T)

# Find the grid cells with highest land take and then get land takes for those as kml
View(gridAreas %>% mutate(tot = ltCropland + ltNature))


# Prepare for bivariate color map
#https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
#https://bluegreenlabs.org/post/map-building-3/
bivariate1 <- c('#3b4799', '#8a61af', '#bb63b1', 
                '#5a97bc', '#a5acd7', '#ddb0d9', 
                '#63c8c9', '#afe4e4', '#e8e8e8')
bivSel <- bivariate1
legend_3 <- tibble(
  "3 - 3" = bivSel[1], # high nature, high cropland
  "2 - 3" = bivSel[2],
  "1 - 3" = bivSel[3],
  "3 - 2" = bivSel[4],
  "2 - 2" = bivSel[5],
  "1 - 2" = bivSel[6],
  "3 - 1" = bivSel[7], 
  "2 - 1" = bivSel[8],
  "1 - 1" = bivSel[9] 
) %>%
  gather("group", "fill")

df <- gridAreas

df %>%
  ggplot(aes(x=ltCropland, y=ltNature)) +
  geom_point()

varX <- 'ltNature'
varY <- 'ltCropland'

hist(df[[varX]])
hist(df[[varY]])

labelX <- 'Land take nature'
labelY <- 'Land take cropland'
title <- 'A) Land take of nature and cropland'

datBiVarToPlot <- df %>% 
  mutate(x =df[[varX]],
         y = df[[varY]]) %>%
  dplyr::select(id, x, y) 

limsY <- quantile(datBiVarToPlot$y, probs=c(0.0, 0.9))
#limsY <- c(min(datBiVarToPlot$y), max(datBiVarToPlot$y))
breaksY <- seq(limsY[1], limsY[2], (limsY[2]-limsY[1])/6)
labsY <- round(c(breaksY[2], breaksY[4], breaksY[6]))
legBreaksY <- c(breaksY[2], breaksY[4], breaksY[6])

limsX <- quantile(datBiVarToPlot$x, probs=c(0.0, 0.9))
#limsX <- c(min(datBiVarToPlot$x), max(datBiVarToPlot$x))
breaksX <- seq(limsX[1], limsX[2], (limsX[2]-limsX[1])/6)
labsX <- round(c(breaksX[2], breaksX[4], breaksX[6]))
legBreaksX <- c(breaksX[2], breaksX[4], breaksX[6])

# Create legend
g1 <- legend_3 %>%
  separate(group,
           into = c(varY, varX),
           sep = " - ") %>%
  mutate_at(vars(varX), function(varX){dplyr::recode(varX, "1" = legBreaksX[1], "2" = legBreaksX[2], "3" = legBreaksX[3])}) %>%
  mutate_at(vars(varY), function(varY){dplyr::recode(varY, "1" = legBreaksY[1], "2" = legBreaksY[2], "3" = legBreaksY[3])}) %>%
  ggplot() +
  geom_tile(mapping = aes(x =.data[[varX]],y = .data[[varY]],  fill = fill)) +
  scale_fill_identity() +
  labs(x = paste0(labelX, '  →'), y = paste0(labelY, '  →')) +
  theme(axis.title = element_text(size = 8)) +
  scale_y_continuous(oob=scales::squish,  
                     limits=c(limsY[1],limsY[2]), 
                     labels = labsY,
                     breaks=legBreaksY) +
  scale_x_continuous(oob=scales::squish,
                     limits=c(limsX[1],limsX[2]), 
                     labels = labsX,
                     breaks=legBreaksX) +
  theme(plot.background = element_blank())
g1

# Add labels and fill values to df
datMap <- datBiVarToPlot  %>%
  mutate_at(vars(x), function(x){ifelse(x <= breaksX[3], "1",
                                        ifelse(x >= breaksX[5], "3",
                                               "2"))}) %>%
  mutate_at(vars(y), function(y){ifelse(y <= breaksY[3], "1",
                                        ifelse(y >= breaksY[5], "3",
                                               "2"))})  %>%
  mutate(
    group = paste(y, x, sep = " - ")
  )

datMap <- left_join(datMap, legend_3)

toMap <- datMap %>%
  left_join(grid_sf) %>%
  st_as_sf()

# Make bivariate map
m1 <- ggplot() +
  geom_sf(data = countries_sf) +
  geom_sf(data = toMap, color='#666666', aes(fill=fill), alpha=0.8, linewidth=0.05) +
  geom_sf(data = grid_sf %>% filter(id == 89499666), fill=NA, color='red', linewidth=1) +
  scale_fill_identity() + 
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size=7)) +
  labs(x='')+
  theme(axis.title = element_text(size = 11),
        plot.title = element_text(size=11)) +
  xlim(1500000, 6980000)+
  ylim(1000000, 5300000) +
  ggtitle(title) 
m1

# Make inset map with land takes in grid cell with most land take
# Import pre-exported land takes
ltNatreGrid <- st_read('./data/land_take_inside_grid_top_nature.kml') %>% #bb63b1 
  st_transform(st_crs(3035)) %>%
  st_filter(grid_sf %>% filter(id == 89499666))
ltCroplandGrid <- st_read('./data/land_take_inside_grid_top_cropland.kml') %>% #63c8c9 
  st_transform(st_crs(3035))%>%
  st_filter(grid_sf %>% filter(id == 89499666))

# Define bounding box and get basemap
inBox <- grid_sf %>% filter(id == 89499666) %>%
  st_buffer(-5000) %>%
  st_transform(st_crs(4326))  %>%
  st_bbox()
inBox
map <- get_stadiamap( bbox = c(left = inBox[[1]], bottom = inBox[[2]], right = inBox[[3]], top = inBox[[4]]),
                      zoom = 10, 
                      maptype = "stamen_toner_lite")

insetMap <- ggmap(map) +
  geom_sf(data = ltNatreGrid %>%
            st_transform(st_crs(4326)), fill='#bb63b1', color='#bb63b1', inherit.aes = F) +
  geom_sf(data = ltCroplandGrid %>%
            st_transform(st_crs(4326)), fill='#63c8c9', color='#63c8c9', inherit.aes = F) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t=0,r=0,b=0,l=0,unit='cm'),
        plot.background = element_blank(),
        panel.border = element_rect(colour = "red", fill=NA, linewidth=0.5))
  
insetMap

# Put it all together
grob <- ggplotGrob(g1)
grob2 <- ggplotGrob(insetMap)
b <- st_bbox(st_buffer(grid_sf, -10000))
bivarPlot <- m1 + annotation_custom(grob = grob, 
                                  xmin = 5600000,
                                  xmax = 6900000,
                                  ymin = 3600000,
                                  ymax = 4900000)+
  annotation_custom(grob = grob2, 
                    xmin = 1200000,
                    xmax = 2900000,
                    ymin = 2500000,
                    ymax = 4000000)
bivarPlot


landTakeMapPlot <-  grid.arrange( bivarPlot, imgPlotRows, ncol=1,
                               padding = unit(0, "line"),heights=c(3,1), newpage = T)

# Export
ggsave("./output/landTakeMapPlot.png", landTakeMapPlot, width = 20, height=22, units='cm')
