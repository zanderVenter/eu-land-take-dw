library(tidyverse)
library(sf)
library(gridExtra)
library(grid)

#### CLC overlap analysis ------------------------------------------------------
ecoPal <- c("#CC0303", 
            "#CDB400", 
            "#F7E174",
            "#92AF1F",
            "#B76124",
            "#235123", 
            "#32c2c8"
            )

ecoTypes <-  c("Artificial",
               "Cropland" ,
               "Sparsely vegetated land",
               "Grassland",
               "Heathland and shrub" ,
               "Woodland and forest",
               "Wetlands, riparian and coastal")


overlap <- read_csv('./data/From_GEE/clc_overlap_areas_v3.csv')  %>%
  mutate(ecoType = ifelse(stratum <= 142, 'Artificial',
                        ifelse((stratum >= 211 & stratum <= 223) | (stratum >= 241 & stratum <= 244), 'Cropland',
                               ifelse((stratum >= 311 & stratum <= 313) | stratum == 324, 'Woodland and forest',
                                      ifelse(stratum == 231 | stratum == 321, 'Grassland',
                                             ifelse(stratum >= 322 & stratum <= 323, 'Heathland and shrub',
                                                    ifelse(stratum >= 331 & stratum <= 335, 'Sparsely vegetated land',
                                                           ifelse(stratum >= 411 , 'Wetlands, riparian and coastal', NA)))))))) %>%
  drop_na(ecoType)
unique(overlap$type)
colSums(is.na(overlap))


# EU level ---
clcOverlapEU <- overlap %>%
  #filter(type == 'loss') %>%
  group_by(ecoType, type) %>%
  summarise(area = sum(area)/1000000)  %>%
  group_by(type) %>%
  mutate(areaTot = sum(area),
         perc = area/areaTot*100) %>%
  ungroup() %>%
  pivot_wider(values_from = c('area','areaTot', 'perc'), names_from = type) %>%
  mutate(ecoType = factor(ecoType, levels = ecoTypes))

clcOverlapEU

clcOverlapEUplot <- clcOverlapEU %>%
  mutate(ecoType = str_wrap(ecoType, width = 15))%>%
  ggplot(aes(y = reorder(ecoType, perc_loss), x = perc_loss)) +
  geom_bar(stat='identity') +
  geom_label(aes(label = paste(round(perc_loss, 1), "%"), x = perc_loss+2), size=3) +
  scale_fill_manual(values=ecoPal) +
  labs(y = 'Ecosystem type',
       x = 'Land take share\n of total loss (%)',
       title = 'A) Share per ecosystem type') +
  theme(legend.position = 'none')
clcOverlapEUplot

clcOverlapRelEUplot <- clcOverlapEU %>%
  dplyr::select(ecoType, area_loss, area_allLand,perc_loss) %>%
  mutate(areaRel = area_loss / area_allLand * 100 ) %>%
  ungroup() %>%
  mutate(areaRelTot = sum(areaRel),
         areaRelPerc = areaRel / areaRelTot*100)   %>%
  ggplot(aes(y = reorder(ecoType, perc_loss), x = areaRel)) +
  geom_bar(stat='identity') +
  geom_label(aes(label = paste(round(areaRel, 2), "%"), x = areaRel+0.05), size=3) +
  scale_fill_manual(values=ecoPal)+
  labs(y = 'Ecosystem type',
       x = 'Land take share\n of baseline ecosystem area (%)',
       title = 'B) Share of baseline ecosystem area') +
  theme(legend.position = 'none')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
clcOverlapRelEUplot



#### Ecoregion overlap analysis ------------------------------------------------------

ecoLookup <- read_csv('./data/ecoRegionLookup.csv') %>%
  dplyr::select(stratum=ECO_ID, ECO_NAME, BIOME_NAME)

ecoregOverlap <- read_csv('./data/From_GEE/ecoregion_overlap_areas_v3.csv') %>%
  left_join(ecoLookup, by = 'stratum') %>%
  filter(BIOME_NAME != 'N/A')

ecoOverlapEU <- ecoregOverlap %>%
  group_by(BIOME_NAME, type) %>%
  summarise(area = sum(area)/1000000)  %>%
  group_by(type) %>%
  mutate(areaTot = sum(area),
         perc = area/areaTot*100) %>%
  ungroup() %>%
  pivot_wider(values_from = c('area','areaTot', 'perc'), names_from = type)

ecoOverlapEU

ecoOverlapEUplot <- ecoOverlapEU %>%
  mutate(BIOME_NAME = str_wrap(BIOME_NAME, width = 15)) %>%
  ggplot(aes(y = reorder(BIOME_NAME, perc_loss), x = perc_loss)) +
  geom_bar(stat='identity') +
  geom_label(aes(label = paste(round(perc_loss, 1), "%"), x = perc_loss+3), size=3) +
  scale_fill_manual(values=ecoPal) +
  labs(y = 'Biome',
       x = 'Land take share\n of total loss (%)',
       title = 'C) Share per biome') +
  theme(legend.position = 'none')
ecoOverlapEUplot

ecoOverlapRelEUplot <- ecoOverlapEU %>%
  dplyr::select(BIOME_NAME, area_loss, area_allLand,perc_loss) %>%
  mutate(areaRel = area_loss / area_allLand * 100 ) %>%
  ungroup() %>%
  mutate(areaRelTot = sum(areaRel),
         areaRelPerc = areaRel / areaRelTot*100)   %>%
  ggplot(aes(y = reorder(BIOME_NAME, perc_loss), x = areaRel)) +
  geom_bar(stat='identity') +
  geom_label(aes(label = paste(round(areaRel, 2), "%"), x = areaRel+0.01), size=3) +
  scale_fill_manual(values=ecoPal)+
  labs(y = 'Biome',
       x = 'Land take share\n of baseline biome area (%)',
       title = 'D) Share of baseline biome area') +
  theme(legend.position = 'none') +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
ecoOverlapRelEUplot



clcEcoregionOverlapPlot <- grid.arrange(clcOverlapEUplot, clcOverlapRelEUplot, ecoOverlapEUplot, ecoOverlapRelEUplot, ncol=4,
              padding = unit(0, "line"),widths=c(1.2,1,1.2,1), newpage = T)


ggsave("./output/clcEcoregionOverlapPlot.png", clcEcoregionOverlapPlot, width = 40, height=16, units='cm')


