library(tidyverse)
library(sf)
library(broom)
library(caret)
library(gridExtra)

# Check threshold values - optimal is 5
# Check months in year - optimal is grid-specific months

#### Data import and cleaning -----------------------------------------------------------


# Import country borders used - GISCO
# https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries
countries_sf <- st_read('./DATA/From_GEE/countries.geojson') %>%
  #filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  dplyr::select(country = ISO3_CODE)%>%
  st_transform(st_crs(3035))
countries_sf
unique(countries_sf$country)

ggplot() + geom_sf(data=countries_sf)


monthLookup <- read_csv('./data/For_GEE/export_grid_with_monthsets.csv') %>%
  dplyr::select(-geo)
gridMonths <- st_read('./data/From_GEE/export_grid.geojson') %>%
  left_join(monthLookup, by = 'CellCode')

gridMonths %>%
  ggplot(aes(fill=monthSet)) +
  #annotation_map_tile(type = "osm") +
  geom_sf() 

# Samples generated in GEE based on pilot version of the map
samplesRaw <- read_csv('./data/From_GEE/pilot_samples.csv') 

samples<- samplesRaw %>% 
  st_as_sf(coords=c('LONGITUDE','LATITUDE')) %>%
  dplyr::select(REFID = PLOTID) 

# Join with the month sets
st_crs(samples) <-st_crs(gridMonths)
sampleMetadata <- samples %>%
  st_join(gridMonths) %>%
  drop_na()
nrow(sampleMetadata)
colSums(is.na(sampleMetadata))
unique(sampleMetadata$monthSet)

# Import the verification points from sampling app
refSamplersRaw <- read_csv('./data/From_samplers/public_arena_pilot_collection - Sheet1.csv') 

refSamplers <- refSamplersRaw %>%
  dplyr::select(REFID, reference = verification) %>%
  filter(reference %in% c('yes', 'no'))

# Import full DW timeseries for reference locations - exported from GEE
refdw <- read_csv('./data/From_GEE/pilot_dw_timeseries.csv')%>%
  mutate(month = month(date), year = year(date))

#### Quick check to see the metadata distributions ---------------------------------------------

refSamplersRaw %>%
  ggplot(aes(x=verification)) +
  geom_bar()

# how much nature loss versus cropland loss
refSamplersRaw %>%
  filter(verification == 'yes') %>%
  drop_na(baseline_land_use) %>%
  mutate(lossType = ifelse(baseline_land_use == 'cropland', 'cropland loss', 
                           ifelse(baseline_land_use == 'uncertain', 'uncertain', 'nature loss'))) %>%
  count(lossType, baseline_land_use) %>%
  mutate(perc = n/sum(n)*100) %>%
  ggplot(aes(x=reorder(baseline_land_use, perc), fill=lossType, y=perc)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(y='Percentage (%)',
       x = 'Land cover in 2018') +
  scale_fill_manual(values = c('#e9c100', '#63c8c9', 'grey')) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.2),
        legend.background = element_blank())

# distribution across years
refSamplersRaw %>%
  filter(verification == 'yes') %>%
  ggplot(aes(x=factor(change_year))) +
  geom_bar()

# distribution across causes
refSamplersRaw %>%
  filter(verification == 'yes' & change_type != '...') %>%
  count(change_type)  %>%
  mutate(perc = n/sum(n)*100)%>%
  ggplot(aes(x=reorder(change_type, perc), y=perc)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(y='Percentage (%)',
       x = 'Agent of change') +
  theme_bw() +
  theme(legend.position = c(0.7, 0.2),
        legend.background = element_blank())


#### Test unique month sets grid versus all year -----------------------------------------

# Create monthsets vector
monthSets <- c('all', 'grid')

# Create a vector of thresholds to loop through
thresholds <- c(4,5,6,7,8,9)


ms <- 'grid'
t <- 5


samplesSel <- sampleMetadata %>%
  pull(REFID)
monthSetThreshAcc <- tibble()

for (ms in monthSets){
  
  
  refDwSel <- refdw %>%
    filter(year >= 2018 & year <= 2023)  %>%
    left_join(sampleMetadata, by = 'REFID') %>%
    filter(REFID %in% samplesSel)
  
  if (ms == 'grid'){
    refDwSel <- refDwSel %>%
      filter(start > end) %>%
      filter(month >= start | month <= end) %>%
      bind_rows(refDwSel %>%
                  filter(start < end) %>%
                  filter(month >= start & month <= end) )
    
  } 
  
  trendsSub <- refDwSel %>%
    group_by(REFID)  %>%
    nest() %>% 
    mutate(model = map(data, ~lm(built ~ year, data = .x))) %>%
    mutate(tidy = map(model, tidy),
           slope = tidy %>% map_dbl(~ filter(.x, term == "year") %>% pull(estimate))*100)%>%
    dplyr::select(REFID, slope)
  
  for (t in thresholds){
    print(paste0('Doing iteration with threshold ',t, ' and month ', ms))
    
    outSub <- trendsSub %>%
      mutate(mapped = ifelse(slope > t, 'yes', 'no')) %>%
      left_join(refSamplers, by = 'REFID') %>%
      drop_na() %>%
      mutate_at(vars(mapped, reference), factor)
    #outSub %>% ggplot(aes(x=mapped)) + geom_bar()
    
    cm <- confusionMatrix(outSub$mapped, outSub$reference)
    
    acc <- as.data.frame(cm$byClass) %>%
      mutate(val = cm$byClass,
             metric = rownames(as.data.frame(cm$byClass))) %>%
      dplyr::select(val, metric) %>%
      bind_rows(tibble(val = cm$overall[[1]], metric = 'OA')) %>%
      mutate(monthSet = ms, 
             threshold = t)
    
    monthSetThreshAcc <- monthSetThreshAcc %>%
      bind_rows(acc)
    
  }
    

  
}

monthSetThreshAcc


# Make plots of output
p1 <- monthSetThreshAcc  %>%
  mutate(monthSet = ifelse(monthSet == 'all', 'all year', 'grid-specific')) %>%
  filter(metric %in% c('F1')) %>% 
  group_by(monthSet, metric) %>%
  summarise(val = mean(val)) %>%
  ggplot(aes(x=monthSet, y = val)) +
  geom_point() +
  labs(x = 'Grid-specific or all-year built-up \nprobability scores',
       y = 'F1 score',
       title='B)') +
  theme()
p1
p2 <- monthSetThreshAcc  %>%
  filter(metric %in% c('F1')) %>% 
  group_by(threshold, metric) %>%
  summarise(val = mean(val)) %>%
  ggplot(aes(x=threshold, y = val)) +
  geom_point()  +
  labs(x = 'Threshold for land take detection \nfrom linear trend',
       y = 'F1 score',
       title='C)') +
  theme()

pMap <- gridMonths %>%
  st_transform(st_crs(countries_sf)) %>%
  st_filter(countries_sf) %>%
  ggplot(aes(fill=monthSet)) +
  geom_sf(data = countries_sf, inherit.aes = F) +
  geom_sf(alpha=0.5)  +
  labs(fill='Month set',
       title='A)') +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank())
pMap
pilotAccPlot <- grid.arrange( p1, p2, ncol=2,
                                    padding = unit(0, "line"),widths=c(1,1.2), newpage = T)

pilotAccPlotOut <- grid.arrange( pMap, pilotAccPlot, ncol=1,
                                 padding = unit(0, "line"),heights=c(2,1), newpage = T)

ggsave("./output/pilotAccPlot.png", pilotAccPlotOut, width = 22, height=22, units='cm')



