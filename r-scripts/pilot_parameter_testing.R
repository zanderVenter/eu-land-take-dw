# Check threshold values - optimal is 5
# Check months in year - optimal is all year
# check inclusion of bare probability scores - optimal is built

#### Data import and cleaning -----------------------------------------------------------
library(tidyverse)
library(broom)
library(caret)
library(sf)

biomeLookup <- tibble(BIOME_NAME = c("N/A",
                                     "Mangroves",
                                     "Temperate Conifer Forests",
                                     "Deserts & Xeric Shrublands",
                                     "Flooded Grasslands & Savannas",
                                     "Montane Grasslands & Shrublands",
                                     "Temperate Broadleaf & Mixed Forests",
                                     "Mediterranean Forests, Woodlands & Scrub",
                                     "Tropical & Subtropical Coniferous Forests",
                                     "Temperate Grasslands, Savannas & Shrublands",
                                     "Tropical & Subtropical Dry Broadleaf Forests",
                                     "Tropical & Subtropical Moist Broadleaf Forests",
                                     "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                                     "Boreal Forests/Taiga",
                                     "Tundra"),
                      biome_num = c("0",
                                    "1",
                                    "2",
                                    "3",
                                    "4",
                                    "5",
                                    "6",
                                    "7",
                                    "8",
                                    "9",
                                    "10",
                                    "11",
                                    "12",
                                    "13",
                                    "14"))

sampleMetadata <- read_csv('./data/From_GEE/biomes_pilot_samples.csv') %>% 
  st_as_sf(coords=c('LONGITUDE','LATITUDE')) %>%
  mutate(biome = factor(first)) %>%
  dplyr::select(REFID = PLOTID, biome)
levels(sampleMetadata$biome) <- biomeLookup[biomeLookup$biome_num %in% levels(sampleMetadata$biome), ]$BIOME_NAME

sampleMetadata <-  sampleMetadata %>%
  mutate(biomeRecoded = recode_factor(biome, "Mediterranean Forests, Woodlands & Scrub" = 'Mediterranean',
                                      "Temperate Broadleaf & Mixed Forests" = 'Temperate',
                                      "Temperate Conifer Forests" = 'Boreal',
                                      "Temperate Grasslands, Savannas & Shrublands" = 'Temperate',
                                      "Boreal Forests/Taiga" = 'Boreal',
                                      "Tundra"  = 'Boreal')) 
sampleMetadata %>%
  ggplot()+
  geom_sf(aes(color=biome))
sampleMetadata %>%
  ggplot()+
  geom_sf(aes(color=biomeRecoded))

# Import the verification points from sampling app
refSamplers <- read_csv('./data/From_samplers/public_arena_pilot_collection - Sheet1.csv') %>%
  dplyr::select(REFID, reference = verification) %>%
  filter(reference %in% c('yes', 'no'))

# Import the reference points with Dynamic World time series

# refdw <- read_csv('./data/From_GEE/pilot_dw_timeseries.csv') %>%
#   dplyr::select(REFID = PLOTID, date, built, bare) %>%
#   mutate(month = month(date), year = year(date))

refdw <- read_csv('./data/From_GEE/pilot_dw_timeseries.csv')%>%
  mutate(month = month(date), year = year(date))
refdw

# sanity check years and months
refdw %>%
  group_by(year) %>%
  summarise(n=n())
refdw %>%
  group_by(month, year) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=month, y=n,fill=factor(year))) +
  geom_bar(stat='identity')
refdw %>%
  group_by(month, year) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=year, y=n,fill=factor(month))) +
  geom_bar(stat='identity')

#### Test thresholds x months x biome combinations -----------------------------------------------------------

# First have a look to see when the "built" probability scores are highest in the year
refdw %>%
  mutate(month = month(date, label=TRUE)) %>%
  group_by(month) %>%
  summarise(built = mean(built)) %>%
  ggplot(aes(x=month, y=built)) +
  geom_bar(stat='identity')

# Create a dataframe of month intervals to test
monthDF <- tibble(name = c('all_year', 'winter', 'summer', 'spring', 'autumn'),
                  startM = c(1, 11, 5, 3, 9),
                  endM = c(12, 2, 8, 4, 11))
monthDF

# Create a vector of thresholds to loop through
thresholds <- c(4,5,6,7,8,9)

# Create a vector of biomes to loop through
biomes <- c('Temperate', 'Mediterranean', 'Boreal', 'All')

m <- 'winter'
t <- 4
b <- 'Temperate'

monthThreshAcc <-tibble()

for (b in biomes){
  
  samplesSel <- sampleMetadata %>%
    filter(biomeRecoded == b) %>%
    pull(REFID)
  if (b == 'All'){
    samplesSel <- sampleMetadata$REFID
  }
  
  refDwSel <- refdw %>%
    filter(REFID %in% samplesSel)
  
  for (m in monthDF$name){
    
    monthsSel <- monthDF %>% filter(name == m)
    if (m == 'winter'){
      trendsSub <- refDwSel %>%
        filter(month >= monthsSel$startM | month <= monthsSel$endM) 
    } else {
      trendsSub <- refDwSel %>%
        filter(month >= monthsSel$startM & month <= monthsSel$endM)
    }
    trendsSub <- trendsSub %>%
      #group_by(year, REFID) %>%
      #summarise(built = median(built)) %>%
      group_by(REFID)  %>%
      nest() %>% 
      mutate(model = map(data, ~lm(built ~ year, data = .x))) %>%
      mutate(tidy = map(model, tidy),
             slope = tidy %>% map_dbl(~ filter(.x, term == "year") %>% pull(estimate))*100)%>%
      dplyr::select(REFID, slope)
    
    for (t in thresholds){
      print(paste0('Doing iteration with threshold ',t, ' and month ', m, ' and biome', b))
      
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
        mutate(months = m,
               threshold = t,
               biome = b)
      
      monthThreshAcc <- monthThreshAcc %>%
        bind_rows(acc)
      
    }
  }
}



# precision: Of the instances predicted to be positive, how many are actually positive? 
  #- the higher the more conservative the predictions - ie. less areas mapped as loss than there actually are
# recall: Of the actual positive instances, how many were correctly identified?
  #- the higher the more generous the predictions - i.e more areas mapped as loss than there actually are

monthThreshAcc %>%
  filter(metric == 'Balanced Accuracy') %>%
  ggplot(aes(x=months, y=val)) +
  geom_point() +
  facet_grid(biome~threshold)

monthThreshAcc %>%
  mutate(combo = paste0(months, '_', threshold)) %>%
  filter(metric %in% c('Balanced Accuracy')) %>%
  ggplot(aes(x=val, y=reorder(combo, val), color=metric)) +
  geom_point()  +
  facet_wrap(~biome)

monthThreshAcc %>%
  mutate(combo = paste0(months, '_', threshold)) %>%
  filter(metric %in% c('Recall', 'Precision')) %>%
  ggplot(aes(x=val, y=reorder(combo, val), color=metric)) +
  geom_point() +
  facet_wrap(~biome)



#### Test thresholds x built/bare combinations -----------------------------------------------------------

# Create vector of built or bare or combo
classes <- c('built', 'bare', 'both')

c <- 'built'
t <- 5

classThreshAcc <-tibble()


for (c in classes){
  
  if (c == 'both'){
    refDwSel <- refdw %>%
      mutate(classSel = built + bare)
  } else {
    refDwSel <- refdw %>%
      mutate(classSel = .[[c]])
  }
  
  trendsSub <- refDwSel %>%
    group_by(REFID)  %>%
    nest() %>% 
    mutate(model = map(data, ~lm(classSel ~ year, data = .x))) %>%
    mutate(tidy = map(model, tidy),
           slope = tidy %>% map_dbl(~ filter(.x, term == "year") %>% pull(estimate))*100)%>%
    dplyr::select(REFID, slope)
  
  for (t in thresholds){
    print(paste0('Doing iteration with threshold ',t, ' and class ', c))
    
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
      mutate(threshold = t,
             classes = c)
    
    classThreshAcc <- classThreshAcc %>%
      bind_rows(acc)
    
  }
}

classThreshAcc %>%
  mutate(combo = paste0(classes, '_', threshold)) %>%
  filter(metric %in% c('Balanced Accuracy')) %>%
  ggplot(aes(x=val, y=reorder(combo, val), color=metric)) +
  geom_point()  

classThreshAcc %>%
  mutate(combo = paste0(classes, '_', threshold)) %>%
  filter(metric %in% c('Recall', 'Precision')) %>%
  ggplot(aes(x=val, y=reorder(combo, val), color=metric)) +
  geom_point()


