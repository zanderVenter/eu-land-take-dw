library(sf)
library(tidyverse)


#### Import country specific areas -----------------------

# Define expected accuracies and target standard error around accuracy estimates
expectedAccuracies <- tibble(
  stratum = c( 'nature stable','crop/built stable', 'buffer nature stable', 'buffer crop/built stable','nature loss','cropland loss'), 
  accuracy = c(0.95, 0.95, 0.6,0.6, 0.7, 0.7))
targetSE <- 0.02
minSampleSize <- 40 # minimum number of points desired per stratum

# Areas per strata from GEE
areas <- read_csv('./data/From_GEE/sample_areas_countries_v3.csv')%>%
  dplyr::select(country, CellCode, area, stratum)%>%
  mutate(stratum = ifelse(stratum == 1, 'nature stable',
                          ifelse(stratum == 2, 'crop/built stable',
                                 ifelse(stratum == 3, 'buffer nature stable',
                                        ifelse(stratum == 4, 'buffer crop/built stable',
                                               ifelse(stratum == 5, 'nature loss', 
                                                      ifelse(stratum == 6, 'cropland loss', NA)))))))
length(unique(areas$country))
unique(areas$country)

# Check total areas
areas %>%
  mutate(tot = sum(area)) %>%
  group_by(stratum, tot) %>%
  summarise(area = sum(area)) %>%
  mutate(perc = area/tot*100)


#### Calculate recommended sample sizes per country -----------------------

# Calculate sample sizes per country
# determined using the stratified variance estimator solved for n as described in Cochran (1977) 
# with a target standard error for overall accuracy of 0.02
sampleSizes <- areas %>%
  group_by(country, stratum) %>%
  summarise(area = sum(area)) %>%
  group_by(country) %>%
  mutate(areaTot = sum(area)) %>%
  ungroup() %>%
  mutate(weight = area/areaTot,
         areaTotkm = areaTot/1000000) %>%
  left_join(expectedAccuracies) %>%
  mutate(numerators = weight*(sqrt(accuracy*(1-accuracy))))%>%
  group_by(country) %>%
  mutate(numeratorSum = sum(numerators)) %>%
  ungroup() %>%
  mutate(sampleSize = round((numeratorSum/targetSE)^2),
         propAllocation = round(sampleSize*weight))%>%
  mutate(propAllocation = ifelse(propAllocation < minSampleSize, minSampleSize, propAllocation))%>%
  dplyr::select(country, stratum, propAllocation) 

# Allocation per stratum over EU
sampleSizes %>%
  group_by(stratum) %>%
  summarise(alloc = sum(propAllocation))

# Allocation by country
sampleSizes %>%
  group_by(country) %>%
  mutate(sum = sum(propAllocation))

sampleSizes %>%
  ungroup() %>%
  summarise(sum = sum(propAllocation))

# The samples were stratified and generated in GEE where the reference data collection tool place
# We filter out small countries with small sample numbers picked up in the country == EU sample 
# Lichetenstein, Luxembourg, Cyprus and Malta 
samplesFinal <- read_csv('./data/For_GEE/samples_stratified.csv')
unique(samplesFinal$country)

# The sample sizes differ slightly to the allocation because we added additional samples for some 
# strata and countries where we expected even lower accuracies

sampleFinalCounts <- samplesFinal %>%
  group_by(country, stratum) %>%
  count()  %>%
  mutate(stratum = ifelse(stratum == 'buffer crop/built stable', 'land take cropland buffer',
                          ifelse(stratum == 'buffer nature stable', 'land take nature buffer',
                                 ifelse(stratum == 'crop/built stable', 'stable artificial/cropland', 
                                        ifelse(stratum == 'cropland loss', 'land take cropland',
                                               ifelse(stratum == 'nature loss', 'land take nature',
                                                      ifelse(stratum == 'nature stable', 'stable nature'))))))) 

sampleFinalCounts$stratum <- factor(sampleFinalCounts$stratum, levels = c(
  'stable nature',
  'stable artificial/cropland',
  'land take nature',
  'land take cropland',
  'land take nature buffer',
  'land take cropland buffer'
))
sampleFinalCounts %>% arrange(country, stratum)

sampleFinalCounts  %>% 
  arrange(country, stratum) %>%
  pivot_wider(names_from = stratum, values_from=n) %>%
  write_csv('./output/sample_allocation_final.csv')

sampleFinalCounts %>%
  group_by(stratum) %>%
  summarise(n = sum(n))

# Make a figure of points verified per country
pointsPerCountryPlot <- samplesFinal %>%
  group_by(country) %>%
  count()  %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_point() +
  geom_text(aes(label = n, y= n+10)) +
  coord_flip() +
  labs(x='',
       y='Number verification pts')
pointsPerCountryPlot


