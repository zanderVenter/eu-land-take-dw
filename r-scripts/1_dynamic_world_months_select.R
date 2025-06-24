library(tidyverse)
library(sf)
library(ggspatial)
library(RANN)


# Read in DW timeseries
# Note that the multiple CSV files exported from GEE were merged outside of this script
timeseries <- read_csv('./data/From_GEE/dw_urban_timeseries.csv')%>%
  mutate(year = year(date))
length(unique(timeseries$CellCode))

# Check to see how many points per cell
timeseries %>%
  group_by(CellCode, PLOTID) %>%
  summarise() %>%
  group_by(CellCode) %>%
  count() %>%
  ggplot(aes(x=n)) +
  geom_histogram()

# Check the time series for one location
timeseries %>%
  filter(PLOTID == 'a27-8847544_41-4273565') %>%
  ggplot(aes(x=date, y=first)) +
  geom_point()

timeseries %>%
  filter(CellCode == '23417801') %>%
  ggplot(aes(x=month, y=first)) +
  geom_point(alpha=0.5) +
  stat_summary(geom='point', fun=median, color='red', size=2) +
  stat_summary(geom='point', fun=mean, color='green', size=2)

# sanity check years and months
timeseries %>%
  group_by(year) %>%
  summarise(n=n())
timeseries %>%
  group_by(month, year) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=month, y=n,fill=factor(year))) +
  geom_bar(stat='identity')
timeseries %>%
  group_by(month, year) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=year, y=n,fill=factor(month))) +
  geom_bar(stat='identity')

# Get a tibble of month combos - need 3 consecutive months which can overlap to next year
monthCombos <- tibble()
for (i in seq(1,12)){
  start = i
  end = i+3
  end = ifelse(end > 12, end-12, end)
  monthCombos <- monthCombos %>%
    bind_rows(tibble(start=start, end=end, monthSet = paste0('start_',start, '_end_', end)))
}
monthCombos

# get median built probability scores for each month set
tsOut <- tibble()
x <- 'start_11_end_1'
for (x in monthCombos$monthSet){
  s <- monthCombos[monthCombos$monthSet == x, ]$start
  e <- monthCombos[monthCombos$monthSet == x, ]$end
  
  if (s < e){
    tsSub <- timeseries  %>%
      filter(month >= s & month <= e)
  } else {
    tsSub <- timeseries  %>%
      filter(month >= s | month <= e) 
  }
  #unique(tsSub$month)
  out <- tsSub %>%
    group_by(CellCode) %>%
    summarise(prob = mean(first)) %>%
    mutate(monthSet = x,
           start = s,
           end = e)
  
  tsOut <- tsOut %>%
    bind_rows(out)
  
}

tsOut

selectedMonths <- tsOut %>%
  arrange(CellCode) %>%
  group_by(CellCode) %>%
  mutate(max = max(prob)) %>%
  filter(max == prob) %>%
  distinct(CellCode, prob, monthSet, start, end)

# Get export grid joined to months tibble
grid <- st_read('./data/From_GEE/export_grid.geojson') %>%
  mutate(CellCode = as.character(CellCode)) %>%
  left_join(selectedMonths, by = 'CellCode')

grid %>%
  filter(!is.na(prob)) %>%
  ggplot(aes(fill=monthSet)) +
  #annotation_map_tile(type = "osm") +
  geom_sf() 

# Interpolate NA values with nearest neighbor monthSet
grid_with <- grid %>% filter(!is.na(monthSet))
grid_without <- grid %>% filter(is.na(monthSet))

# Extract coordinates for nearest neighbor search
coords_with <- st_coordinates( st_centroid(grid_with))
coords_without <- st_coordinates(st_centroid(grid_without))

# Find the nearest neighbor for each `NA` cell
nn_indices <- nn2(data = coords_with, query = coords_without, k = 1)$nn.idx

# Assign the nearest neighbor's `monthSet` value to the `NA` cells
grid_without <- grid_without %>%
  mutate(monthSet = grid_with$monthSet[nn_indices],
         start = grid_with$start[nn_indices],
         end = grid_with$end[nn_indices])

# Combine the data back into a single dataset
grid_interpolated <- bind_rows(grid_with, grid_without)
colSums(is.na(grid_interpolated))
nrow(grid_interpolated)

grid_interpolated %>%
  ggplot(aes(fill=monthSet))+
  #annotation_map_tile(type = "osm") +
  geom_sf(alpha=0.4) +
  geom_sf(data = grid_with)

# Write out for upload back to GEE
grid_interpolated %>%
  dplyr::select(CellCode, monthSet, start, end) %>%
  mutate(geo = st_as_text(geometry)) %>%
  as_tibble() %>%
  dplyr::select(-geometry) %>%
  st_write('./data/For_GEE/export_grid_with_monthsets.csv')

