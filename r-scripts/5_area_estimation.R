#### Setup environment and define functions -------------------------------------
library(tidyverse)
library(sf)
library(stringr)
library(gridExtra)
library(grid)
library(ggtext)

# Set ggplot theme
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
            theme(strip.background =element_rect(fill="white")))


# Design-based area estimation function adapted from https://rdrr.io/cran/mapaccuracy/src/R/olofsson.R
# only altered som null checking and added csv export - no change to the statistical formulae
olofsson <-function(r, m, Nh, margins=TRUE, csv_filepath){
  
  # check arguments
  r<-unlist(r)
  m<-unlist(m)
  Nh<-unlist(Nh)
  if(is.null(names(Nh))) stop("Nh must be named.", call. = FALSE)
  if(length(names(Nh))>length(unique(names(Nh)))) stop("Repeated names detected in Nh.", call. = FALSE)
  
  # convert arguments
  r<-factor(r, levels = names(Nh))
  m<-factor(m, levels = names(Nh))
  
  
  # error matrix
  matrix<-table(m, r)
  
  # Sampling settings
  q <-length(Nh)       # number of classes
  A <-sum(Nh)          # total area
  nh<-rowSums(matrix)  # stratum sample size
  Wh<-Nh/A             # map class proportions
  
  # confusion matrix (estimated area proportions)
  props<-prop.table(matrix,1)
  for(j in 1:q){
    props[j,]<-Wh[j]*props[j,]
  }
  
  # Accuracy and area estimates
  OA<-sum(diag(props),na.rm=T)
  UA<-diag(props)/rowSums(props)
  PA<-diag(props)/colSums(props)
  # tarea<-A*colSums(props)
  tarea<-colSums(props)
  
  
  # standard error of OA
  VAR_OA<-0
  for(j in 1:q){
    temp<-Wh[j]^2*UA[j]*(1-UA[j])/(nh[j]-1)
    if(is.na(temp)){temp<-0}
    if(is.infinite(temp)){temp<-0}
    VAR_OA<-VAR_OA + temp
  }
  SEoa<-sqrt(VAR_OA)
  names(SEoa)<-NULL
  
  
  # standard error of UA
  VAR_UA<-NULL
  for(j in 1:q){
    temp<-UA[j]*(1-UA[j])/(nh[j]-1)
    if(is.na(temp)){temp<-0}
    if(is.infinite(temp)){temp<-0}
    VAR_UA<-c(VAR_UA, temp)
  }
  SEua<-sqrt(VAR_UA)
  
  
  # standard error of PA
  N.j<-NULL
  for(j in 1:q){
    temp<-0
    for(i in 1:q){
      temp<-sum(temp,Nh[i]/nh[i]*matrix[i,j],na.rm=T)
    }
    N.j<-c(N.j, temp)
  }
  
  VAR_PA<-NULL
  for(j in 1:q){
    # temp1<-N.j[j]^2*(1-PA[j])^2*UA[j]*(1-UA[j])/(nh[j]-1)
    temp1<-Nh[j]^2*(1-PA[j])^2*UA[j]*(1-UA[j])/(nh[j]-1)
    if(is.na(temp1)){temp1<-0}
    if(is.infinite(temp1)){temp1<-0}
    temp2<-0
    seque<-1:q
    seque<-seque[-j]
    for(i in seque){
      temp2<-sum(temp2, Nh[i]^2*matrix[i,j]/nh[i]*(1-matrix[i,j]/nh[i])/(nh[i]-1), na.rm=T)
    }
    if(is.na(temp2)){temp2<-0}
    if(is.infinite(temp2)){temp2<-0}
    
    VAR_PA<-c(VAR_PA, (1/N.j[j]^2)*(temp1+PA[j]^2*temp2))
  }
  SEpa<-sqrt(VAR_PA)
  
  
  # standard error of the area
  VAR_A<-list()
  for(j in 1:q){
    a<-Wh^2
    b<-matrix[,j]/nh
    c<-(1-matrix[,j]/nh)
    d<-nh-1
    VAR_A[[j]]<-sum(a*(b*c/d))
  }
  VAR_A<-unlist(VAR_A)
  SEa<-sqrt(VAR_A)
  # SEa<-A*SEa
  names(SEa)<-names(Nh)
  
  # gather calculations together
  if(!missing(csv_filepath)){
    # export as a table to csv
  }
  
  # Add margins to confusion matrix (potentially useful for exporting only)
  props[matrix==0]<-NA
  if(margins){
    props<-cbind(props,rowSums(props, na.rm = TRUE))
    props<-rbind(props,c(colSums(props, na.rm = TRUE)))
    colnames(props)[ncol(props)]<-"sum"; rownames(props)[nrow(props)]<-"sum"
  }
  
  # return
  list(OA=OA,
       UA=UA,
       PA=PA,
       area=tarea,
       SEoa=SEoa,
       SEua=SEua,
       SEpa=SEpa,
       SEa =SEa,
       matrix=props)
}


#### Country-specific area estimation ----------------------------------------------------


# Import cleaned reference dataset
samplesRaw <- read_csv('./data/From_samplers/reference_samples_clean.csv')
unique(mappedSamples$country)

# Empty tibble to house area estimates from loop
areasEstOut <- tibble()

# Import map labels for reference samples
mappedSamples <- read_csv('./data/For_GEE/samples_stratified.csv')
unique(mappedSamples$country)

# Now re-label the sample data to match the strata lables from the mappedSamples dataframe
samples <- samplesRaw %>%
  # Need to have full reference data on baseline land cover - so dropping NA values
  drop_na(baseline_land_use) %>%
  # Inherit the mapped sample labels for "buffer" class - this was not explicitly labelled by samplers
  left_join(mappedSamples %>%
              dplyr::select(PLOTID, stratumMapped=stratum), by = 'PLOTID', relationship = "many-to-many")%>%
  # Create strata based on sampler labels for "verification" and "baseline_land_use"
  mutate(stratum = ifelse(verification == 'no' & !baseline_land_use %in% c('cropland', 'built') & !str_detect(stratumMapped, 'buffer'), 'nature stable',
                          ifelse(verification == 'no' & !baseline_land_use %in% c('cropland', 'built') & str_detect(stratumMapped, 'buffer'), 'buffer nature stable',
                                 ifelse(verification == 'no' & baseline_land_use %in% c('cropland', 'built') & !str_detect(stratumMapped, 'buffer'), 'crop/built stable',
                                        ifelse(verification == 'no' & baseline_land_use %in% c('cropland', 'built') & str_detect(stratumMapped, 'buffer'), 'buffer crop/built stable',
                                               ifelse(verification == 'yes' & !baseline_land_use %in% c('cropland', 'built'), 'nature loss',
                                                      ifelse(verification == 'yes' & baseline_land_use %in% c('cropland', 'built'), 'cropland loss', NA )))))))  %>%
  drop_na(stratum)

# Check sample strata
unique(samples$stratum)
samples %>% ggplot(aes(x=baseline_land_use)) + geom_bar() + coord_flip()
samples %>% ggplot(aes(x=stratum)) + geom_bar()+ coord_flip()

# Some samples were labelled more than once separate interpreters - due to chance simultaneous allocation in the app
# Therefore need to get the mode class (majority vote)
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Create a final reference sample dataframe in format digestable by the olofsson() function for area estimation
referenceSamples <- samples %>%
  dplyr::select(PLOTID, r = stratum) %>%
  drop_na(r) %>%
  group_by(PLOTID) %>%
  # Take the most common vote from samples which were sharedbetween samplers
  summarise(r = get_mode(r))
nrow(referenceSamples)

# Import and label mapped areas from GEE - these are the pixel counting areas
areas <- read_csv('./DATA/From_GEE/sample_areas_countries_v3.csv') %>%
  dplyr::select(country, CellCode, area, stratum)%>%
  mutate(stratum = ifelse(stratum == 1, 'nature stable',
                          ifelse(stratum == 2, 'crop/built stable',
                                 ifelse(stratum == 3, 'buffer nature stable',
                                        ifelse(stratum == 4, 'buffer crop/built stable',
                                               ifelse(stratum == 5, 'nature loss', 
                                                      ifelse(stratum == 6, 'cropland loss', NA))))))) %>%
  group_by(country, stratum) %>%
  # Area in km2
  summarise(area = sum(area)/1000000) %>%
  # correct for Greece ISO3 code
  mutate(country = ifelse(country == 'GRE', 'GRC', country))
areas
areas %>%
  group_by(stratum) %>%
  summarise(area = sum(area)) 


# Merge mapped and reference samples
mergedSamples <- mappedSamples %>% as_tibble() %>% 
  dplyr::select(PLOTID, country, m = stratum)  %>%
  left_join(referenceSamples,  by = 'PLOTID') %>%
  ungroup( )%>%
  drop_na(r)


# Loop through countries and calculate design-based areas
c <- 'FIN'
for (c in unique(mappedSamples$country)){
  
  # Do area estimation for land take combined or separate
  for (takeType in c('merged', 'separate')){
    
    if (takeType == 'merged'){
      
      # Get country subset
      combined <- mergedSamples %>%
        filter(country == c)%>%
        mutate(r = ifelse(str_detect(r, 'loss'), 'land take',r)) %>%
        mutate(m = ifelse(str_detect(m, 'loss'), 'land take',m)) %>%
        ungroup()
      combined %>% ggplot(aes(x=r, fill=m)) + geom_bar()
      
      # Get areas in a format ready for the design-based estimation function
      Nh<-areas %>%
        filter(country == c) %>%
        mutate(stratum = ifelse(str_detect(stratum, 'loss'), 'land take',stratum))  %>%
        group_by(country, stratum) %>%
        summarise(area = sum(area)) %>%
        ungroup() %>%
        dplyr::select(-country) %>%
        deframe()
      
    } else {
      
      # Get country subset
      combined <- mergedSamples %>%
        filter(country == c)%>%
        drop_na(r)  %>%
        ungroup()
      combined %>% ggplot(aes(x=r, fill=m)) + geom_bar()
      
      # Get areas in a format ready for the design-based estimation function
      Nh<-areas %>%
        filter(country == c) %>%
        ungroup() %>%
        dplyr::select(-country) %>%
        deframe()
    }
  
    # For design-based estimation, we subsume the buffer stratum into stable for reference pts only
    combinedForEstimation <- combined %>%
      mutate(r= ifelse(r == 'buffer nature stable', 'nature stable', 
                       ifelse(r == 'buffer crop/built stable', 'crop/built stable', r)))
    
    # Run stratified area estimator
    e<-olofsson(combinedForEstimation$r, combinedForEstimation$m, Nh)
    
    # Unpack outputs into a tibble
    changeDF <- tibble()
    for (i in seq(1, length(e$area))) {
      ua_i <- e$UA[i]
      pa_i <- e$PA[i]
      
      subChangeDF <- tibble(
        stratum = names(e$area)[i],
        area = e$area[i] * sum(Nh),
        prop = e$area[i],
        se = e$SEa[i] * sum(Nh),
        pa = pa_i,
        ua = ua_i,
        seUa = e$SEua[i],
        sePa = e$SEpa[i],
        oa = e$OA,
        ciArea = qnorm(0.975) * e$SEa[i] * sum(Nh),
        f1 = ifelse((ua_i + pa_i) > 0, 2 * ua_i * pa_i / (ua_i + pa_i), NA_real_)
      )
      changeDF <- changeDF %>% bind_rows(subChangeDF)
    }
    changeDF$Nh <- Nh
    changeDF$takeType  <- takeType
    changeDF$country  <- c
    
    # Combine with output tibble
    areasEstOut <- areasEstOut %>%
      bind_rows(changeDF)
    
    
  }
  
  
}

areasEstOut

# Get pixel counting areas which are not separated out into buffer zones
# Need to subsume into the stable class
NhTots <- areasEstOut %>%
  mutate(stratum = str_remove(stratum, 'buffer ')) %>%
  group_by(country, stratum, takeType) %>%
  summarise(area_mapped = sum(Nh))

# Join back with output
areasEstOut <- areasEstOut %>%
  filter(!str_detect(stratum, 'buffer')) %>%
  dplyr::select(-Nh) %>%
  left_join(NhTots)

# Write out for later
areasEstOut %>%
  write_csv('./data/area_estimates_countries.csv')

# Write out pixel counting areas for later as well
areas %>%
  write_csv('./data/area_mapped_countries.csv')

# Write out merged mapped and reference labels for later
mergedSamples %>%
  write_csv('./data/samples_labelled_countries.csv')

# Write out UA and PA for table

areasEstOut %>%
  filter(stratum %in% c('land take', 'nature loss', 'cropland loss')) %>%
  dplyr::select(country, stratum, ua, seUa, pa, sePa) %>%
  mutate_at(vars(ua:sePa), round, 2) %>%
  mutate(UA = paste0(ua, ' (',seUa,')'))%>%
  mutate(PA = paste0(pa, ' (',sePa,')')) %>%
  dplyr::select(country, stratum, UA, PA) %>%
  pivot_wider(values_from=c('UA', 'PA'), names_from=stratum) %>%
  arrange(country) %>%
  write_csv('./output/ua_pa_countries.csv')

#### EU-level area estimation - extrapolate with stratified estimator ----------------------------------
# It is important to note here that I will be making area estimation for the EEA-39 here to make
# it comparable with EEA estimates here:
# https://www.eea.europa.eu/en/analysis/maps-and-charts/land-take-statistics-dashboards?activeTab=265e2bee-7de3-46e8-b6ee-76005f3f434f
# This assumes that the reference data we collected is representative of EEA-39 as a whole
# given that we sampled 29 countries, it is a reasonable assumption to make

# We could derive area estimates only for the 29 countries (excludes UKR) in our analysis if we wanted to

# Bring in the pixel counting areas
area_mapped_countries <- read_csv('./data/area_mapped_countries.csv')
sort(unique(area_mapped_countries$country))

# Bring in the reference and mapped labels for samples
samples_labelled_countries <- read_csv('./data/samples_labelled_countries.csv')
sort(unique(samples_labelled_countries$country))

# Dataframe to house outputs of loop
EUareasEstOut <- tibble()

# Going to make estimates for land take combined and separated by nature and cropland take
for (takeType in c('merged', 'separate')){
  
  
  if (takeType == 'merged'){
    
    areasEU <- area_mapped_countries %>%
      mutate(stratum = ifelse(str_detect(stratum, 'loss'), 'land take',stratum)) %>%
      group_by(stratum) %>%
      summarise(area = sum(area))
    sum(areasEU$area)
    
    combinedEU <- samples_labelled_countries %>%
      mutate(r = ifelse(str_detect(r, 'loss'), 'land take',r)) %>%
      mutate(m = ifelse(str_detect(m, 'loss'), 'land take',m))%>%
      ungroup()
    
  } else {
    
    areasEU <- area_mapped_countries %>%
      filter(country != 'UKR') %>%
      group_by(stratum) %>%
      summarise(area = sum(area))
    sum(areasEU$area)
    
    combinedEU <- samples_labelled_countries %>%
      ungroup()
    
  }
  
  Nh<-areasEU %>%
    deframe()
  
  combinedEUForEstimation <- combinedEU %>%
    mutate(r= ifelse(r == 'buffer nature stable', 'nature stable', 
                     ifelse(r == 'buffer crop/built stable', 'crop/built stable', r))) 
  
  e<-olofsson(combinedEUForEstimation$r, combinedEUForEstimation$m, Nh)
  
  changeDF <- tibble()
  for (i in seq(1, length(e$area))) {
    ua_i <- e$UA[i]
    pa_i <- e$PA[i]
    e$SEoa * qnorm(0.975)
    subChangeDF <- tibble(
      stratum = names(e$area)[i],
      area = e$area[i] * sum(Nh),
      prop = e$area[i],
      se = e$SEa[i] * sum(Nh),
      pa = pa_i,
      ua = ua_i,
      seUa = e$SEua[i],
      sePa = e$SEpa[i],
      oa = e$OA,
      ciArea = qnorm(0.975) * e$SEa[i] * sum(Nh),
      f1 = ifelse((ua_i + pa_i) > 0, 2 * ua_i * pa_i / (ua_i + pa_i), NA_real_)
    )
    changeDF <- changeDF %>% bind_rows(subChangeDF)
  }
  changeDF$Nh <- Nh
  changeDF$takeType <- takeType
  
  EUareasEstOut <- EUareasEstOut %>%
    bind_rows(changeDF)
  
}

View(EUareasEstOut  %>%
       filter(takeType == 'separate')%>%
       dplyr::select(stratum, Nh))

EUareasEstOut %>%
  filter(!str_detect(stratum, 'buffer'))
# Get pixel counting areas which are not separated out into buffer zones
# Need to subsume into the stable class
NhTotsEU <- EUareasEstOut %>%
  mutate(stratum = str_remove(stratum, 'buffer ')) %>%
  group_by(stratum, takeType) %>%
  summarise(area_mapped = sum(Nh))

# Join back with output
EUareasEstOut <- EUareasEstOut %>%
  filter(!str_detect(stratum, 'buffer')) %>%
  dplyr::select(-Nh) %>%
  left_join(NhTotsEU)

EUareasEstOut %>%
  # Convert to annual rate
  mutate_at(vars(area, ciArea, area_mapped), function(x) x/6)



#### EU-level area estimation 29 countries - extrapolate with percentage land area ------------------------------

# Now we wnat to get design-based estimates for the 29 countries in our sample
area_mapped_countries_29 <- area_mapped_countries %>%
  filter(country %in% unique(samples_labelled_countries$country))
unique(area_mapped_countries_29$country)

# Dataframe to house outputs of loop
EUareasEstOut_29 <- tibble()

# Going to make estimates for land take combined and separated by nature and cropland take
for (takeType in c('merged', 'separate')){
  
  
  if (takeType == 'merged'){
    
    areasEU <- area_mapped_countries_29 %>%
      mutate(stratum = ifelse(str_detect(stratum, 'loss'), 'land take',stratum)) %>%
      group_by(stratum) %>%
      summarise(area = sum(area))
    sum(areasEU$area)
    
    combinedEU <- samples_labelled_countries %>%
      mutate(r = ifelse(str_detect(r, 'loss'), 'land take',r)) %>%
      mutate(m = ifelse(str_detect(m, 'loss'), 'land take',m))%>%
      ungroup()
    
  } else {
    
    areasEU <- area_mapped_countries_29 %>%
      filter(country != 'UKR') %>%
      group_by(stratum) %>%
      summarise(area = sum(area))
    sum(areasEU$area)
    
    combinedEU <- samples_labelled_countries %>%
      filter(country != 'UKR')%>%
      ungroup()
    
  }
  
  Nh<-areasEU %>%
    deframe()
  
  combinedEUForEstimation <- combinedEU %>%
    mutate(r= ifelse(r == 'buffer nature stable', 'nature stable', 
                     ifelse(r == 'buffer crop/built stable', 'crop/built stable', r))) 
  
  e<-olofsson(combinedEUForEstimation$r, combinedEUForEstimation$m, Nh)
  
  changeDF <- tibble()
  for (i in seq(1, length(e$area))) {
    ua_i <- e$UA[i]
    pa_i <- e$PA[i]
    
    subChangeDF <- tibble(
      stratum = names(e$area)[i],
      area = e$area[i] * sum(Nh),
      prop = e$area[i],
      se = e$SEa[i] * sum(Nh),
      pa = pa_i,
      ua = ua_i,
      seUa = e$SEua[i],
      sePa = e$SEpa[i],
      oa = e$OA,
      ciArea = qnorm(0.975) * e$SEa[i] * sum(Nh),
      f1 = ifelse((ua_i + pa_i) > 0, 2 * ua_i * pa_i / (ua_i + pa_i), NA_real_)
    )
    changeDF <- changeDF %>% bind_rows(subChangeDF)
  }
  changeDF$Nh <- Nh
  changeDF$takeType <- takeType
  
  EUareasEstOut_29 <- EUareasEstOut_29 %>%
    bind_rows(changeDF)
  
}

# Get pixel counting areas which are not separated out into buffer zones
# Need to subsume into the stable class
NhTotsEU_29 <- EUareasEstOut_29 %>%
  mutate(stratum = str_remove(stratum, 'buffer ')) %>%
  group_by(stratum, takeType) %>%
  summarise(area_mapped = sum(Nh))

# Join back with output
EUareasEstOut_29 <- EUareasEstOut_29 %>%
  filter(!str_detect(stratum, 'buffer')) %>%
  dplyr::select(-Nh) %>%
  left_join(NhTotsEU_29)


# Now we calcualte total land area for all countries inside sample and total EU
# All pixels from GEE were masked out water - so they sum to total land area
totalLandAreaEU <- area_mapped_countries  %>%
  # create subgroup showing area those included in our sample
  mutate(inSample = ifelse(country %in% unique(samples_labelled_countries$country), 'sample', 'outside')) %>%
  group_by(inSample) %>%
  summarise(area = sum(area)) %>%
  # add a total area column and percentage of that
  ungroup() %>%
  mutate(areaTot = sum(area),
         areaPerc = area/areaTot)
totalLandAreaEU

coverage_fraction <- 0.963

EUareasEstOut_Perc <- EUareasEstOut_29 %>%
  mutate(area_39 = area/coverage_fraction,
         ciArea_39 = ciArea/coverage_fraction)
EUareasEstOut_Perc
EUareasEstOut

EUareasEstOut_Perc %>%
  dplyr::select(stratum, area_39, ciArea_39, area_mapped) %>%
  # Convert to annual rate
  mutate_at(vars(area_39, ciArea_39, area_mapped), function(x) x/6)



#### Area use efficiency ---------------------------------------------------------------
# Here I will compute are use efficiency indicators and land take indicators
# I am using the land take statistics from Arena LUCAS definition
areaEstCountry <- read_csv('./data/area_estimates_countries.csv') %>%
  # Convert to annual rate
  mutate_at(vars(area, ciArea, area_mapped), function(x) x/6) %>%
  mutate(unbiased_area = area)

# Population data
# https://population.un.org/wpp/Download/Standard/Population/
pop <- read_csv('./data/WPP2024_PopulationByAge5GroupSex_Medium.csv') %>%
  mutate(country = ISO3_code) %>%
  group_by(country, Time) %>%
  summarise(PopTotal = sum(PopTotal)) 
population2023 <- pop %>%
  filter(Time == 2023)

pop %>%
  filter(country == 'NOR') %>%
  filter(Time < 2023) %>%
  ggplot(aes(x = Time, y = PopTotal)) +
  geom_point()

lueDFlandtake <- areaEstCountry %>%
  filter(takeType == 'merged') %>%
  filter(stratum %in% c('land take')) %>%
  dplyr::select(country, areaEst = unbiased_area, ciArea) %>%
  left_join(population2023, by = 'country') %>%
  mutate(lue = (areaEst*1000000) / (PopTotal*1000),
         lueCI = (ciArea*1000000) / (PopTotal*1000)) 

lueLandTakePlot <- lueDFlandtake %>%
  ggplot(aes(x = reorder(country, lue), y = lue)) +
  geom_point(size=3, alpha=0.6) +
  geom_errorbar(aes(ymin=lue-lueCI, ymax=lue+lueCI), width=0) +
  coord_flip() +
  labs(x = '',
       title = 'A) Land take total',
       y = expression('Land take ('~m^-2~citizen^-1~yr^-1~')'))
lueLandTakePlot


lueDFnature <- areaEstCountry%>%
  filter(takeType == 'separate')%>%
  filter(stratum == 'nature loss') %>%
  dplyr::select(country, areaEst = unbiased_area, ciArea) %>%
  left_join(population2023, by = 'country') %>%
  mutate(lue = (areaEst*1000000) / (PopTotal*1000),
         lueCI = (ciArea*1000000) / (PopTotal*1000)) 

luefNaturePlot <- lueDFnature %>%
  ggplot(aes(x = reorder(country, lue), y = lue)) +
  geom_point(size=3, alpha=0.6, color='#bb63b1') +
  geom_errorbar(aes(ymin=lue-lueCI, ymax=lue+lueCI), width=0, color='#bb63b1') +
  coord_flip() +
  labs(x = '',
       title = 'B) Land take nature',
       y = expression('Nature take ('~m^-2~citizen^-1~yr^-1~')'))
luefNaturePlot

lueDFcropland <- areaEstCountry%>%
  filter(takeType == 'separate')%>%
  filter(stratum == 'cropland loss') %>%
  dplyr::select(country, areaEst = unbiased_area, ciArea) %>%
  left_join(population2023, by = 'country') %>%
  mutate(lue = (areaEst*1000000) / (PopTotal*1000),
         lueCI = (ciArea*1000000) / (PopTotal*1000)) 


print(lueDFlandtake %>%
  dplyr::select(country, ltOverall = lue, ltOverallCI = lueCI) %>%
  left_join(lueDFnature %>%
              dplyr::select(country, ltNature = lue, ltNatureCI = lueCI))%>%
  left_join(lueDFcropland %>%
              dplyr::select(country, ltCropland = lue, ltCroplandCI = lueCI)), n = 40)

luefCroplandPlot <- lueDFcropland %>%
  ggplot(aes(x = reorder(country, lue), y = lue))+
  geom_point(size=3, alpha=0.6, color='#63c8c9') +
  geom_errorbar(aes(ymin=lue-lueCI, ymax=lue+lueCI), width=0, color='#63c8c9')  +
  coord_flip() +
  labs(x = '',
       title = 'C) Land take cropland',
       y = expression('Cropland take ('~m^-2~citizen^-1~yr^-1~')'))
luefCroplandPlot

luefPlot <- grid.arrange( lueLandTakePlot, luefNaturePlot, luefCroplandPlot, ncol=3,
                          padding = unit(0, "line"),widths=c(1,1,1), newpage = T)

ggsave("./output/luefPlot.png", luefPlot, width = 24, height=15, units='cm')


#### Comparison with EEA CLC Accounting Layer ------------------------------------
# Land take from EEA website 2000-2018
# https://www.eea.europa.eu/en/analysis/maps-and-charts/land-take-statistics-dashboards?activeTab=265e2bee-7de3-46e8-b6ee-76005f3f434f

EEACLC_maes <- read_csv('./data/EEA_CLC_landtake_countries_maes.csv') 
names(EEACLC_maes)

EEACLC_overall <- EEACLC_maes %>%
  gather(key, val, Cropland:`Rivers and lakes`)  %>%
  summarise(areaEstClc = (sum(val)/18 ) )
EEACLC_overall

EEACLC_maes %>%
  gather(key, val, Cropland:`Rivers and lakes`) %>%
  mutate(type = ifelse(key %in% c("Cropland"), 'cropland', 'nature')) %>%
  group_by(type) %>%
  summarise(areaEstClc =  (sum(val)/18 )) %>%
  dplyr::select( type, areaEstClc)

# Our estimate
EUareasEstOut %>%
  # Convert to annual rate
  mutate_at(vars(area, ciArea, area_mapped), function(x) x/6) %>%
  filter(stratum == 'land take')
1543/998
(1543 - 998) / 998

907/606
590/392

EEACLC_maes %>%
  gather(key, val, Cropland:`Rivers and lakes`) %>%
  mutate(type = ifelse(key %in% c("Cropland"), 'cropland', 'nature')) %>%
  group_by(type, country) %>%
  summarise(area = sum(val)) %>%
  filter(type == 'nature') %>%
  left_join(population2023, by = 'country') %>%
  mutate(lue = area*1000000 / (PopTotal*1000))  %>%
  ggplot(aes(x = reorder(country, lue), y = lue)) +
  geom_point() +
  coord_flip() +
  labs(x = '',
       title = 'Land take efficiency',
       y = 'Land take per citizen \nin 2023 (m2 / person)')

colors <- c("CLC AL" = "#f49f4f", "Venter et al" = "#c493f5")

lueCompPlot1 <- lueDFlandtake %>%
  left_join(EEACLC_maes %>%
              gather(key, val, Cropland:`Rivers and lakes`)  %>%
              group_by( country) %>%
              summarise(areaEstClc = (sum(val)/18 ) ) %>%
              dplyr::select(country, areaEstClc)) %>%
  mutate(lueClc = areaEstClc*1000000  / (PopTotal*1000),
         foldDiff = lue/lueClc) %>%
  ggplot(aes(x = reorder(country, lue))) +
  geom_segment(aes(y = lue, yend = lueClc))+
  geom_point(aes(y = lueClc, color='CLC AL')) +
  geom_point(aes(y = lue, color='Venter et al'))  +
  scale_color_manual(values = colors)+
  coord_flip() +
  labs(y = expression('Land take ('~m^-2~citizen^-1~yr^-1~')'),
       x = '') +
  labs(title = "A) Land take total") + 
  theme(axis.title = element_text(size=10),
        legend.background = element_blank(),
        legend.position = c(0.7,0.3),
        legend.title = element_blank())
lueCompPlot1

lueCompPlot2 <- lueDFnature %>%
  left_join(EEACLC_maes %>%
              gather(key, val, Cropland:`Rivers and lakes`) %>%
              mutate(type = ifelse(key %in% c("Cropland"), 'cropland', 'nature')) %>%
              group_by(type, country) %>%
              summarise(areaEstClc =  (sum(val)/18 )) %>%
              filter(type == 'nature')  %>%
              dplyr::select(country, areaEstClc)) %>%
  mutate(lueClc = areaEstClc*1000000  / (PopTotal*1000))%>%
  ggplot(aes(x = reorder(country, lue))) +
  geom_segment(aes(y = lue, yend = lueClc))+
  geom_point(aes(y = lueClc), color='#f49f4f') +
  geom_point(aes(y = lue), color='#c493f5')  +
  coord_flip() +
  labs(y =expression('Nature take ('~m^-2~citizen^-1~yr^-1~')'),
       x = '') +
  labs(title = "B) Land take nature") + 
  theme(axis.title = element_text(size=10))

lueCompPlot3 <- lueDFcropland %>%
  left_join(EEACLC_maes %>%
              gather(key, val, Cropland:`Rivers and lakes`) %>%
              mutate(type = ifelse(key %in% c("Cropland"), 'cropland', 'nature')) %>%
              group_by(type, country) %>%
              summarise(areaEstClc =  (sum(val)/18 )) %>%
              filter(type == 'cropland')  %>%
              dplyr::select(country, areaEstClc)) %>%
  mutate(lueClc = areaEstClc*1000000  / (PopTotal*1000))%>%
  ggplot(aes(x = reorder(country, lue))) +
  geom_segment(aes(y = lue, yend = lueClc))+
  geom_point(aes(y = lueClc), color='#f49f4f') +
  geom_point(aes(y = lue), color='#c493f5')  +
  #geom_text(aes(label = paste0( round(foldDiff, 1), ' X'), y = arena), hjust=-0.1) +
  #ylim(0, 1800) +
  coord_flip() +
  labs(y = expression('Cropland take ('~m^-2~citizen^-1~yr^-1~')'),
       x = '') +
  labs(title = "C) Land take cropland") + 
  theme(axis.title = element_text(size=10))


clcArenaLueCompPlot <- grid.arrange( lueCompPlot1, lueCompPlot2, lueCompPlot3, ncol=3,
                                     padding = unit(0, "line"),widths=c(1,1,1), newpage = T)

ggsave("./output/lueCompPlot.png", clcArenaLueCompPlot, width = 24, height=15,units='cm')


#### Land use drivers -------------------------------------------------------------
areaEstCountry <- read_csv('./data/area_estimates_countries.csv') %>%
  # Convert to annual rate
  mutate_at(vars(area, ciArea, area_mapped), function(x) x/6) %>%
  mutate(unbiased_area = area)

euLUdrivers <- samplesRaw %>%
  # filter for nature losses
  filter(verification == 'yes') %>% 
  mutate(change_type = ifelse(is.na(change_type), 'Uncertain', change_type)) %>%
  # Filter out uncertain
  filter(change_type != 'Uncertain') %>%
  # Determine if was cropland or nature take
  mutate(stratum = ifelse(baseline_land_use == 'cropland', 'cropland loss', 'nature loss')) %>%
  # get country name from other data frame
  left_join(mappedSamples %>%
              dplyr::select(PLOTID, country), by = 'PLOTID')%>%
  dplyr::select(PLOTID, country, stratum, change_type) %>%
  group_by(country, stratum, change_type) %>%
  # count samples per land use type and country
  count() %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(nTot = sum(n),
         prop = n/nTot) %>%
  # get area estimates per country
  left_join(areaEstCountry %>%
              filter(stratum %in% c('cropland loss', 'nature loss')) %>%
              dplyr::select(country, stratum, area=unbiased_area, ciArea), by = c('country', 'stratum')) %>%
  # multiply out proportions
  mutate(areaLU = area*prop) %>%
  # percentage change_type x stratum
  group_by(change_type, stratum) %>%
  summarise(area = sum(areaLU, na.rm=T)) %>%
  ungroup() %>%
  mutate(areaTot = sum(area),
         perc = area/areaTot*100)

print(euLUdrivers, n=25)

landuseDriverPlot <- euLUdrivers %>%
  mutate(change_type = str_wrap(change_type, width = 25))%>%
  mutate(stratum = ifelse(stratum == 'nature loss', 'land take nature', 'land take cropland')) %>%
  ggplot(aes(y = reorder(change_type, perc), x = perc, fill=stratum)) +
  geom_bar(stat='identity', position = 'dodge', alpha=0.8) +
  geom_label(aes(label = paste0(round(perc,1), '%'), x = perc+1), 
             position = position_dodge(width=1), 
             alpha=0.5,
             show.legend =   FALSE,
             size=2.5) +
  scale_fill_manual(values = c( '#63c8c9','#bb63b1')) +
  labs(y = 'Land use driver',
       x = 'Share of land take (%)') +
  theme(legend.position = c(0.5,0.5),
        legend.title = element_blank(),
        legend.background = element_blank())
landuseDriverPlot
ggsave("./output/landuseDriverPlot.png", landuseDriverPlot, width = 25, height=14, units='cm')



#### Country-level area estimation - sensitivity to human error --------------------------------------------

areasOutSensitivity <- tibble()
iterations <- c(1,2,3,4,5)
it <- 1
for (it in iterations){
  
  
  # Empty tibble to house area estimates from loop
  areasEstOut <- tibble()
  
  area_mapped_countries <- read_csv('./data/area_mapped_countries.csv')
  
  areas <- area_mapped_countries
  
  samples_labelled_countries <- read_csv('./data/samples_labelled_countries.csv')
  merged <- samples_labelled_countries 
  
  # Set a random seed
  set.seed(it+2)
  # Get 125 samples mapped as buffer and labelled as stable and then reverse the human label
  addInYes <- merged %>%
    filter(str_detect(m, 'buffer')) %>%
    filter(str_detect(r, 'stable')) %>%
    sample_n(125) %>%
    mutate(r = ifelse(r == 'buffer crop/built stable', 'cropland loss', 
                      ifelse(r == 'buffer nature stable', 'nature loss', r)))
  
  set.seed(it+12)
  # Get 125 samples mapped as buffer and labelled as land take and then reverse the human label
  addInNo <- merged %>%
    filter(str_detect(m, 'buffer')) %>%
    filter(str_detect(r, 'loss'))%>%
    sample_n(125) %>%
    mutate(r = ifelse(r == 'cropland loss', 'buffer crop/built stable', 
                      ifelse(r == 'nature loss', 'buffer nature stable', r)))
  
  merged <- merged %>%
    filter(!PLOTID %in% addInYes$PLOTID) %>%
    filter(!PLOTID %in% addInNo$PLOTID) %>%
    bind_rows(addInYes) %>%
    bind_rows(addInNo)
  
  # Loop through countries and calculate design-based areas
  c <- 'ESP'
  for (c in unique(mappedSamples$country)){
    
    # Get country subset
    combined <- merged %>%
      filter(country == c)%>%
      drop_na(r)  %>%
      mutate(r = ifelse(str_detect(r, 'loss'), 'land take',r)) %>%
      mutate(m = ifelse(str_detect(m, 'loss'), 'land take',m)) %>%
      ungroup()
    combined %>% ggplot(aes(x=r, fill=m)) + geom_bar()
    
    # Get areas in a format ready for the design-based estimation function
    Nh<-areas %>%
      #filter(!str_detect(stratum, 'buffer')) %>%
      filter(country == c) %>%
      mutate(stratum = ifelse(str_detect(stratum, 'loss'), 'land take',stratum))  %>%
      group_by(country, stratum) %>%
      summarise(area = sum(area)) %>%
      ungroup() %>%
      dplyr::select(-country) %>%
      deframe()
    
    
    # For design-based estimation, we subsume the buffer stratum into stable for reference pts only
    combinedForEstimation <- combined %>%
      mutate(r= ifelse(r == 'buffer nature stable', 'nature stable', 
                       ifelse(r == 'buffer crop/built stable', 'crop/built stable', r)))
    
    # Run stratified area estimator
    e<-olofsson(combinedForEstimation$r, combinedForEstimation$m, Nh)
    e$UA
    e$SEua*qnorm(0.975)
    
    # Unpack outputs into a tibble
    changeDF <- tibble()
    for (i in seq(1, length(e$area))) {
      ua_i <- e$UA[i]
      pa_i <- e$PA[i]
      
      subChangeDF <- tibble(
        stratum = names(e$area)[i],
        area = e$area[i] * sum(Nh),
        prop = e$area[i],
        se = e$SEa[i] * sum(Nh),
        pa = pa_i,
        ua = ua_i,
        seUa = e$SEua[i],
        sePa = e$SEpa[i],
        oa = e$OA,
        ciArea = qnorm(0.975) * e$SEa[i] * sum(Nh),
        f1 = ifelse((ua_i + pa_i) > 0, 2 * ua_i * pa_i / (ua_i + pa_i), NA_real_)
      )
      changeDF <- changeDF %>% bind_rows(subChangeDF)
    }
    changeDF$Nh <- Nh
    changeDF$country  <- c
    
    # Combine with output tibble
    areasEstOut <- areasEstOut %>%
      bind_rows(changeDF)
    
    
  }
  
  areasEstOut$iteration  <- it
  
  areasOutSensitivity <- areasOutSensitivity %>%
    bind_rows(areasEstOut)
}
areasOutSensitivity

areasEstCountries <- read_csv('./data/area_estimates_countries.csv')


stratEstLandTakeCountryPlot_sensitivityBufferError <- areasEstCountries %>%
  filter(stratum %in% c('land take')) %>%
  mutate(sig = ifelse((area-ciArea) < 0, '', 'x')) %>%
  ggplot(aes(x = reorder(country, area), y = area)) +
  geom_hline(yintercept=0) +
  geom_point(aes(x = reorder(country, area), y = area_mapped), inherit.aes=F, color='#eb56ff', size=2) +
  geom_point(color='blue', shape=21, fill=NA, size=2) +
  geom_errorbar(aes(ymax=area-ciArea, ymin=area+ciArea), width=0,color='blue', linewidth=1.2) +
  geom_point(data = areasOutSensitivity %>%
               filter(stratum %in% c('land take')), 
             color='red', size=2, shape=4) +
  labs(title = "Area estimates for land take. <span style='color: #eb56ff;'> Pink represent pixel counts.</span> <span style='color: blue;'> Blue represent unbiased estimates.</span>",
       subtitle = "<span style='color: red;'> Red represent unbiased with 10% human error.</span> Blue bar represents 95% CI.") + 
  theme(plot.title = element_markdown(hjust = 0.5,face="bold", size = 8),
        plot.subtitle = element_markdown(hjust = 0.5, size = 8))+
  coord_flip() +
  labs(x = '',
       y = 'Area land take (km2)')
stratEstLandTakeCountryPlot_sensitivityBufferError
ggsave("./output/stratEstLandTakeCountryPlot_sensitivityBufferError.png", stratEstLandTakeCountryPlot_sensitivityBufferError, width = 16, height=18, units='cm')
