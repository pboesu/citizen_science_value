library(spaa)
library(vegan)
library(tidyverse)

wales_birds <- read_csv('Data/uk_birds/bird_data_wales_2015_2018.csv') %>% 
  rename_all(~ gsub(" ", "_", .)) %>% 
  dplyr::select(OBSERVATION_DATE, LATITUDE, LONGITUDE, TAXONOMIC_ORDER)

UK <- getData("GADM", country="GB", level=1)
WALES <- subset(UK, NAME_1=='Wales')

# 0.1 degree unit size

wales_birds <- wales_birds %>% 
  mutate(GridID = paste(round(LATITUDE,1), round(LONGITUDE,1), sep='_'))

# Richness and Sampling Effort per grid 
wales_birds %>% 
  group_by(GridID) %>% 
  summarise(SR=n_distinct(TAXONOMIC_ORDER), N=n())  

wales_birds <- wales_birds %>% 
  mutate(year=year(OBSERVATION_DATE))


# Get species (TAXONOMIC_ORDER) abundance per grid

abundance_wales_birds_2015 <- wales_birds %>% 
  filter(year==2015) %>% 
  group_by(GridID) %>% 
  arrange(desc(GridID)) %>% 
  mutate(sampleNumber=row_number()) %>% 
  dplyr::select(cell_id=GridID, sample=sampleNumber, species=TAXONOMIC_ORDER)

abundance_wales_birds_2016 <- wales_birds %>% 
  filter(year==2016) %>% 
  group_by(GridID) %>% 
  arrange(desc(GridID)) %>% 
  mutate(sampleNumber=row_number()) %>% 
  dplyr::select(cell_id=GridID, sample=sampleNumber, species=TAXONOMIC_ORDER)

abundance_wales_birds_2017 <- wales_birds %>% 
  filter(year==2017) %>% 
  group_by(GridID) %>% 
  arrange(desc(GridID)) %>% 
  mutate(sampleNumber=row_number()) %>% 
  dplyr::select(cell_id=GridID, sample=sampleNumber, species=TAXONOMIC_ORDER)

abundance_wales_birds_2018 <- wales_birds %>% 
  filter(year==2018) %>% 
  group_by(GridID) %>% 
  arrange(desc(GridID)) %>% 
  mutate(sampleNumber=row_number()) %>% 
  dplyr::select(cell_id=GridID, sample=sampleNumber, species=TAXONOMIC_ORDER)

# The function ```get_gridsSlopes``` finds a species accumulation curve (SAC) for each grid-cell using the method ‘exact’ of the function specaccum of the vegan package and then calculates the degree of curvilinearity as the mean slope of the last 10% of the curve.

get_gridsSlopes <- function(data_abundance){
  GridSlope <- data.frame(Grid=integer(), Slope=numeric(), stringsAsFactors=FALSE)
  data_abundance <- as.data.frame(data_abundance) #if it is a tibble
  data_abundance$abundance <- as.integer(1)
  cells <- unique(data_abundance$cell_id)
  splistT <- list()
  spaccum <- list()
  slope <- list()
  for (i in cells) {
    splist <- data_abundance[data_abundance$cell_id == i,c(2:4)]
    splistT[[i]] = data2mat(splist) 
    spaccum[[i]] = specaccum(splistT[[i]], method = "exact")
    slope[[i]] = (spaccum[[i]][[4]][length(spaccum[[i]][[4]])]-
                    spaccum[[i]][[4]][ceiling(length(spaccum[[i]][[4]])*0.9)])/
      (length(spaccum[[i]][[4]])-
         ceiling(length(spaccum[[i]][[4]])*0.9))
    GridSlope_i <- data.frame(Grid=i, Slope=slope[[i]], stringsAsFactors=FALSE)
    GridSlope <- rbind(GridSlope, GridSlope_i)
  }  
  return(GridSlope)
}  

SACs_2015 <- get_gridsSlopes(abundance_wales_birds_2015)
SACs_2016 <- get_gridsSlopes(abundance_wales_birds_2016)
SACs_2017 <- get_gridsSlopes(abundance_wales_birds_2017)
SACs_2018 <- get_gridsSlopes(abundance_wales_birds_2018)


SACs_2015 <- SACs_2015 %>% 
  mutate(Slope=(ifelse(is.na(Slope), 1, Slope)), year=2015)

SACs_2016 <- SACs_2016 %>% 
  mutate(Slope=(ifelse(is.na(Slope), 1, Slope)), year=2016)

SACs_2017 <- SACs_2017 %>% 
  mutate(Slope=(ifelse(is.na(Slope), 1, Slope)), year=2017)

SACs_2018 <- SACs_2018 %>% 
  mutate(Slope=(ifelse(is.na(Slope), 1, Slope)), year=2018)

wales_birds <- wales_birds %>% 
  mutate(LATITUDE= round(LATITUDE, 1)) %>% 
  mutate(LONGITUDE= round(LONGITUDE, 1))

wales_birds_SACs <- left_join(bind_rows(SACs_2015, SACs_2016, SACs_2017, SACs_2018), dplyr::select(wales_birds, Grid=GridID, LATITUDE, LONGITUDE)) %>%
  dplyr::select(LATITUDE, LONGITUDE, Slope, year) %>% 
    distinct()

ggplot() + 
  geom_polygon(data=WALES, aes(long,lat,group=group), fill="white") +
  geom_path(data=WALES, aes(long,lat, group=group), color="grey", size=0.1) +
  geom_point(data = wales_birds_SACs, aes(x=LONGITUDE, y = LATITUDE, color = Slope)) +
  facet_wrap(~year) +
  scale_color_viridis_c() +
  labs(x='', y= '', color = '') +
  theme_bw()


save(wales_birds_SACs, file = "samplingeffort.RData")



