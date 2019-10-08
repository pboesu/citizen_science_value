library(spaa)
library(vegan)
library(tidyverse)

wales_birds <- read_csv('Data/uk_birds/bird_data_wales_2015_2018.csv') %>% 
  rename_all(~ gsub(" ", "_", .)) %>% 
  dplyr::select(LAST_EDITED_DATE, LATITUDE, LONGITUDE, TAXONOMIC_ORDER)

UK <- getData("GADM", country="GB", level=1)
WALES <- subset(UK, NAME_1=='Wales')

# 0.1 degree unit size

wales_birds <- wales_birds %>% 
  mutate(GridID = paste(round(LATITUDE,1), round(LONGITUDE,1), sep='_'))

# Richness and Sampling Effort per grid 
wales_birds %>% 
  group_by(GridID) %>% 
  summarise(SR=n_distinct(TAXONOMIC_ORDER), N=n())  

# Get species (TAXONOMIC_ORDER) abundance per grid

abundance_wales_birds <- wales_birds %>% 
  group_by(GridID) %>% 
  arrange(desc(GridID)) %>% 
  mutate(sampleNumber=row_number()) %>% 
  dplyr::select(cell_id=GridID, sample=sampleNumber, species=TAXONOMIC_ORDER)

# The function ```get_gridsSlopes``` finds a species accumulation curve (SAC) for each grid-cell using the method ‘exact’ of the function specaccum of the vegan package and then calculates the degree of curvilinearity as the mean slope of the last 10% of the curve. We considered grids with slope values > 0.05 as under-sampled and those with slope values ≤ 0.05 as well sampled.

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

SACs <- get_gridsSlopes(abundance_wales_birds)


SACs <- SACs %>% 
  mutate(Slope=(ifelse(is.na(Slope), 1, Slope)))

wales_birds <- wales_birds %>% 
  mutate(LAT= round(LATITUDE, 1)) %>% 
  mutate(LON= round(LONGITUDE, 1))

wales_birds_SACs <- left_join(as.tibble(SACs), dplyr::select(wales_birds, Grid=GridID, LAT, LON)) %>%
  dplyr::select(LAT, LON, Slope) %>% 
    distinct()

ggplot() + 
  geom_polygon(data=WALES, aes(long,lat,group=group), fill="white") +
  geom_path(data=WALES, aes(long,lat, group=group), color="grey", size=0.1) +
  geom_point(data = wales_birds_SACs, aes(x=LON, y = LAT, color = Slope)) +
  #scale_color_brewer(palette ='Spectral') +
  labs(x='', y= '', color = '') +
  theme_bw()




