# how isolated: the more isolated the more priority

library(readr)
library(dplyr)
library(raster)
library(spdplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(viridis)

ebd <- read_tsv("C:/Users/leadam/Downloads/QE Hackathon/ebd_sampling_filtered_GB_2015_2018.txt")
names(ebd) <- gsub(x = names(ebd),
                        pattern = "\\ ",
                        replacement = "_")

ebd_wales <- ebd %>%
  filter(STATE == "Wales") %>%
  mutate(lat = round(LATITUDE, 1)) %>%
  mutate(long = round(LONGITUDE, 1)) %>%
  unite("latlong", lat:long)


isolation_func <- function(df = ebd_wales, mindate = "2015-01-01", maxdate = "2015-12-31"){
  
  mindate <- date(mindate)
  maxdate <- date(maxdate)
  
  isolated <- df %>%
    dplyr::filter(OBSERVATION_DATE >= mindate, OBSERVATION_DATE <= maxdate) %>%
    count(latlong)
  
  isolated$isolation <- rescale(isolated$n, to = c(1,0))
  
  isolation <- left_join(isolated, df, by = "latlong")
  return(isolation)
}

# not pretty but works, aka I suck at loops, sorry
iso_prio_2015 <- isolation_func(ebd_wales, mindate = "2015-01-01", maxdate = "2015-12-31") %>%
  mutate(year = 2015)
iso_prio_2016 <- isolation_func(ebd_wales, mindate = "2016-01-01", maxdate = "2016-12-31") %>%
  mutate(year = 2016)
iso_prio_2017 <- isolation_func(ebd_wales, mindate = "2017-01-01", maxdate = "2017-12-31") %>%
  mutate(year = 2017)
iso_prio_2018 <- isolation_func(ebd_wales, mindate = "2018-01-01", maxdate = "2018-12-31") %>%
  mutate(year = 2018)

iso_prio <- bind_rows(iso_prio_2015, iso_prio_2016, iso_prio_2017, iso_prio_2018) %>%
  dplyr::select(latlong, isolation, LATITUDE, LONGITUDE, year)

save(iso_prio, file = "isolation.RData")

tiff("Isolationplot.tiff")
ggplot(data = iso_prio) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = isolation)) +
  scale_color_viridis(option = "plasma", "Priority") +
  theme_minimal()
dev.off()
