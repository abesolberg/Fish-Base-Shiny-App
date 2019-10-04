library(leaflet)
library(robis)
library(tidyverse)

background.fish <- c(
  "Amphiprion ocellaris" , 
  "Salmo salar" ,
  "Oncorhynchus kisutch" ,
  "Thunnus orientalis" , 
  "Thunnus albacares" ,
  #"Coptodon zillii" ,
  "Carcharhinus leucas"
)

point.locations <- robis::occurrence(background.fish[1]) %>% 
  select(year = date_year ,
         species = scientificName , 
         lat = decimalLatitude , 
         lng = decimalLongitude)

if (nrow(out.file) > 500 ) {
  out.file <- sample_n(out.file , 500)
}

for (i in 2:length(background.fish)) {
  
  out.file <- robis::occurrence(background.fish[i]) %>% 
    select(year = date_year ,
           species = scientificName , 
           lat = decimalLatitude , 
           lng = decimalLongitude)
  
  if (nrow(out.file) > 500 ) {
    out.file <- sample_n(out.file , 500)
  }
  
  point.locations <- bind_rows(point.locations , out.file)
  
}

point.locations <- point.locations %>% mutate(species = factor(species))

df <- point.locations %>% 
  left_join(
    bind_cols(species = background.fish , order = c(1:6)) ,
    by = "species"
  ) %>% 
  left_join(
    colors , by = "order"
  )

write_rds(df , 'background-locations.RDS')
