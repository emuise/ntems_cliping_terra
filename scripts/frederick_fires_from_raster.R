library(terra)
library(tidyverse)
library(sf)

folder <-
  here::here("D:/", "Bud", "bc", "mosaiced", "change_annual")
files <- list.files(folder, pattern = "annual.dat$",
                    full.names = T)

rasts <- files %>%
  map(rast)

bcb <- bcmaps::bc_bound() %>%
  vect()

bcb_rast <- bcb %>%
  rasterize(y = rasts[[1]]) %>%
  crop(bcb)

r <- rasts[[1]]

fire_vect <- as.polygons(r == 1)

output <- st_as_sf(fire_vect) %>%
  mutate(area = st_area(.)) %>%
  filter(area != max(area)) %>%
  st_cast("POLYGON") %>%
  mutate(year = sources(r) %>%
           str_extract("[0-9]{4}"))

for (r in rasts[2:length(rasts)]) {
  print(r)
  fire_poly <- as.polygons(r == 1) %>%
    st_as_sf() %>%
    mutate(area = st_area(.)) %>%
    filter(area != max(area)) %>%
    st_cast("POLYGON") %>%
    mutate(year = sources(r) %>%
             str_extract("[0-9]{4}"))
  
  output <- rbind(output, fire_poly)
}

output <- output %>%
  vect() %>%
  mask(bcb) %>%
  st_as_sf() %>%
  mutate(area = st_area(.))


write_sf(
  output,
  here::here(
    "Z:/",
    "ByUser",
    "Muise",
    "frederick_fires",
    "from_rasters.shp"
  ),
  overwrite = T
)
