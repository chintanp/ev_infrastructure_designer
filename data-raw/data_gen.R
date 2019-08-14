# This file generates the Rds files from the raw data 

library(sf)
library(sp)

LOOKUP_DISTANCE <- 10.0 / 0.000621371 # convert miles to m

shape_trip_feasibility_combo <-
  st_read(dsn = "data-raw/combo", layer = "shape_combo") %>% st_simplify() %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") 

shape_trip_feasibility_combo_utm <-
  shape_trip_feasibility_combo %>%
  st_transform("+proj=utm +zone=10 +datum=WGS84") 

shape_trip_feasibility_chademo <-
  st_read(dsn = "data-raw/chademo", layer = "shape_chademo") %>% st_simplify() %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") 

# Create a buffer around roads
buf_critical <-
  gBuffer(as(shape_trip_feasibility_combo_utm, 'Spatial'),
          width = LOOKUP_DISTANCE, 
          byid = TRUE)
# Transform the buffer
buf_critical_ll <- spTransform(buf_critical,
                               CRS("+proj=longlat +datum=WGS84 +no_defs"))

saveRDS(shape_trip_feasibility_combo, "data/shape_trip_feasibility_combo.Rds")
saveRDS(shape_trip_feasibility_combo_utm, "data/shape_trip_feasibility_combo_utm.Rds")
saveRDS(shape_trip_feasibility_chademo, "data/shape_trip_feasibility_chademo.Rds")
saveRDS(buf_critical, "data/buf_critical.Rds")
saveRDS(buf_critical_ll, "data/buf_critical_ll.Rds")
