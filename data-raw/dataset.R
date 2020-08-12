# Read the RDS files
shape_trip_feasibility_chademo <-
  readRDS("data/shape_trip_feasibility_chademo.Rds")
shape_trip_feasibility_combo <-
  readRDS("data/shape_trip_feasibility_combo.Rds")
buf_merged <- readRDS("data/buf_merged.Rds")
buf_critical_ll <- readRDS("data/buf_critical_ll.Rds")

usethis::use_data(shape_trip_feasibility_chademo, 
                  shape_trip_feasibility_combo, 
                  buf_merged, 
                  buf_critical_ll, 
                  internal = TRUE)
