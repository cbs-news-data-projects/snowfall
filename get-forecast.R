# ----------------------------
# Libraries
# ----------------------------
library(httr)
library(terra)
library(jsonlite)

# ----------------------------
# 1. Download GRIB2
# ----------------------------
url <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.conus/VP.001-003/ds.snow.bin"
grib_file <- tempfile(fileext = ".bin")
res <- GET(url, write_disk(grib_file, overwrite = TRUE), progress())
if(res$status_code != 200) stop("Download failed!")

# ----------------------------
# 2. Read GRIB2 with terra
# ----------------------------
snow <- rast(grib_file)

# ----------------------------
# 2.5. Inspect individual layers
# ----------------------------
cat("\n=== Layer Inspection (Times in EST) ===\n")
cat("Total layers in file:", nlyr(snow), "\n")

for(i in 1:min(12, nlyr(snow))) {
  layer <- snow[[i]]
  layer_in <- layer * 39.3701  # Convert to inches
  
  # Get timestamp from layer metadata and convert to EST
  time_info <- time(layer)
  time_est <- format(as.POSIXct(time_info, tz="UTC"), "%Y-%m-%d %H:%M EST", tz="America/New_York")
  
  cat(sprintf("Layer %2d: ", i))
  cat(sprintf("Time: %s | ", time_est))
  cat(sprintf("Max: %.3f in | ", max(values(layer_in), na.rm=TRUE)))
  cat(sprintf("Mean (non-zero): %.3f in\n", 
              mean(values(layer_in)[values(layer_in) > 0], na.rm=TRUE)))
}

# Check if layers are cumulative or individual periods
cat("\n=== Checking if layers are cumulative ===\n")
n_layers_total <- nlyr(snow)
n_layers_to_use <- min(12, n_layers_total)

layer1_in <- snow[[1]] * 39.3701
last_layer_in <- snow[[n_layers_to_use]] * 39.3701
sum_all_in <- sum(snow[[1:n_layers_to_use]]) * 39.3701

cat(sprintf("Layer 1 max: %.3f in\n", max(values(layer1_in), na.rm=TRUE)))
cat(sprintf("Layer %d max: %.3f in\n", n_layers_to_use, max(values(last_layer_in), na.rm=TRUE)))
cat(sprintf("Sum of all %d layers max: %.3f in\n", n_layers_to_use, max(values(sum_all_in), na.rm=TRUE)))
cat(sprintf("\nIf Layer %d ≈ Sum, layers are CUMULATIVE (use last layer only)\n", n_layers_to_use))
cat(sprintf("If Layer %d < Sum, layers are INDIVIDUAL periods (sum them)\n", n_layers_to_use))
cat("\n")

# ----------------------------
# 3. Use LAST layer only (cumulative total)
# ----------------------------
snow_total <- snow[[n_layers_to_use]]

# ----------------------------
# 4. Convert meters → inches
# ----------------------------
snow_total_in <- snow_total * 39.3701

# ----------------------------
# 5. Reproject to WGS84
# ----------------------------
snow_wgs84 <- project(snow_3day_in, "EPSG:4326")

# ----------------------------
# 6. Optional: Aggregate raster to reduce points
# ----------------------------
snow_coarse <- aggregate(snow_wgs84, fact=10, fun=mean, na.rm=TRUE)  # 10x10 cell aggregation (use mean, not sum!)

# ----------------------------
# 7. Convert raster to points (keep non-zero only)
# ----------------------------
snow_pts <- as.data.frame(snow_coarse, xy = TRUE, na.rm = TRUE)
colnames(snow_pts) <- c("lon", "lat", "snow_in")
snow_pts <- subset(snow_pts, snow_in > 0.01)  # Keep only non-zero snow

# ----------------------------
# 8. Add 6-hour increments efficiently
# ----------------------------
increment_matrix <- matrix(snow_pts$snow_in / n_layers_to_use, nrow=nrow(snow_pts), ncol=n_layers_to_use)

# Get actual valid times from GRIB2 layers
time_seq <- time(snow[[1:n_layers_to_use]])
first_valid_time <- time_seq[1]
last_valid_time <- time_seq[length(time_seq)]

snow_pts$increments <- lapply(1:nrow(snow_pts), function(i){
  lapply(1:n_layers_to_use, function(j){
    list(
      hour = j*6,
      amount = increment_matrix[i,j],
      valid_time = format(as.POSIXct(time_seq[j], tz="UTC"), "%Y-%m-%dT%H:%M:%SZ")
    )
  })
})

# ----------------------------
# 9. Build GeoJSON structure
# ----------------------------
geojson <- list(
  type = "FeatureCollection",
  metadata = list(
    forecast_start = format(as.POSIXct(first_valid_time, tz="UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    forecast_through = format(as.POSIXct(last_valid_time, tz="UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    description = "3-day snowfall forecast from NDFD (inches)"
  ),
  features = lapply(1:nrow(snow_pts), function(i) list(
    type = "Feature",
    geometry = list(
      type = "Point",
      coordinates = c(snow_pts$lon[i], snow_pts$lat[i])
    ),
    properties = list(
      snowfall = snow_pts$snow_in[i],
      increments = snow_pts$increments[[i]]
    )
  ))
)

# ----------------------------
# 10. Save JSON
# ----------------------------
write_json(geojson, "snowfall_forecast.json", pretty = TRUE, auto_unbox = TRUE)
cat("JSON saved as snowfall_forecast.json\n")