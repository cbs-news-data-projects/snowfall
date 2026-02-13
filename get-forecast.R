# Libraries
library(httr)
library(terra)
library(jsonlite)

# 1. Download latest NDFD snowfall GRIB2
url <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.conus/VP.001-003/ds.snow.bin"

grib_file <- tempfile(fileext = ".bin")
res <- GET(url, write_disk(grib_file, overwrite = TRUE), progress())

if (res$status_code != 200) stop("Download failed!")

# 2. Read GRIB
snow <- rast(grib_file)

# Confirm at least 12 layers exist
if (nlyr(snow) < 12) stop("Not enough forecast layers available.")

# 3. Select first 72 hours (12 × 6-hour layers)
layers_in <- snow[[1:12]] * 39.3701   # meters → inches

# 4. Compute TRUE forecast window
# (6-hour periodic product)
layer_times_utc <- time(snow[[1:12]])

# First period START = first valid time − 6 hours
forecast_start_utc <- as.POSIXct(layer_times_utc[1], tz="UTC") - 6*3600

# Final period END = last valid time
forecast_end_utc   <- as.POSIXct(layer_times_utc[12], tz="UTC")

# Convert to Eastern time
forecast_start_est <- format(forecast_start_utc,
                             tz="America/New_York",
                             "%Y-%m-%dT%H:%M:%S%z")

forecast_end_est <- format(forecast_end_utc,
                           tz="America/New_York",
                           "%Y-%m-%dT%H:%M:%S%z")

# 5. Sum snowfall (PERIODIC accumulation)
snow_total_in <- sum(layers_in)

# 6. Reproject to WGS84
snow_wgs84 <- project(snow_total_in, "EPSG:4326")

# 7. Reduce density (optional but recommended)
snow_coarse <- aggregate(
  snow_wgs84,
  fact = 10,
  fun = mean,
  na.rm = TRUE
)

# 8. Convert raster → points
snow_pts <- as.data.frame(snow_coarse, xy = TRUE, na.rm = TRUE)
colnames(snow_pts) <- c("lon", "lat", "snow_in")

snow_pts <- subset(snow_pts, snow_in > 0.01)

# 9. Issued time (auto)
issued_time_est <- format(Sys.time(),
                          tz="America/New_York",
                          "%Y-%m-%dT%H:%M:%S%z")

# 10. Build GeoJSON
geojson <- list(
  type = "FeatureCollection",
  metadata = list(
    forecast_start = forecast_start_est,
    forecast_end = forecast_end_est,
    issued = issued_time_est,
    description = "72-hour snowfall forecast from NDFD (inches)"
  ),
  features = lapply(seq_len(nrow(snow_pts)), function(i) {
    list(
      type = "Feature",
      geometry = list(
        type = "Point",
        coordinates = c(snow_pts$lon[i], snow_pts$lat[i])
      ),
      properties = list(
        snowfall = round(snow_pts$snow_in[i], 2)
      )
    )
  })
)

format(time(snow),
       tz = "America/New_York",
       "%Y-%m-%d %H:%M %Z")

# 11. Save output
write_json(
  geojson,
  "snowfall_forecast_72hr.json",
  pretty = TRUE,
  auto_unbox = TRUE
)