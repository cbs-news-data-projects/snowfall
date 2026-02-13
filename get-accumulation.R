# Libraries
library(httr)
library(terra)
library(jsonlite)

# 1. Download NOHRSC 72-hour accumulation GRIB2
url <- "https://www.nohrsc.noaa.gov/snowfall_v2/data/202602/sfav2_CONUS_72h_2026021212_grid184.grb2"

grib_file <- tempfile(fileext = ".grb2")
res <- GET(url, write_disk(grib_file, overwrite = TRUE), progress())

if (res$status_code != 200) stop("Download failed!")

# 2. Read GRIB
snow <- rast(grib_file)

# 3. Convert to inches (assuming data is in meters)
snow_in <- snow * 39.3701   # meters → inches

# 4. Extract time information from filename
# sfav2_CONUS_72h_2026021212_grid184.grb2
# This represents 72-hour accumulation ending at 2026-02-12 12:00 UTC
forecast_end_utc <- as.POSIXct("2026-02-12 12:00:00", tz="UTC")
forecast_start_utc <- forecast_end_utc - 72*3600  # 72 hours before

# Convert to Eastern time
forecast_start_est <- format(forecast_start_utc,
                             tz="America/New_York",
                             "%Y-%m-%dT%H:%M:%S%z")

forecast_end_est <- format(forecast_end_utc,
                           tz="America/New_York",
                           "%Y-%m-%dT%H:%M:%S%z")

# 5. Reproject to WGS84
snow_wgs84 <- project(snow_in, "EPSG:4326")

# 6. Reduce density (optional but recommended)
snow_coarse <- aggregate(
  snow_wgs84,
  fact = 10,
  fun = mean,
  na.rm = TRUE
)

# 7. Convert raster → points
snow_pts <- as.data.frame(snow_coarse, xy = TRUE, na.rm = TRUE)
colnames(snow_pts) <- c("lon", "lat", "snow_in")

snow_pts <- subset(snow_pts, snow_in > 0.01)

# 8. Issued time (auto)
issued_time_est <- format(Sys.time(),
                          tz="America/New_York",
                          "%Y-%m-%dT%H:%M:%S%z")

# 9. Build GeoJSON
geojson <- list(
  type = "FeatureCollection",
  metadata = list(
    forecast_start = forecast_start_est,
    forecast_end = forecast_end_est,
    issued = issued_time_est,
    description = "72-hour snowfall accumulation from NOHRSC (inches)"
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

# 10. Save output
write_json(
  geojson,
  "snowfall_accumulation_72hr.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("Successfully saved snowfall_accumulation_72hr.json\n")
