# Libraries
library(httr)
library(terra)
library(grDevices)
library(jsonlite)

# 1. Download NOHRSC 72-hour accumulation GRIB2
# Get current date in Eastern time
current_time_et <- Sys.time()
attr(current_time_et, "tzone") <- "America/New_York"
current_date_et <- as.Date(current_time_et)

# Set forecast period based on current date (always today at 7pm ET back to 3 days ago at 7pm ET)
forecast_end_et <- as.POSIXct(paste(current_date_et, "19:00:00"), tz="America/New_York")
forecast_start_et <- forecast_end_et - 72*3600  # 72 hours before

# Convert to UTC for downloading (7pm ET today = 00:00 UTC tomorrow)
forecast_end_utc <- as.POSIXct(paste(current_date_et + 1, "00:00:00"), tz="UTC")

# Try downloading current and up to 3 previous 12-hour intervals
grib_file <- tempfile(fileext = ".grb2")
success <- FALSE

for (attempt in 0:3) {
  try_time <- forecast_end_utc - (attempt * 12 * 3600)
  date_str <- format(try_time, "%Y%m%d%H")
  year_month <- format(try_time, "%Y%m")
  
  url <- sprintf("https://www.nohrsc.noaa.gov/snowfall/data/%s/sfav2_CONUS_72h_%s_grid184.grb2",
                 year_month, date_str)
  
  cat("Attempting:", url, "\n")
  
  res <- GET(url, write_disk(grib_file, overwrite = TRUE), progress())
  
  if (res$status_code == 200) {
    cat("Successfully downloaded data for:", format(try_time, "%Y-%m-%d %H:%M UTC"), "\n")
    success <- TRUE
    break
  } else if (res$status_code == 404) {
    cat("File not found (404), trying earlier time...\n")
  } else {
    cat("Download failed with status code:", res$status_code, "\n")
  }
}

if (!success) {
  stop("Could not download data from current or previous 3 time intervals")
}

# 2. Read GRIB
snow <- rast(grib_file)

# 3. Convert to inches (assuming data is in meters)
snow_in <- snow * 39.3701   # meters → inches

# 4. Format time range as strings for metadata
forecast_start_est <- format(forecast_start_et,
                             tz="America/New_York",
                             "%Y-%m-%dT%H:%M:%S%z")

forecast_end_est <- format(forecast_end_et,
                           tz="America/New_York",
                           "%Y-%m-%dT%H:%M:%S%z")

# 5. Reproject to Web Mercator (better for tiling)
snow_3857 <- project(snow_in, "EPSG:3857")

# 6. Remove near-zero noise
snow_3857[snow_3857 < 0.01] <- 0

# 7. Clamp extreme values
snow_3857[snow_3857 > 40] <- 40

# Define color mapping function - returns NA for no data or zero
get_color <- function(snowfall) {
  if (is.na(snowfall) || snowfall == 0) return(NA)  # transparent for NA or zero
  if (snowfall < 1) return("#c7d6ef")  # light blue for trace to <1"
  if (snowfall < 2) return("#8faedf")
  if (snowfall < 3) return("#5786d0")
  if (snowfall < 4) return("#1f5dc0")
  if (snowfall < 6) return("#013f8c")
  if (snowfall < 8) return("#fad089")
  if (snowfall < 12) return("#f1b24b")
  if (snowfall < 18) return("#fd8724")
  if (snowfall < 24) return("#cf512b")
  if (snowfall < 30) return("#a5091e")
  if (snowfall < 36) return("#820415")
  return("#2d1b47")
}

# 9. Create RGB raster
snow_rgb <- rast(nlyrs = 3, crs = crs(snow_3857),
                 ext = ext(snow_3857), resolution = res(snow_3857))

# Get flattened values
vals <- values(snow_3857)

# Map each value to RGB (keeping NA as NA)
colors_hex <- sapply(vals, get_color)
cols <- col2rgb(colors_hex, alpha = FALSE)

# Assign to RGB layers (NA values stay NA = transparent)
values(snow_rgb[[1]]) <- cols[1, ]
values(snow_rgb[[2]]) <- cols[2, ]
values(snow_rgb[[3]]) <- cols[3, ]

# 10. Write colored GeoTIFF with metadata (ready for gdal2tiles)
writeRaster(
  snow_rgb,
  "snowfall_72hr.tif",
  overwrite = TRUE,
  datatype = "INT1U",
  wopt = list(gdal = c(
    "COMPRESS=DEFLATE",
    "PHOTOMETRIC=RGB",
    sprintf("TIFFTAG_DATETIME=%s", format(Sys.time(), "%Y:%m:%d %H:%M:%S")),
    sprintf("TIFFTAG_IMAGEDESCRIPTION=72hr snow accumulation: %s to %s", forecast_start_est, forecast_end_est)
  ))
)

# 11. Save forecast dates to temporary files for later metadata creation
writeLines(forecast_start_est, ".accumulation_forecast_start")
writeLines(forecast_end_est, ".accumulation_forecast_end")

cat("Saved colored RGB GeoTIFF: snowfall_72hr.tif\n")
cat("Saved forecast dates for metadata\n")