# Libraries
library(httr)
library(terra)
library(grDevices)

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
forecast_end_utc <- as.POSIXct("2026-02-12 12:00:00", tz="UTC")
forecast_start_utc <- forecast_end_utc - 72*3600  # 72 hours before

# Convert to Eastern time
forecast_start_est <- format(forecast_start_utc,
                             tz="America/New_York",
                             "%Y-%m-%dT%H:%M:%S%z")

forecast_end_est <- format(forecast_end_utc,
                           tz="America/New_York",
                           "%Y-%m-%dT%H:%M:%S%z")

# 5. Reproject to Web Mercator (better for tiling)
snow_3857 <- project(snow_in, "EPSG:3857")

# 6. Remove near-zero noise
snow_3857[snow_3857 < 0.01] <- 0

# 7. Clamp extreme values
snow_3857[snow_3857 > 40] <- 40

# Define color mapping function with NA handling
get_color <- function(snowfall) {
  if (is.na(snowfall) || snowfall < 0.01) return("#c7d6ef")  # background / near zero
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

# Map each value to RGB
cols <- col2rgb(sapply(vals, get_color))

# Assign to RGB layers
values(snow_rgb[[1]]) <- cols[1, ]
values(snow_rgb[[2]]) <- cols[2, ]
values(snow_rgb[[3]]) <- cols[3, ]

# 10. Write colored GeoTIFF (ready for gdal2tiles)
writeRaster(
  snow_rgb,
  "snowfall_72hr.tif",
  overwrite = TRUE,
  datatype = "INT1U",
  wopt = list(gdal = c("COMPRESS=DEFLATE", "PHOTOMETRIC=RGB"))
)

cat("Saved colored RGB GeoTIFF: snowfall_72hr.tif\n")