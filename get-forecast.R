# Libraries
library(httr)
library(terra)
library(grDevices)

# 1. Download latest NDFD snowfall GRIB2
url <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.conus/VP.001-003/ds.snow.bin"

grib_file <- tempfile(fileext = ".bin")
res <- GET(url, write_disk(grib_file, overwrite = TRUE), progress())

if (res$status_code != 200) stop("Download failed!")

# 2. Read GRIB
snow <- rast(grib_file)

# Check available layers
n_layers <- nlyr(snow)
cat("Found", n_layers, "forecast layers\n")

if (n_layers < 1) stop("No forecast layers available.")

# 3. Use available layers (up to 12 for 72 hours)
max_layers <- min(n_layers, 12)
layers_in <- snow[[1:max_layers]] * 39.3701   # meters → inches

# 4. Compute TRUE forecast window
# (6-hour periodic product)
layer_times_utc <- time(snow[[1:max_layers]])

# First period START = first valid time − 6 hours
forecast_start_utc <- as.POSIXct(layer_times_utc[1], tz="UTC") - 6*3600

# Final period END = last valid time
forecast_end_utc   <- as.POSIXct(layer_times_utc[max_layers], tz="UTC")

# Convert to Eastern time
forecast_start_est <- format(forecast_start_utc,
                             tz="America/New_York",
                             "%Y-%m-%dT%H:%M:%S%z")

forecast_end_est <- format(forecast_end_utc,
                           tz="America/New_York",
                           "%Y-%m-%dT%H:%M:%S%z")

# 5. Sum snowfall (PERIODIC accumulation)
snow_total_in <- sum(layers_in)

# 6. Reproject to Web Mercator (better for tiling)
snow_3857 <- project(snow_total_in, "EPSG:3857")

# 7. Remove near-zero noise
snow_3857[snow_3857 < 0.01] <- 0

# 8. Clamp extreme values
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

# 10. Write colored GeoTIFF (ready for gdal2tiles)
writeRaster(
  snow_rgb,
  "snowfall_forecast_72hr.tif",
  overwrite = TRUE,
  datatype = "INT1U",
  wopt = list(gdal = c("COMPRESS=DEFLATE", "PHOTOMETRIC=RGB"))
)

cat("Saved colored RGB GeoTIFF: snowfall_forecast_72hr.tif\n")
cat("Forecast period:", forecast_start_est, "to", forecast_end_est, "\n")