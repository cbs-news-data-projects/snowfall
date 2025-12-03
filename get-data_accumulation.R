library(terra)
library(sf)
library(httr)
library(lubridate)

# ----------------------------
# 1. Construct today's URL at 12 UTC
# ----------------------------
today <- Sys.Date()
year_month <- format(today, "%Y%m")           # e.g., "202512"
date_tag   <- format(today, "%Y%m%d")         # e.g., "20251203"
hour_tag   <- "12"                             # fixed 12 UTC

url <- paste0(
  "https://www.nohrsc.noaa.gov/snowfall/data/",
  year_month, "/sfav2_CONUS_72h_", date_tag, hour_tag, ".tif"
)

# ----------------------------
# 2. Download the TIFF
# ----------------------------
dir.create("data", showWarnings = FALSE)
tif_file <- file.path("data", paste0("snow_", date_tag, ".tif"))

message("Downloading snowfall TIFF: ", url)
resp <- httr::GET(url, write_disk(tif_file, overwrite = TRUE))
if(resp$status_code != 200){
  stop("Failed to download file: ", url)
}

# ----------------------------
# 3. Load your snowfall GeoTIFF
# ----------------------------
snow_raster <- rast(tif_file)

snow_raster
plot(snow_raster)

# ----------------------------
# 4. Reproject to Web Mercator (optional but recommended for web map tiles)
# ----------------------------
snow_raster_3857 <- project(snow_raster, "EPSG:3857")
out_file <- file.path("data", paste0("snow_", date_tag, "_3857.tif"))
writeRaster(snow_raster_3857, out_file, overwrite=TRUE)

message("✅ Reprojected raster saved to ", out_file)