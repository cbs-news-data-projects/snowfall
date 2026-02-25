# This script writes the metadata.json files for the tile directories
# It runs AFTER gdal2tiles.py to ensure it overwrites any metadata that gdal2tiles creates

library(jsonlite)

# Read saved forecast dates for accumulation
forecast_start_accum <- readLines(".accumulation_forecast_start", warn = FALSE)
forecast_end_accum <- readLines(".accumulation_forecast_end", warn = FALSE)

# Read saved forecast dates and layer count for forecast
forecast_start_fc <- readLines(".forecast_forecast_start", warn = FALSE)
forecast_end_fc <- readLines(".forecast_forecast_end", warn = FALSE)
max_layers <- as.integer(readLines(".forecast_layers", warn = FALSE))

# Get current timestamp for "issued" field
issued_time <- format(Sys.time(), tz="America/New_York", "%Y-%m-%dT%H:%M:%S%z")

# Write accumulation metadata
metadata_accum <- list(
  type = "accumulation",
  forecast_start = forecast_start_accum,
  forecast_end = forecast_end_accum,
  issued = issued_time,
  description = "72-hour observed snowfall accumulation (inches)"
)

write_json(
  metadata_accum,
  "tiles/accumulation/metadata.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

# Write forecast metadata
metadata_fc <- list(
  type = "forecast",
  forecast_start = forecast_start_fc,
  forecast_end = forecast_end_fc,
  issued = issued_time,
  layers = max_layers,
  description = sprintf("Snowfall forecast from NDFD (inches, %d layers)", max_layers)
)

write_json(
  metadata_fc,
  "tiles/forecast/metadata.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("Updated tiles/accumulation/metadata.json\n")
cat("  Forecast period:", forecast_start_accum, "to", forecast_end_accum, "\n")
cat("Updated tiles/forecast/metadata.json\n")
cat("  Forecast period:", forecast_start_fc, "to", forecast_end_fc, "\n")
cat("  Issued:", issued_time, "\n")
