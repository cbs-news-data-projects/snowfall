# NOAA NDFD Snowfall Prediction Map
# Data source: https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/

# Load required libraries
library(terra)      # For reading GRIB2 files
library(sf)         # For spatial operations
library(ggplot2)    # For plotting
library(viridis)    # For color scales
library(httr)       # For downloading files
library(maps)       # For state boundaries

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Function to download NDFD snowfall data
download_snowfall_data <- function(time_period = "001-003") {
  # Base URL for operational NDFD GRIB2 data
  base_url <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.expr/DF.gr2/DC.ndfd/AR.conus/VP."
  
  # Snowfall element code for 72-hour sum
  file_url <- paste0(base_url, time_period, "/ds.snow.bin")
  local_file <- paste0("data/snowfall_72hr_", time_period, ".grib2")
  
  cat("Downloading snowfall data for", time_period, "...\n")
  
  tryCatch({
    download.file(file_url, local_file, mode = "wb", quiet = FALSE)
    cat("Download complete!\n")
    return(local_file)
  }, error = function(e) {
    cat("Error downloading file:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Function to read and process GRIB2 data
read_snowfall_grib <- function(file_path) {
  cat("Reading GRIB2 file...\n")
  
  # Read the GRIB2 file using terra
  snow_raster <- rast(file_path)
  
  # Reproject to WGS84 (EPSG:4326) if not already
  if (crs(snow_raster) != "EPSG:4326") {
    cat("Reprojecting to WGS84...\n")
    snow_raster <- project(snow_raster, "EPSG:4326")
  }
  
  # Convert from meters to inches (NDFD stores snowfall in meters)
  # 1 meter = 39.3701 inches
  snow_raster <- snow_raster * 39.3701
  
  return(snow_raster)
}

# Function to create snowfall map with custom legend
create_snowfall_map <- function(snow_raster, output_file = "snowfall_map.png") {
  cat("Creating snowfall map...\n")
  
  # Get US state boundaries in WGS84
  states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
  states <- st_transform(states, crs = 4326)  # Ensure WGS84
  
  # Convert raster to data frame for ggplot
  snow_df <- as.data.frame(snow_raster, xy = TRUE)
  colnames(snow_df) <- c("x", "y", "snowfall")
  
  # Remove NA values
  snow_df <- snow_df[!is.na(snow_df$snowfall), ]
  
  # Define custom breaks for snowfall amounts (in inches)
  breaks <- c(-0.01, 0.01, 1, 2, 4, 6, 8, 10, 15, 20, 30, Inf)
  labels <- c("0", "1 inch", "2", "4", "6", "8", "10", "15", "20", "30", "30+ in.")
  
  # Cut snowfall into categories
  snow_df$category <- cut(snow_df$snowfall, 
                          breaks = breaks, 
                          labels = labels,
                          include.lowest = TRUE,
                          right = FALSE)
  
  # Create the map
  p <- ggplot() +
    geom_raster(data = snow_df, aes(x = x, y = y, fill = category)) +
    geom_sf(data = states, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values = c(
        "#d3d3d3",  # Gray (0)
        "#e0f3ff",  # Very light blue (1 inch)
        "#b3d9ff",  # Light blue (2)
        "#80bfff",  # Medium light blue (4)
        "#4da6ff",  # Medium blue (6)
        "#1a8cff",  # Blue (8)
        "#0073e6",  # Dark blue (10)
        "#0059b3",  # Darker blue (15)
        "#004080",  # Very dark blue (20)
        "#002d59",  # Even darker blue (30)
        "#66001a"   # Dark red (30+ in.)
      ),
      name = NULL,
      drop = FALSE
    ) +
    coord_sf(crs = 4326, expand = FALSE) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.text = element_text(size = 10),
      legend.box.spacing = unit(0.5, "cm"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_blank(),
      panel.grid = element_line(color = "gray90")
    ) +
    guides(fill = guide_legend(title = NULL, label.position = "bottom", nrow = 1)) +
    labs(
      title = "NOAA NDFD 72-Hour Snowfall Forecast",
      subtitle = paste0("72-Hour Expected Snowfall Sum (inches) - Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
      caption = "Source: NOAA National Digital Forecast Database"
    )
  
  # Save the plot
  ggsave(output_file, p, width = 12, height = 8, dpi = 300)
  cat("Map saved to:", output_file, "\n")
  
  return(p)
}

# Main execution
main <- function() {
  cat("=== NDFD 72-Hour Snowfall Prediction Map Generator ===\n\n")
  
  # Download 72-hour snowfall data for days 1-3 (you can change to "004-007" for days 4-7)
  time_period <- "001-003"
  
  # Download 72-hour snowfall sum
  cat("Downloading 72-hour snowfall sum forecast...\n")
  snow_file <- download_snowfall_data(time_period)
  
  if (is.null(snow_file)) {
    stop("Failed to download snowfall data")
  }
  
  # Read the GRIB2 file
  snow_raster <- read_snowfall_grib(snow_file)
  
  # Print summary statistics
  cat("\nSnowfall Statistics (inches):\n")
  cat("Min:", min(values(snow_raster), na.rm = TRUE), "\n")
  cat("Max:", max(values(snow_raster), na.rm = TRUE), "\n")
  cat("Mean:", mean(values(snow_raster), na.rm = TRUE), "\n")
  cat("Median:", median(values(snow_raster), na.rm = TRUE), "\n\n")
  
  # Create the map
  map <- create_snowfall_map(snow_raster, paste0("snowfall_map_", time_period, ".png"))
  
  # Display the map
  print(map)
  
  cat("\n=== Complete! ===\n")
}

# Run the main function
main()
