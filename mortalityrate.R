#' @title Read SEDAC Global Infant Mortality Rates
#' @description Reads the SEDAC Global Infant Mortality Rates dataset from a GeoTIFF file.
#' @details Handles quirks like negative values (coded as -9999) by setting them to NA.
#' @return A `terra::rast` object representing infant mortality rates.

read_sedac_global_infant_mortality_rates <- function() {
  # Hardcoded file path for testing purposes

  ## URL not avaliable
  #https://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-infant-mortality-rates-v2-01/data-download
  fname <- "povmap-global-subnational-infant-mortality-rates-v2-01-geotiff-2/povmap_global_subnational_infant_mortality_rates_v2_01.tif"
  # Read the GeoTIFF file
  r <- terra::rast(fname)

  # Handle negative values by setting them to NA
  r <- terra::clamp(r, 0, values = FALSE)

  # Return the raster object
  return(r)
}

#' @title Generate SEDAC Global Infant Mortality Rates Raster
#' @description Processes the SEDAC dataset to ensure compatibility with PRIO Grid.
#' @param data A `terra::rast` object (default: result of `read_sedac_global_infant_mortality_rates`).
#' @return A `terra::rast` object compatible with PRIO Grid.

gen_sedac_global_infant_mortality_rates <- function(data = read_sedac_global_infant_mortality_rates()) {
  pg <- prio_blank_grid()

  # Ensure CRS matches between the dataset and PRIO Grid
  if (terra::crs(data) != terra::crs(pg)) {
    data <- terra::project(data, terra::crs(pg))
  }

  # Ensure extents match between the dataset and PRIO Grid
  if (terra::ext(data) != terra::ext(pg)) {
    tmp <- terra::rast(terra::ext(pg),
                       crs = terra::crs(data),
                       ncol = terra::ncol(pg),
                       nrow = terra::nrow(pg))
    data <- terra::resample(data, tmp, method = "near", threads = TRUE)
  }

  # Resample resolution to match PRIO Grid
  data <- terra::resample(data, pg, method = "near", threads = TRUE)

  # Name the raster variable for clarity
  names(data) <- "sedac_global_infant_mortality_rate"

  # Return the processed raster
  return(data)
}

# --- TEST ---

# Time the read function
start_read <- Sys.time()
r_data <- read_sedac_global_infant_mortality_rates()
time_read <- Sys.time() - start_read
print(paste("Time to read data:", time_read))

# Time the processing function
start_gen <- Sys.time()
r_processed <- gen_sedac_global_infant_mortality_rates(data = r_data)
time_gen <- Sys.time() - start_gen
print(paste("Time to process data:", time_gen))

# Load the PRIO Grid template
pg <- prio_blank_grid()

# Convert the processed raster to a data.frame
df <- as.data.frame(c(pg, r_processed))

# Plot
plot(r_processed)

