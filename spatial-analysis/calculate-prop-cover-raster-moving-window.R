
### A function to calculate proportional cover using a moving window passed over a binary raster using the terra package. Be aware that processing time may increase dramatically as moving window size increases. Original code developed by Amy Collins and Madi Standen and modified by Patrick Freeman. 
foc_prop_cover <- function(input_binary_raster,
                           focal_window_size = 100,
                           out_raster_name = "output_proportion_cover",
                           save_output = FALSE,
                           output_path = "proportional_cover.tif") {
  
  # Validate input
  if (!inherits(input_binary_raster, "SpatRaster")) {
    stop("input_binary_raster must be a terra SpatRaster object.")
  }
  if (!all(unique(values(input_binary_raster)) %in% c(0, 1, NA))) {
    stop("input_binary_raster must be binary (0 and 1) with optional NA values.")
  }
  if (!grepl("metres", crs(input_binary_raster))) {
    stop("Ensure input_binary_raster has a projected CRS with units in meters.")
  }
  
  # Create the focal weight matrix (non-normalized binary mask)
  foc_mat <- focalMat(input_binary_raster, focal_window_size, "circle", fillNA = TRUE)
  foc_mat[!is.na(foc_mat)] <- 1  # Convert to binary mask
  
  # Calculate proportional cover
  perc_cover <- focal(input_binary_raster, w = foc_mat, fun = "sum", na.policy = "all", na.rm = TRUE) / sum(foc_mat, na.rm = TRUE)
  
  # Assign a name to the output raster
  names(perc_cover) <- out_raster_name
  
  # Optionally save the raster
  if (save_output) {
    writeRaster(perc_cover, output_path, overwrite = TRUE)
  }
  
  return(perc_cover)
}