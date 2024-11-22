

# Using the terra package to align rasters (same resolution and extent) using resampling
align_raster <- function(input_raster, 
                         target_raster, 
                         method = "bilinear", output_path = NULL) {
  
  # Check the chosen method
  if (!method %in% c("bilinear", "near")) {
    stop("Invalid method. Use 'bilinear' for continuous data or 'near' for categorical data.")
  }
  
  # Resample the input raster to match the target raster
  aligned_raster <- resample(input_raster, target_raster, method = method)
  
  # Save the output raster if output_path is provided
  if (!is.null(output_path)) {
    writeRaster(aligned_raster, output_path, overwrite = TRUE)
    message("Aligned raster saved to: ", output_path)
  }
  
  # Return the aligned raster
  return(aligned_raster)
}