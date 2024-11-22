### A function to smooth a covariate using a circular moving window leveraging functionality from terra package in R -- beware that this may take a very long time if focal window size is large and raster resolution is small 
smooth_covariate <- function(input_raster=input_raster,
                             focal_window_size=100,
                             out_raster_name = "output",
                             save_output = FALSE,
                             output_path = "path"){
  
  # Create the focal weight matrix
  foc_mat <- focalMat(input_raster, focal_window_size, "circle", fillNA = TRUE)
  
  # Smooth the raster by calculating mean value within the focal window
  smoothed <- focal(input_raster, w = foc_mat, fun = "mean", na.policy = "all", na.rm = TRUE)
  
  # Assign a name to the output raster
  names(smoothed) <- out_raster_name
  
  # Optionally save the raster
  if (save_output) {
    writeRaster(smoothed, output_path, overwrite = TRUE)
  }
  
  return(smoothed)
}