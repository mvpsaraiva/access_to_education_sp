# Compute travel times at finer resolution --------------------------------

# r5_folder <- tar_read(r5_folder)
# hexgrid <- tar_read(hexgrid_res_10)
compute_travel_times <- function(r5_folder, hexgrid) {

  # load r5r
  r5r_core <- setup_r5(r5_folder)
  
  # convert hexagons to points
  points <- h3_to_point(hexgrid$h3_address, simple = FALSE)
  colnames(points) <- c("id", "h3_resolution", "geometry")
  
  # filter points for testing
  # points <- head(points, 1000)

  # compute travel times
  ttm <- travel_time_matrix(r5r_core, 
                            origins = points,
                            destinations = points,
                            mode = "WALK",
                            max_walk_time = 45,
                            max_trip_duration = 45)  
  colnames(ttm) <- c("from_id", "to_id", "travel_time")
  
  # cleanup memory
  stop_r5(r5r_core)
  
  return(ttm)
}


# ttm <- tar_read(travel_times_r08)
calculate_unitary_access <- function(ttm) {
  # count how many cells are accessible from each origin
  access <- ttm[, .N, by = from_id]
  colnames(access) <- c("id", "accessibility")
  
  return(access)
}


# access_list <- list(tar_read(unitary_access_r08), 
#                     tar_read(unitary_access_r09), 
#                     tar_read(unitary_access_r10))
bind_unitary_access <- function(access_list) {

  # bind data.frames and add resolution column    
  df <- rbindlist(access_list)
  resolution <- h3jsr::get_res(df$id)
  df$resolution <- resolution
  
  return(df)
}




