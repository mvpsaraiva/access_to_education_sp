get_boundary_muni <- function() {
  boundary_sf <- read_municipality(code_muni = 3550308, simplified = FALSE) |> 
    st_transform(crs = 4326)
  
  return(boundary_sf)
}

get_boundary_rmsp <- function() {
  boundary_rmsp_sf <- read_metro_area(simplified = FALSE) |> filter(name_metro == "RM São Paulo") |> 
    st_transform(crs = 4326)
  
  return(boundary_rmsp_sf)
}

get_census_tracts <- function() {
  census_tracts_sf <- read_census_tract(code_tract = 3550308, simplified = FALSE) |> 
    st_transform(crs = 4326)
  
  return(census_tracts_sf)
}

# file <- tar_read(dem_file)
read_topography <- function(file) {
  dem_rst <- raster(file) |> 
    raster::as.data.frame(xy=TRUE) |> 
    rename("elevation" = topografia3_spo)
  
  return(dem_rst)
}

# muni <- tar_read(boundary_muni)
# resolution = 8
build_hex_grid <- function(muni, resolution) {
  hexes <- h3jsr::polyfill(muni, res = resolution)
  
  hexgrid <- h3jsr::h3_to_polygon(hexes, simple = FALSE) |> 
    st_as_sf(crs = 4326)
  
  return(hexgrid)
}

# centroid <- tar_read(sp_centroid)
build_central_bounding_box <- function(centroid, size) {
  sp_centroid_sf <- h3jsr::point_to_h3(centroid, res = 9) |> 
    h3jsr::h3_to_point(simple = FALSE) |> 
    st_as_sf(crs = 4326)
  
  units(size) <- "m"
  sp_buffer <- st_buffer(sp_centroid_sf, dist = size)
  bbox_buffer <- st_bbox(sp_buffer)
  
  return(bbox_buffer)
}


