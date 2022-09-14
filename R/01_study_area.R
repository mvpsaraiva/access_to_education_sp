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

# central_neighborhoods_file <- tar_read(central_neighborhoods_file)
load_central_neighborhoods <- function(central_neighborhoods_file) {
  data_sf <- st_read(central_neighborhoods_file) |> 
    st_transform(crs = 4326)
  
  return(data_sf)
}

get_census_tracts <- function() {
  ## IBGE code of São Paulo municipality: 3550308
  census_tracts_sf <- read_census_tract(code_tract = 3550308, simplified = FALSE) |> 
    st_transform(crs = 4326)
  
  return(census_tracts_sf)
}


# schools_geo <- tar_read(schools_geo)
build_districts_by_dre_table <- function(schools_geo) {
  ## collect districts and regions from schools information
  dre_districts_df <- schools_geo |> 
    mutate(cd_dist = sprintf("%02d", cd_dist),
           cd_dist_full = paste0("3550308", cd_dist)) |> 
    count(dre, nm_unidade_educacao, cd_dist, distr, cd_dist_full) |> 
    group_by(cd_dist, distr) |> 
    arrange(desc(n)) |> 
    slice(1) |> 
    select(-n) |> 
    ungroup()
  
  return(dre_districts_df)
}

# census_tracts <- tar_read(census_tracts)
# districts_by_dre_table <- tar_read(districts_by_dre_table)
build_districts <- function(census_tracts, districts_by_dre_table) {
  ## use custom function to dissolve census tracts into districts
  districts_sf <- dissolve_polygons(census_tracts, "code_district")
  
  ## gather lost columns with district information
  districts_data <- census_tracts |> 
    st_set_geometry(NULL) |> 
    select(code_state, code_muni, name_muni, code_district, name_district) |> 
    distinct()
  
  ## put extra information back into SF
  districts_sf <- districts_sf |> left_join(districts_data, by = "code_district")
  districts_sf <- districts_sf |> left_join(districts_by_dre_table, by = c("code_district" = "cd_dist_full"))
  
  ## return processed dataset
  return(districts_sf)  
}

# sme_districts <- tar_read(sme_districts)
build_regions <- function(sme_districts) {
  
  ## dissolve dre polygons using custom function
  sme_regions_sf <- dissolve_polygons(sme_districts, "dre")
  
  ## gather dre data
  sme_data <- sme_districts |> 
    st_set_geometry(NULL) |> 
    select(code_state, code_muni, name_muni, dre, nm_unidade_educacao) |> 
    distinct()
  
  ## join dre data back go dissolved SF
  sme_regions_sf <- left_join(sme_regions_sf, sme_data, by = "dre")
  
  return(sme_regions_sf)
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
  
  hexgrid_ext <- st_join(hexgrid, muni, largest = TRUE)
  
  return(hexgrid_ext)
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


# schools_geo <- tar_read(schools_geo)
# census_tracts <- tar_read(census_tracts)
# build_sme_sectors <- function(schools_geo, census_tracts) {
#   
#   schools_df <- schools_geo |> 
#     select(cd_escola, dre, nm_unidade_educacao, cd_dist, distr, setor, lat, lon)
# 
#   schools_sf <- st_as_sf(schools_df, coords = c("lon", "lat"), crs = 4326) 
#   
#   census_tracts
#   
#   
#   schools_geo |> 
#     count(dre, distr, setor, id_hex_10) 
#   
# }



