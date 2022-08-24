#' Dados espaciais da área de estudo
#' 
#' Script para baixar e organizar os dados espaciais da cidade de São Paulo,
#' como limites municipais, unidades espaciais, topografia e sistema viário.


# setup -------------------------------------------------------------------
v

# download boundaries -----------------------------------------------------

# São Paulo = 3550308
boundary_sf <- read_municipality(code_muni = 3550308, simplified = FALSE) 
census_tracts_sf <- read_census_tract(code_tract = 3550308, simplified = FALSE)
boundary_rmsp_sf <- read_metro_area(simplified = FALSE) |> filter(name_metro == "RM São Paulo")

mapview::mapview(boundary_sf)
mapview::mapview(census_tracts_sf)
mapview::mapview(boundary_rmsp_sf)

write_rds(boundary_sf, "../data/geo/municipal_boundary.rds")
write_rds(boundary_rmsp_sf, "../data/geo/metropolitan_boundary.rds")
write_rds(census_tracts_sf, "../data/geo/census_tracts.rds")

# hexagonal grid -----------------------------------------------------
hex_08 <- h3jsr::polyfill(boundary_sf, res = 8)
hex_09 <- h3jsr::polyfill(boundary_sf, res = 9)
hex_10 <- h3jsr::polyfill(boundary_sf, res = 10)

hexgrid_08_sf <- h3jsr::h3_to_polygon(hex_08, simple = FALSE)
hexgrid_09_sf <- h3jsr::h3_to_polygon(hex_09, simple = FALSE)
hexgrid_10_sf <- h3jsr::h3_to_polygon(hex_10, simple = FALSE)

mapview::mapview(hexgrid_08_sf)
mapview::mapview(hexgrid_09_sf)
mapview::mapview(hexgrid_10_sf)

write_rds(hexgrid_08_sf, "../data/geo/hexgrid_08.rds")
write_rds(hexgrid_09_sf, "../data/geo/hexgrid_09.rds")
write_rds(hexgrid_10_sf, "../data/geo/hexgrid_10.rds")
