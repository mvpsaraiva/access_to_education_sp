#' Mapas da área de estudo
#' 
#' Script para gerar as figuras da seção 2 do relatório (área de estudo)


# setup -------------------------------------------------------------------
source("R/00_setup/00_setup.R")


# load data ---------------------------------------------------------------

boundary_sf <- read_rds("../data/geo/municipal_boundary.rds")
boundary_rmsp_sf <- read_rds("../data/geo/metropolitan_boundary.rds")
census_tracts_sf <- read_rds("../data/geo/census_tracts.rds")

dem_rst <- raster("../data/network/topografia3_spo.tif") |> 
  raster::as.data.frame(xy=TRUE) |> 
  rename("elevation" = topografia3_spo)

hexgrid_08_sf <- read_rds("../data/geo/hexgrid_08.rds")
hexgrid_09_sf <- read_rds("../data/geo/hexgrid_09.rds")
hexgrid_10_sf <- read_rds("../data/geo/hexgrid_10.rds")


# define centroid and build buffer ----------------------------------------

# centroide da área de estudo, para visualização de áreas menores
sp_centroid <- c(-46.63306176720343, -23.548164364465265)
sp_centroid_sf <- h3jsr::point_to_h3(sp_centroid, res = 9) |> 
  h3jsr::h3_to_point(simple = FALSE) |> 
  st_as_sf(crs = 4326)

buffer_dist <- 1500
units(buffer_dist) <- "m"
sp_buffer <- st_buffer(sp_centroid_sf, dist = buffer_dist)
bbox_buffer <- st_bbox(sp_buffer)

# figure 1a - municipality limits --------------------------------------------------

boundary_sf |> 
  ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 10) +
  geom_sf(fill = NA) 

dem_rst %>%
  ggplot( ) +
  geom_raster(aes(x=x, y=y, fill = elevation)) +
  geom_sf(data = boundary_sf, fill = NA)



# figura 2 - spatial units ------------------------------------------------

census_tracts_sf %>%
  ggplot( ) +
  annotation_map_tile(type = "cartolight", zoom = 14) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(bbox_buffer$xmin, bbox_buffer$xmax),
         ylim = c(bbox_buffer$ymin, bbox_buffer$ymax))

hexgrid_08_sf %>%
  ggplot( ) +
  annotation_map_tile(type = "cartolight", zoom = 14) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(bbox_buffer$xmin, bbox_buffer$xmax),
           ylim = c(bbox_buffer$ymin, bbox_buffer$ymax))

hexgrid_09_sf %>%
  ggplot( ) +
  annotation_map_tile(type = "cartolight", zoom = 14) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(bbox_buffer$xmin, bbox_buffer$xmax),
           ylim = c(bbox_buffer$ymin, bbox_buffer$ymax))

hexgrid_10_sf %>%
  ggplot( ) +
  annotation_map_tile(type = "cartolight", zoom = 14) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(bbox_buffer$xmin, bbox_buffer$xmax),
           ylim = c(bbox_buffer$ymin, bbox_buffer$ymax))

  