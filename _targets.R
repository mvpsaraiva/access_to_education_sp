# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Package loading and overall setup ---------------------------------------

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# library(tarchetypes) # Load other packages as needed. # nolint

# Load additional packages
suppressPackageStartupMessages({
  library(geobr)
  library(h3jsr)
  library(ggspatial)
  library(raster)
  library(sf)
  library(data.table)
  library(tidyverse)
  library(patchwork)
  library(writexl)
  library(scales)
  library(lubridate)
  library(viridis)
  library(ggsci)
  library(igraph)
  library(edgebundle)
})

# Set target options:
tar_option_set(
  packages = c("tidyverse"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint


# Build targets -----------------------------------------------------------


# Replace the target list below with your own:
list(
  
  ## Data processing targets ----------------------------------------------

  ## raw data files location
  tar_target(students_raw_file, "../data_raw/BID_RELACAO_ALUNOS_2022.csv", format = "file"),
  tar_target(students_geo_file, "../data_raw/BID_ALUNOS_COORDENADA_2022.csv", format = "file"),
  tar_target(students_socio_file, "../data_raw/BID_INFORMACOES_SOCIOECONOMICAS_2022.csv", format = "file"),
  tar_target(schools_raw_file, "../data_raw/BID_CADASTRO_ESCOLA_2022.csv", format = "file"),
  tar_target(rooms_raw_file, "../data_raw/BID_AMBIENTE_METRAGEM_2022.csv", format = "file"),
  tar_target(dem_file, "../data/network/topografia3_spo.tif"),
  
  ## geographical boundaries and topography
  tar_target(sp_centroid, c(-46.63306176720343, -23.548164364465265)),
  tar_target(sp_bbox, build_central_bounding_box(sp_centroid, 1000)),
  tar_target(boundary_muni, get_boundary_muni()),
  tar_target(boundary_rmsp, get_boundary_rmsp()),
  tar_target(census_tracts, get_census_tracts()),
  tar_target(topography, read_topography(dem_file)),
  
  ## hexagonal grid (Uber H3 system)
  tar_target(hexgrid_res_08, build_hex_grid(boundary_muni,  8)),
  tar_target(hexgrid_res_09, build_hex_grid(boundary_muni,  9)),
  tar_target(hexgrid_res_10, build_hex_grid(boundary_muni, 10)),
  
  ## process students data  
  tar_target(students_raw, load_students_base_data(students_raw_file)),
  tar_target(students_socio, load_students_socio_data(students_raw, students_socio_file)),
  tar_target(students_geo, load_students_geo_data(students_socio, students_geo_file, boundary_rmsp)),
  tar_target(students_processed, recode_students_data(students_geo)),
  
  ## process schools and rooms data
  tar_target(schools_geo, load_schools(schools_raw_file)),
  tar_target(rooms_geo, load_rooms(rooms_raw_file)),
  

  ## Report 02 targets ----------------------------------------------------

  ## figures section 1 - study area
  tar_target(figure_study_area, create_map_study_area(boundary_muni, topography), format = "file"),
  tar_target(figure_census_tracts, create_map_census_tracts(census_tracts, sp_bbox), format = "file"),
  tar_target(figure_hexgrid, create_map_hexgrid(hexgrid_res_08, hexgrid_res_09, hexgrid_res_10, sp_bbox), format = "file"),
  
  ## tables section 2 - students
  tar_target(table_student_count, create_table_student_count(students_raw, students_socio, students_processed), format = "file"),
  tar_target(table_students_by_stage, create_table_students_by_stage(students_processed), format = "file"),
  tar_target(table_students_by_race, create_table_students_by_race(students_processed), format = "file"),
  tar_target(table_students_by_income, create_table_students_by_income(students_processed), format = "file"),
  
  ## figures section 2 - students
  tar_target(figure_students_count, create_map_students_count(students_processed, boundary_muni, hexgrid_res_08)),
  tar_target(figure_students_by_race, create_map_students_by_race(students_processed, boundary_muni, hexgrid_res_08)),
  tar_target(figure_students_by_income, create_map_students_by_income(students_processed, boundary_muni, hexgrid_res_08)),
  tar_target(figure_students_by_teg, create_map_students_by_teg(students_processed, boundary_muni, hexgrid_res_08)),

  ## tables section 3 - schools and rooms
  tar_target(table_schools_count, create_table_schools_count(schools_geo), format = "file"),
  tar_target(table_rooms_count, create_table_rooms_count(rooms_geo), format = "file"),
  
  ## figures section 3 - schools and rooms
  tar_target(figure_schools_count, create_map_schools_count(schools_geo, boundary_muni, hexgrid_res_08)),
  
  ## figures section 4 - flows
  tar_target(figure_distance_to_school, create_plot_distance_to_school(students_processed), format = "file")
  
  ## Report 03 targets ----------------------------------------------------
  
)


