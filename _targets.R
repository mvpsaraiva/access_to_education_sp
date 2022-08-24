# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Package loading and overall setup ---------------------------------------

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Load other packages as needed. # nolint

# set r5r properties

options(java.parameters = '-Xmx32G')

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
  # library(igraph)
  library(r5r)
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
  tar_target(schools_census_raw_file, "../data_raw/schools_sp_geo2.csv", format = "file"),
  tar_target(rooms_raw_file, "../data_raw/BID_AMBIENTE_METRAGEM_2022.csv", format = "file"),
  tar_target(dem_file, "../network/bkp/topografia3_spo.tif"),
  tar_target(r5_folder, "../network/"),
  
  ## geographical boundaries and topography
  tar_target(sp_centroid, c(-46.63306176720343, -23.548164364465265)),
  tar_target(sp_bbox, build_central_bounding_box(sp_centroid, 1000)),
  tar_target(boundary_muni, get_boundary_muni()),
  tar_target(boundary_rmsp, get_boundary_rmsp()),
  tar_target(census_tracts, get_census_tracts()),
  
  tar_target(districts_by_dre_table, build_districts_by_dre_table(schools_geo)),
  tar_target(sme_districts, build_districts(census_tracts, districts_by_dre_table)),
  tar_target(sme_regions, build_regions(sme_districts)),

  tar_target(topography, read_topography(dem_file)),
  
  # tar_target(sme_sectors, build_sme_sectors(schools_geo, census_tracts)),
  
  ## hexagonal grid (Uber H3 system)
  tar_target(hexgrid_res_10, build_hex_grid(boundary_muni, 10)),
  tar_target(hexgrid_res_09, build_hex_grid(boundary_muni,  9)),
  tar_target(hexgrid_res_08, build_hex_grid(boundary_muni,  8)),

  ## travel times
  tar_target(travel_times_r10, compute_travel_times(r5_folder, hexgrid_res_10)),
  # tar_target(travel_times_r09, compute_travel_times(r5_folder, hexgrid_res_09)),
  # tar_target(travel_times_r08, compute_travel_times(r5_folder, hexgrid_res_08)),

  ## unitary accessibility
  tar_target(unitary_access_r10, calculate_unitary_access(travel_times_r10)),
  # tar_target(unitary_access_r09, calculate_unitary_access(travel_times_r09)),
  # tar_target(unitary_access_r08, calculate_unitary_access(travel_times_r08)),

  ## process students data  
  tar_target(students_raw, load_students_base_data(students_raw_file)),
  tar_target(students_socio, load_students_socio_data(students_raw, students_socio_file)),
  tar_target(students_geo, load_students_geo_data(students_socio, students_geo_file, boundary_rmsp)),
  tar_target(students_processed, recode_students_data(students_geo)),
  
  ## process schools and rooms data
  tar_target(schools_raw, load_schools(schools_raw_file)),
  tar_target(schools_with_size, add_students_rooms_to_schools(schools_raw, students_processed, rooms_fixed)),
  tar_target(schools_geo, add_h3_to_schools(schools_with_size)),
  
  tar_target(schools_census_raw, load_schools_census(schools_census_raw_file)),
  
  # tar_target(schools_with_students, populate_schools(schools_geo, students_processed)),
  tar_target(rooms_geo, load_rooms(rooms_raw_file)),
  tar_target(rooms_fixed, fix_room_capacity(rooms_geo)),
  
  ## 
  # tar_target(hexgrid, populate_hexgrid(hexgrid_res_10, students_processed, schools_with_students) ),
  
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
  tar_target(figure_distance_to_school, create_plot_distance_to_school(students_processed), format = "file"),
  
  ## Report 03 targets ----------------------------------------------------
  tar_target(figure_unitary_access, create_map_unitary_access(unitary_access_r10, hexgrid_res_10), format = "file"),
  tar_target(figure_regions_districts, create_map_regions_districts(sme_districts), format = "file"),
  tar_target(plot_problematic_rooms, create_plot_problematic_rooms(rooms_geo), format = "file"),
  tar_target(plot_fixed_rooms, create_plot_fixed_rooms(rooms_fixed), format = "file"),
  tar_target(plot_fixed_rooms_histogram, create_plot_fixed_rooms_histogram(rooms_fixed), format = "file")
  
)


