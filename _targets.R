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
  library(r5r)
  library(accessibility)
})

# Set target options:
tar_option_set(
  packages = c("tidyverse"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")
options(scipen=10000)

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint


# targets -----------------------------------------------------------------


# Replace the target list below with your own:
list(
  
  ## Data processing targets ----------------------------------------------

  ### raw data files location ---------
  tar_target(students_raw_file, "../data_raw/BID_RELACAO_ALUNOS_2022.csv", format = "file"),
  tar_target(students_geo_file, "../data_raw/BID_ALUNOS_COORDENADA_2022.csv", format = "file"),
  tar_target(students_socio_file, "../data_raw/BID_INFORMACOES_SOCIOECONOMICAS_2022.csv", format = "file"),
  
  tar_target(schools_raw_file, "../data_raw/BID_CADASTRO_ESCOLA_2022.csv", format = "file"),
  
  tar_target(schools_census_raw_folder, "../data_raw/censo_escolar/"),
  tar_target(schools_census_raw_file, "../data_raw/censo_escolar/microdados_ed_basica_2021.csv", format = "file"),
  tar_target(schools_census_geo_file, "../data_raw/schools_sp_geo2.csv", format = "file"),
  
  tar_target(rooms_raw_file, "../data_raw/BID_AMBIENTE_METRAGEM_2022.csv", format = "file"),
  
  tar_target(state_schools_geo_file, "../data_raw/dados_estaduais/Endereço Escolas.csv"),
  tar_target(state_enrollments_raw_file, "../data_raw/dados_estaduais/Matriculas_por_aluno_2021.csv"),
  
  tar_target(pop_growth_estimates_raw_file, "../data_raw/estimativas_populacionais_seade/pop_idade_escolar_2000a2050_msp.csv"),
  tar_target(dem_file, "../network/bkp/topografia3_spo.tif", format = "file"),
  tar_target(r5_folder, "../network/", format = "file"),
  
  ### geographical boundaries and topography ---------
  tar_target(sp_centroid, c(-46.63306176720343, -23.548164364465265)),
  tar_target(sp_bbox, build_central_bounding_box(sp_centroid, 1000)),
  tar_target(boundary_muni, get_boundary_muni()),
  tar_target(boundary_rmsp, get_boundary_rmsp()),
  tar_target(census_tracts, get_census_tracts()),

  tar_target(central_neighborhoods_file, "../data_raw/bairros/bairros_centro_2.gpkg", format = "file"),
  
  tar_target(districts_by_dre_table, build_districts_by_dre_table(schools_geo)),
  tar_target(sme_districts, build_districts(census_tracts, districts_by_dre_table)),
  tar_target(sme_regions, build_regions(sme_districts)),
  
  tar_target(central_neighborhoods, load_central_neighborhoods(central_neighborhoods_file)),

  tar_target(topography, read_topography(dem_file)),
  
  ### hexagonal grid (Uber H3 system) ---------
  tar_target(hexgrid_res_10, build_hex_grid(sme_districts, 10)),
  tar_target(hexgrid_res_09, build_hex_grid(sme_districts,  9)),
  tar_target(hexgrid_res_08, build_hex_grid(sme_districts,  8)),

  ### travel times ---------
  tar_target(travel_times_r10, compute_travel_times(r5_folder, hexgrid_res_10)),
  # tar_target(travel_times_r09, compute_travel_times(r5_folder, hexgrid_res_09)),
  # tar_target(travel_times_r08, compute_travel_times(r5_folder, hexgrid_res_08)),

  ### unitary and tmi accessibility
  tar_target(unitary_access_r10, calculate_unitary_access(travel_times_r10)),
  tar_target(tmi_r10, calculate_tmi(travel_times_r10, schools_geo)),

  ### process students data ---------
  tar_target(students_raw, load_students_base_data(students_raw_file)),
  tar_target(students_socio, load_students_socio_data(students_raw, students_socio_file)),
  tar_target(students_geo, load_students_geo_data(students_socio, students_geo_file, boundary_rmsp)),
  tar_target(students_processed, recode_students_data(students_geo)),
  
  tar_target(students_flows, build_student_flows_with_times(students_processed, schools_geo, travel_times_r10)),
  tar_target(flows_per_district, build_student_flows_per_district(students_flows, hexgrid_res_10)),
  
  ### process schools and rooms data ---------
  tar_target(schools_raw, load_schools(schools_raw_file)),
  tar_target(schools_geo, add_h3_to_schools(schools_raw)),
  tar_target(schools_enrollments, add_enrollments_to_schools(schools_geo, students_processed)),

  ### process state schools and students ---------
  tar_target(state_schools_geo, load_state_schools(state_schools_geo_file)),
  tar_target(state_students_raw, load_state_students(state_enrollments_raw_file, state_schools_geo)),
  # tar_target(state_students_geo, load_state_students_geo_data(state_students_raw)),
  tar_target(state_schools_enrollments, add_state_enrollments_to_schools(state_schools_geo, state_students_raw)),
  
  tar_target(rooms_geo, load_rooms(rooms_raw_file)),
  tar_target(rooms_fixed, fix_room_capacity(rooms_geo)),
  
  ### populate hexgrid with students and school enrollments ---------
  tar_target(hexgrid_students, add_students_to_grid(hexgrid_res_10, students_processed)),
  tar_target(hexgrid_enrollments, add_enrollments_to_grid(hexgrid_res_10, schools_enrollments)),
  
  ### population growth estimates by SEADE ---------
  tar_target(pop_growth_estimates_seade, load_pop_growth_estimates_by_district_seade(pop_growth_estimates_raw_file)),
  tar_target(hexgrid_students_future_seade, estimate_future_population_seade(hexgrid_students, pop_growth_estimates_seade)),

  ### population growth estimates from School Census ---------
  tar_target(enrollments_by_year, load_enrollments_from_census(schools_census_raw_folder, sme_districts)),
  tar_target(pop_growth_estimates_census, estimate_growth_by_district_census(enrollments_by_year)),
  tar_target(hexgrid_students_future_census, estimate_future_population_census(hexgrid_students, pop_growth_estimates_census)),
  
  ## Report 02 targets ----------------------------------------------------

  ### figures section 1 - study area ----------
  tar_target(figure_study_area, create_map_study_area(boundary_muni, topography), format = "file"),
  tar_target(figure_census_tracts, create_map_census_tracts(census_tracts, sp_bbox), format = "file"),
  tar_target(figure_hexgrid, create_map_hexgrid(hexgrid_res_08, hexgrid_res_09, hexgrid_res_10, sp_bbox), format = "file"),
  
  ### tables section 2 - students ----------
  tar_target(table_student_count, create_table_student_count(students_raw, students_socio, students_processed), format = "file"),
  tar_target(table_students_by_stage, create_table_students_by_stage(students_processed), format = "file"),
  tar_target(table_students_by_race, create_table_students_by_race(students_processed), format = "file"),
  tar_target(table_students_by_income, create_table_students_by_income(students_processed), format = "file"),
  
  ### figures section 2 - students ----------
  tar_target(figure_students_count, create_map_students_count(students_processed, boundary_muni, hexgrid_res_08)),
  tar_target(figure_students_by_race, create_map_students_by_race(students_processed, boundary_muni, hexgrid_res_08)),
  tar_target(figure_students_by_income, create_map_students_by_income(students_processed, boundary_muni, hexgrid_res_08)),
  tar_target(figure_students_by_teg, create_map_students_by_teg(students_processed, boundary_muni, hexgrid_res_08)),

  ### tables section 3 - schools and rooms ----------
  tar_target(table_schools_count, create_table_schools_count(schools_geo), format = "file"),
  tar_target(table_rooms_count, create_table_rooms_count(rooms_geo), format = "file"),
  
  ### figures section 3 - schools and rooms ----------
  tar_target(figure_schools_count, create_map_schools_count(schools_geo, boundary_muni, hexgrid_res_08)),
  
  ### figures section 4 - flows ----------
  tar_target(figure_distance_to_school, create_plot_distance_to_school(students_processed), format = "file"),
  
  ## Report 03 targets ----------------------------------------------------
  tar_target(figure_unitary_access, create_map_unitary_access(unitary_access_r10, hexgrid_res_10), format = "file"),
  tar_target(figure_travel_times, create_map_travel_times(unitary_access_r10, tmi_r10, hexgrid_res_10, boundary_muni), format = "file"),
  tar_target(figure_regions_districts, create_map_regions_districts(sme_districts), format = "file"),
  tar_target(figure_problematic_rooms, create_plot_problematic_rooms(rooms_geo), format = "file"),
  tar_target(figure_fixed_rooms, create_plot_fixed_rooms(rooms_fixed), format = "file"),
  tar_target(figure_fixed_rooms_histogram, create_plot_fixed_rooms_histogram(rooms_fixed), format = "file"),
  
  tar_target(figure_state_schools, create_map_state_schools(state_schools_enrollments, boundary_muni, hexgrid_res_08), format = "file"),
  tar_target(figure_pop_growth, create_map_pop_growth(sme_districts, pop_growth_estimates_seade, boundary_muni), format = "file"),
  tar_target(figure_pop_growth_estimates, create_map_pop_growth_estimates(hexgrid_students_future_seade, boundary_muni, hexgrid_res_08), format = "file"),
  tar_target(figure_central_neighborhoods, create_map_central_neighborhoods(central_neighborhoods), format = "file"),
  
  tar_target(table_state_enrollments, create_table_state_enrollments(state_schools_enrollments), format = "file")

)



