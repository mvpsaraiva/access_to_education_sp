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

# flows_missing_df$id_hex |> unique() |> length()
# 
# points_origin <- left_join(flows_missing_df, points, by = c("id_hex" = "id")) |> 
#   rename(id = id_hex)
# 
# points_destination <- left_join(flows_missing_df, points, by = c("id_hex_escola" = "id")) |> 
#   rename(id = id_hex) 
# 
# ttm_dt <- detailed_itineraries(r5r_core,
#                                origins = points_origin,
#                                destinations = points_destination,
#                                mode = "WALK",
#                                max_walk_time = Inf,
#                                max_trip_duration = Inf,
#                                shortest_path = TRUE
#                               )

# ttm <- tar_read(travel_times_r08)
calculate_unitary_access <- function(ttm) {
  # count how many cells are accessible from each origin
  access <- ttm[, .N, by = from_id]
  colnames(access) <- c("id", "accessibility")
  
  return(access)
}

# ttm <- tar_read(travel_times_r10)
# schools_geo <- tar_read(schools_geo)
calculate_tmi <- function(ttm, schools_geo) {
  
  schools_by_hex <- schools_geo |> 
    count(id_hex_10, name = "escolas") |> 
    rename(id = id_hex_10)
  
  tmi <- accessibility::cost_to_closest(ttm, 
                                        land_use_data = schools_by_hex,
                                        opportunity = "escolas",
                                        travel_cost = "travel_time")
  tmi <- na.omit(tmi)
  
  
  return(tmi)
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

# students_processed <- tar_read(students_processed)
# schools_geo <- tar_read(schools_geo)
# ttm <- tar_read(travel_times_r10)
build_student_flows <- function(students_processed, schools_geo) {
  
  # preparação dos dados
  students_df <- students_processed |> 
    select(cd_aluno, cd_escola, nomesc, cd_turma_escola, 
           sg_etapa, sg_serie_ensino, dt_nascimento_aluno, idade, 
           cd_sexo_aluno, tp_raca_cor, dc_raca_cor,
           faixa_renda, bf, qt_distancia, qt_distancia_carro, teg,
           lat, lon, id_hex = id_hex_10)

  schools_df <- schools_geo |> 
    select(cd_escola, depadm, nomedep, dre, nm_unidade_educacao, 
           cd_dist, distr, 
           lat_escola = lat, lon_escola = lon, id_hex_escola = id_hex_10)
  
  flows_df <- left_join(students_df, schools_df, by = "cd_escola")
  
  return(flows_df)
}

# students_processed <- tar_read(students_processed)
# schools_geo <- tar_read(schools_geo)
# ttm <- tar_read(travel_times_r10)
build_student_flows_with_times <- function(students_processed, schools_geo, ttm) {
  
  students_processed |> 
    count(id_hex_10, sort = TRUE)
  
  schools_geo |> 
    count(id_hex_10, sort = TRUE)
  
  # preparação dos dados
  students_df <- students_processed |> 
    select(cd_aluno, cd_escola, nomesc, cd_turma_escola, 
           sg_etapa, sg_serie_ensino, dt_nascimento_aluno, idade, 
           cd_sexo_aluno, tp_raca_cor, dc_raca_cor,
           faixa_renda, bf, qt_distancia, qt_distancia_carro, teg,
           lat, lon, id_hex = id_hex_10)
  
  schools_df <- schools_geo |> 
    select(cd_escola, depadm, nomedep, dre, nm_unidade_educacao, 
           cd_dist, distr, 
           lat_escola = lat, lon_escola = lon, id_hex_escola = id_hex_10)
  
  flows_df <- left_join(students_df, schools_df, by = "cd_escola")
  
  # buscar tempos de viagem da matriz
  flows_merged_df <- left_join(flows_df, ttm, by = c("id_hex" = "from_id", "id_hex_escola" = "to_id"))
  
  return(flows_merged_df)
}

# students_flows <- tar_read(students_flows)
# hexgrid <- tar_read(hexgrid_res_10)
build_student_flows_per_district <- function(students_flows, hexgrid) {
  
  # remover geometria e converter para data.table (para velocidade)
  hexgrid <- st_set_geometry(hexgrid, NULL)
  setDT(hexgrid)
  
  # simplificar tabela de flows
  flows_df <- students_flows |> 
    select(hex_aluno = id_hex, hex_escola = id_hex_escola, cd_distrito_escola = cd_dist,  distrito_escola = distr) |> 
    setDT()

  flows_df[hexgrid, on = .(hex_aluno = h3_address), `:=`(cd_distrito_aluno = i.cd_dist,
                                                         distrito_aluno = i.distr)]

  flows_df[hexgrid, on = .(hex_escola = h3_address), `:=`(cd_distrito_escola2 = i.cd_dist,
                                                         distrito_escola2 = i.distr)]
  
  flows_df[, dentro_distrito_sme := fifelse(distrito_aluno == distrito_escola, "dentro", "fora")]
  flows_df[, dentro_distrito_hex := fifelse(distrito_aluno == distrito_escola2, "dentro", "fora")]
  
  flows_per_district_df <- flows_df |> 
    count(distrito_aluno, dentro_distrito_hex)  |> 
    pivot_wider(names_from = dentro_distrito_hex, values_from = n) |> 
    mutate(dentro_perc = dentro / (dentro + fora)) |> 
    select(-`NA`) |> 
    drop_na()
  
  return(flows_per_district_df)
}

