# Students ----------------------------------------------------------------


# raw_students_file <- tar_read(students_raw_file)
load_students_base_data <- function(raw_students_file) {

  ## carregar csv com dados dos alunos
  alunos_df <- read_delim(raw_students_file, delim = "|", col_types = cols(.default = "c"), locale = locale(encoding = "Latin1")) |> 
    dplyr::select(CD_ALUNO, CD_ESCOLA, NOMESC, CD_TURMA_ESCOLA, SG_ETAPA, SG_SERIE_ENSINO,
                  DT_NASCIMENTO_ALUNO, CD_SEXO_ALUNO, TP_RACA_COR, starts_with("DEF"),
                  CD_NACIONALIDADE_ALUNO, NM_PAIS_MEC, NM_MUNICIPIO_NASC, MUN_ALU, SG_UF,
                  QT_DISTANCIA, QT_DISTANCIA_CARRO, TEG)
  
  ## remover instâncias duplicadas
  alunos_df <- distinct(alunos_df)
  
  ## filtrar por etapas de ensino (Infantil e Fundamental)
  etapas <- c("ENS FUND9A", "FUND ESP 9", "ED INF", "ED INF ESP")
  alunos_filtered_df <- alunos_df %>% filter(SG_ETAPA %in% etapas)
  
  # recodificar colunas SG_ETAPA, SG_SERIE_ENSINO e SG_RACA_COR
  # converter o campo DT_NASCIMENTO_ALUNO para data
  # calcular idade
  # converter distâncias para numérico
  alunos_filtered_df <- alunos_filtered_df %>%
    mutate(DT_NASCIMENTO_ALUNO = as.POSIXct(DT_NASCIMENTO_ALUNO, format = "%Y-%m-%d %H:%M:%OS"),
           IDADE = trunc((DT_NASCIMENTO_ALUNO %--% ymd("2022-03-01")) / years(1)),
           QT_DISTANCIA = as.numeric(QT_DISTANCIA),
           QT_DISTANCIA_CARRO = as.numeric(QT_DISTANCIA_CARRO))
  
  alunos_clean_df <- janitor::clean_names(alunos_filtered_df)
  
  return(alunos_clean_df)
}

# students_data <- tar_read(students_base)
# socio_file <- tar_read(students_socio_file)
load_students_socio_data <- function(students_data, socio_file) {
  # condições socio economicas dos alunos
  socio_df <- read_delim(socio_file, delim = "|", col_types = cols(.default = "c"), locale = locale(encoding = "Latin1")) |> 
    distinct() |> 
    janitor::clean_names()
  
  alunos_socio_df <- students_data %>% left_join(socio_df, by = "cd_aluno") %>%
    replace_na(list(bf = "NAO", faixa_renda = "NAO INFORMADA")) 
  
  return(alunos_socio_df)
}

# students_data <- tar_read(students_socio)
# geo_file <- tar_read(students_geo_file)
# muni <- tar_read(boundary_muni)
# rmsp <- tar_read(boundary_rmsp)
load_students_geo_data <- function(students_data, geo_file, rmsp) {
  # coordenadas geográficas das residêcias dos alunos
  alunos_geo_df <- read_delim(geo_file, delim = "|", col_types = "cdd",
                              locale = locale(encoding = "Latin1", decimal_mark = ",", grouping_mark = ".")) |> 
    drop_na() |> 
    distinct()
  
  colnames(alunos_geo_df) <- c("cd_aluno", "lon", "lat")
  
  # converter para simple features
  alunos_geo_sf <- alunos_geo_df |> 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # encontrar hexágonos H3 de cada aluno
  hex_res8 <- h3jsr::point_to_h3(alunos_geo_sf, res = 8)
  hex_res9 <- h3jsr::point_to_h3(alunos_geo_sf, res = 9)
  hex_res10 <- h3jsr::point_to_h3(alunos_geo_sf, res = 10)
  
  ## identificar alunos localizados fora da RMSP
  alunos_intersect <- st_within(alunos_geo_sf, rmsp, sparse = FALSE)
  
  # encontrar a posição de São Paulo na lista de municípios
  spo_index <- match("São Paulo", rmsp$name_muni)
  alunos_dentro_muni <- alunos_intersect[, spo_index]  # alunos dentro município de São Paulo
  alunos_dentro_rmsp <- rowSums(alunos_intersect) >= 1 # alunos dentro de algum dos municípios da RM
  
  # identificar alunos localizados dentro do município e dentro da RM
  alunos_geo_df$dentro_muni <- alunos_dentro_muni
  alunos_geo_df$dentro_rm <- alunos_dentro_rmsp
  
  # atribuir hex id a cada aluno
  alunos_geo_df$id_hex_08 <- hex_res8
  alunos_geo_df$id_hex_09 <- hex_res9
  alunos_geo_df$id_hex_10 <- hex_res10
  
  # remover duplicados e localizados fora da RM
  alunos_geo_df <- alunos_geo_df |> 
    filter(dentro_rm == TRUE) |> 
    group_by(cd_aluno) |> 
    arrange(desc(dentro_muni)) |> 
    summarise(across(.cols = lon:id_hex_10, first), .groups = "drop") 
  
  alunos_socio_geo_df <- students_data %>% inner_join(alunos_geo_df, by = "cd_aluno")  
  
  return(alunos_socio_geo_df)
}


# students_data <- tar_read(students_geo)
recode_students_data <- function(students_data) {
  
  # recodificar colunas SG_ETAPA, SG_SERIE_ENSINO e SG_RACA_COR
  # converter o campo DT_NASCIMENTO_ALUNO para data
  # calcular idade
  # converter distâncias para numérico
  students_recoded <- students_data %>%
    mutate(sg_etapa = recode(sg_etapa,
                             "ENS FUND9A" = "FUNDAMENTAL", 
                             "FUND ESP 9" = "FUNDAMENTAL ESPECIAL", 
                             "ED INF"     = "INFANTIL", 
                             "ED INF ESP" = "INFANTIL ESPECIAL"),
           sg_serie_ensino = recode(sg_serie_ensino,
                                    "1º ESC PARTIC BI"   = "BERCARIO I",
                                    "2ºESC PARTIC BII"   = "BERCARIO II",  
                                    "3ºESC PARTIC MGI"   = "MATERNAL I",
                                    "4ºESC PARTIC MGII"  = "MATERNAL II",
                                    "5ºESC PARTIC PRE I" = "PRE I",
                                    "6ºESC PARTIC PREII" = "PRE II",
                                    "BERC I"             = "BERCARIO I",
                                    "BERC II"            = "BERCARIO II",
                                    "BI E D"             = "BERCARIO I",
                                    "BII E D"            = "BERCARIO II",
                                    "ESC PART INF UNIF"  = "INFANTIL UNIFICADO",
                                    "FUND I BILINGUE I"  = "FUNDAMENTAL BILINGUE",
                                    "INF LIBRAS EMEI"    = "INFANTIL LIBRAS",
                                    "INFANTIL UNI DIF"   = "INFANTIL UNIFICADO",
                                    "INFANTIL UNI FI II" = "INFANTIL UNIFICADO",
                                    "INFANTIL UNIFICADO" = "INFANTIL UNIFICADO",
                                    "MG I"               = "MATERNAL I",
                                    "MG I ED"            = "MATERNAL I",
                                    "MG II"              = "MATERNAL II",
                                    "MG II ED"           = "MATERNAL II",
                                    "MG UNIF"            = "MATERNAL UNIFICADO"),
           dc_raca_cor = recode(tp_raca_cor,
                                `1` = "BRANCA",
                                `2` = "PRETA",
                                `3` = "PARDA",
                                `4` = "AMARELA",
                                `5` = "INDIGENA",
                                `6` = "NAO INFORMADA",
                                `7` = "RECUSOU INFORMAR",
                                .missing = "NAO INFORMADA"))
  
  # criar fatores para as colunas relevantes, na ordem adequada para cada
  students_recoded <- students_recoded |> 
    mutate(sg_etapa = factor(sg_etapa,
                             levels = c("INFANTIL", "INFANTIL ESPECIAL",
                                        "FUNDAMENTAL", "FUNDAMENTAL ESPECIAL")),
           sg_serie_ensino = factor(sg_serie_ensino,
                                    levels = c("BERCARIO I",
                                               "BERCARIO II",
                                               "MATERNAL I",
                                               "MATERNAL II",
                                               "MATERNAL UNIFICADO",
                                               "PRE I",
                                               "PRE II",
                                               "INFANTIL UNIFICADO",
                                               "INFANTIL LIBRAS",
                                               "1º ANO",
                                               "2º ANO",
                                               "3º ANO",
                                               "4º ANO",
                                               "5º ANO",
                                               "6º ANO",
                                               "7º ANO",
                                               "8º ANO",
                                               "9º ANO",
                                               "FUNDAMENTAL BILINGUE")),
           dc_raca_cor = factor(dc_raca_cor,
                                levels = c("BRANCA",
                                           "PRETA",
                                           "PARDA",
                                           "AMARELA",
                                           "INDIGENA",
                                           "NAO INFORMADA",
                                           "RECUSOU INFORMAR")),
           faixa_renda = factor(faixa_renda,
                                levels = c("EXTREMA POBREZA",
                                           "POBREZA",
                                           "BAIXA RENDA",
                                           "ACIMA DE MEIO SALARIO MINIMO",
                                           "NAO INFORMADA"))
           )
  
  
  # limpar data.frame
  students_clean_df <- students_recoded %>%
    dplyr::select(cd_aluno, cd_escola, nomesc, cd_turma_escola, sg_etapa, sg_serie_ensino,
                  dt_nascimento_aluno, idade, cd_sexo_aluno, tp_raca_cor, dc_raca_cor, 
                  starts_with("def"), faixa_renda, bf, 
                  cd_nacionalidade_aluno, nm_pais_mec, nm_municipio_nasc, mun_alu, sg_uf,
                  qt_distancia, qt_distancia_carro, teg,
                  lon, lat, id_hex_08, id_hex_09, id_hex_10, dentro_muni)
  
  return(students_clean_df)
}

# Schools ----------------------------------------------------------------

# schools_file <- tar_read(schools_raw_file)
load_schools <- function(schools_file) {
  escolas_df <- read_delim(schools_file, delim = "|", 
                           locale = locale(encoding = "Latin1", decimal_mark = ",", grouping_mark = "."),
                           trim_ws = TRUE) %>%
    rename(lat = CD_COORDENADA_GEO_Y, lon = CD_COORDENADA_GEO_X) |> 
    mutate(REDE = replace_na(REDE, "NAO INFORMADO")) |> 
    janitor::clean_names()
  
  return(escolas_df)
}

# tar_load(schools_raw)
add_h3_to_schools <- function(schools_raw) {
  # converter para simple features com coordenadas
  escolas_sf <- schools_raw |> 
    select(cd_escola, lat, lon) |> 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # encontrar hexágonos H3 de cada aluno
  hex_res8 <- h3jsr::point_to_h3(escolas_sf, res = 8)
  hex_res9 <- h3jsr::point_to_h3(escolas_sf, res = 9)
  hex_res10 <- h3jsr::point_to_h3(escolas_sf, res = 10)
  
  # atribuir hex id a cada aluno
  schools_raw$id_hex_08 <- hex_res8
  schools_raw$id_hex_09 <- hex_res9
  schools_raw$id_hex_10 <- hex_res10
  
  return(schools_raw)
}

# tar_load(schools_geo)
# tar_load(students_processed)
add_enrollments_to_schools <- function(schools_geo, students_processed) {

  students_by_school <- students_processed |> 
    count(cd_escola, sg_etapa, sg_serie_ensino)
  
  schools_fill <- schools_geo |> 
    left_join(students_by_school, by = "cd_escola", fill = 0) |> 
    select(depadm, nomedep, rede, dre, nm_unidade_educacao, cd_dist, distr, setor,
           cd_escola, nomesc, sg_etapa, sg_serie_ensino, n_alunos = n, 
           lat, lon, id_hex_10)
  
  return(schools_fill)
  
}

# rooms_file <- tar_read(rooms_raw_file)
load_rooms <- function(rooms_file) {
  # ambientes (salas) disponíveis nas escolas
  ambientes_df <- read_delim(rooms_file, delim = "|", 
                             locale = locale(encoding = "Latin1"),
                             trim_ws = TRUE) |> 
    janitor::clean_names() |> 
    mutate(dc_tipo_ambiente = factor(dc_tipo_ambiente,
                                     levels = c("BERCARIO",
                                                "SALA DE AULA PROPRIA",
                                                "SALA DE ED. INFANTIL PROPRIA",
                                                "SALA DE ED. ESPECIAL PROPRIA",
                                                "SALA DE AULA ADAPTADA",
                                                "SALA DE ED. INFANTIL ADAPTADA",
                                                "SALA DE ED. ESPECIAL ADAPTADA",
                                                "SALA DE AULA OCIOSA")))
  
  return(ambientes_df)
}
  


# Populate HexGrid --------------------------------------------------------
# hexgrid <- tar_read(hexgrid_res_10)
# students_processed <- tar_read(students_processed)
add_students_to_grid <- function(hexgrid, students_processed) {
  
  students_by_hex <- students_processed |> 
    count(id_hex_10, sg_etapa, sg_serie_ensino, name = "n_estudantes") |> 
    drop_na()
  
  hexgrid_pop <- 
    hexgrid |> left_join(students_by_hex, by = c("h3_address" = "id_hex_10")) |> 
    drop_na()
}

# hexgrid <- tar_read(hexgrid_res_10)
# schools_enrollments <- tar_read(schools_enrollments)
add_enrollments_to_grid <- function(hexgrid, schools_enrollments) {
  
  enrollments_by_hex <- schools_enrollments |> 
    count(id_hex_10, sg_etapa, sg_serie_ensino, wt = n_alunos, name = "n_matriculas") |> 
    drop_na()
  
  hexgrid_mat <- 
    hexgrid |> left_join(enrollments_by_hex, by = c("h3_address" = "id_hex_10")) |> 
    drop_na()

}

# schools_geo <- tar_read(schools_geo)
# students_processed <- tar_read(students_processed)
# populate_schools <- function(schools_geo, students_processed) {
#   
#   students_processed |> 
#     select(cd_aluno, cd_escola, sg_etapa, sg_serie_ensino)
#   
#   
#   
# }


# schools_with_size |> 
#   mutate(rooms_na = is.na(qt_area_ambientes),
#          students_na = is.na(qt_alunos_adj)) |> 
#   count(nomedep, rooms_na, students_na)
# 
# schools_with_size |> 
#   ggplot(aes(qt_alunos, qt_alunos_adj)) +
#   geom_point() +
#   geom_abline()




