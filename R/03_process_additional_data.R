
# Escolas Estaduais - Censo Escolar -------------------------------------------------------


# raw_file <- tar_read(schools_census_raw_file)
# geo_file <- tar_read(schools_census_geo_file)
load_state_schools_census <- function(raw_file, geo_file) {
  
  # ler dados do censo escolar
  escolas_df <- fread(raw_file, sep = ";", encoding = "Latin-1")
  
  ## área de estudo: São Paulo, SP
  escolas_df <- escolas_df[SG_UF == "SP" & NO_MUNICIPIO == "São Paulo"]

  # selecionar somente escolas estaduais com matriculas regulares de ensino infantil e fundamental
  escolas_df <- escolas_df[IN_REGULAR == 1 & TP_DEPENDENCIA == 2 & TP_SITUACAO_FUNCIONAMENTO == 1]
  escolas_df <- escolas_df[IN_INF == 1 | IN_FUND == 1]
  
  # selecionar apenas colunas relevantes
  escolas_df <- escolas_df[, .(TP_DEPENDENCIA, CO_DISTRITO, CO_ENTIDADE, NO_ENTIDADE,
                               DS_ENDERECO, NU_ENDERECO, DS_COMPLEMENTO, NO_BAIRRO, CO_CEP,
                               NU_DDD, NU_TELEFONE, 
                               CO_ORGAO_REGIONAL, CO_ESCOLA_SEDE_VINCULADA,
                               QT_SALAS_EXISTENTES, QT_SALAS_UTILIZADAS, QT_SALAS_UTILIZADAS_DENTRO, QT_SALAS_UTILIZADAS_FORA,
                               IN_INF, IN_INF_CRE, IN_INF_PRE, IN_FUND, IN_FUND_AI, IN_FUND_AF,
                               QT_MAT_INF, QT_MAT_INF_CRE, QT_MAT_INF_PRE, QT_MAT_FUND, QT_MAT_FUND_AI, QT_MAT_FUND_AF,
                               QT_MAT_BAS_BRANCA, QT_MAT_BAS_PRETA, QT_MAT_BAS_PARDA, QT_MAT_BAS_AMARELA, QT_MAT_BAS_INDIGENA, QT_MAT_BAS_ND,
                               QT_TUR_INF, QT_TUR_INF_CRE, QT_TUR_INF_PRE, QT_TUR_FUND, QT_TUR_FUND_AI, QT_TUR_FUND_AF)]

  # ler dados geolocalizados das escolas
  escolas_geo_df <- fread(geo_file, select = c("CO_ENTIDADE", "lat", "lon"))

  # corrigir coordenadas problemáticas de escolas 
  ## escola ANTONIETA DE SOUZA ALCANTARA
  escolas_geo_df[CO_ENTIDADE == 35902615, lat := -23.60700199844978]
  escolas_geo_df[CO_ENTIDADE == 35902615, lon := -46.43369440978187]
  
  # converter para simple features
  escolas_geo_sf <- escolas_geo_df |> 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # encontrar hexágonos H3 de cada aluno
  hex_res8 <- h3jsr::point_to_h3(escolas_geo_sf, res = 8)
  hex_res9 <- h3jsr::point_to_h3(escolas_geo_sf, res = 9)
  hex_res10 <- h3jsr::point_to_h3(escolas_geo_sf, res = 10)
  
  escolas_geo_df$id_hex_08 <- hex_res8
  escolas_geo_df$id_hex_09 <- hex_res9
  escolas_geo_df$id_hex_10 <- hex_res10
  
  # juntar escolas com coordenadas geográficas e ids dos hexágonos
  escolas_df[escolas_geo_df, on = "CO_ENTIDADE", 
             `:=`(lat = i.lat, lon = i.lon, 
                  id_hex_08 = i.id_hex_08,
                  id_hex_09 = i.id_hex_09,
                  id_hex_10 = i.id_hex_10)]


  # organizar nomes das colunas
  escolas_df <- janitor::clean_names(escolas_df)
  
  return(escolas_df)
}


# Escolas e Matrículas Estaduais - Secretaria Estadual de Educação ---------------------

# raw_file <- tar_read(state_schools_geo_file)
load_state_schools <- function(raw_file) {
  
  # carregar dados brutos das escolas
  schools_df <- fread(raw_file)
  
  # filtrar apenas escolas estaduais (EE) localizadas no município de São Paulo
  # o filtro EE remove escolas localizadas em hospitais e penitenciárias, entre outras
  # schools_df[MUN == "SAO PAULO"] |> count(TIPOESC)
  
  schools_filtered_df <- schools_df[MUN == "SAO PAULO" & 
                                      TIPOESC %in% c("EE",
                                                     "ÁREA DE ASSENTAMENTO",
                                                     "QUILOMBO")]
  
  schools_filtered_df <- schools_filtered_df[!(COD_ESC %in% c(1569, 3542, 4911))]
  
  # converter coordenadas geográficas para tipo numérico
  schools_filtered_df[, DS_LONGITUDE := str_replace(DS_LONGITUDE, ",", ".")]
  schools_filtered_df[, lon := as.double(DS_LONGITUDE)]
  schools_filtered_df[, DS_LONGITUDE := NULL]
  
  schools_filtered_df[, DS_LATITUDE := str_replace(DS_LATITUDE, ",", ".")]
  schools_filtered_df[, lat := as.double(DS_LATITUDE)]
  schools_filtered_df[, DS_LATITUDE := NULL]
  
  
  # encontrar hexágono H3 de cada escola
  escolas_sf <- schools_filtered_df |> 
    select(COD_ESC, lat, lon) |> 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # encontrar hexágonos H3 de cada aluno
  hex_res10 <- h3jsr::point_to_h3(escolas_sf, res = 10)
  
  # atribuir hex id a cada aluno
  schools_filtered_df$id_hex_10 <- hex_res10

  # renomear colunas
  schools_filtered_df <- janitor::clean_names(schools_filtered_df)
  
  return(schools_filtered_df)
}


# raw_file <- tar_read(state_enrollments_raw_file)
# schools <- tar_read(state_schools_geo)
load_state_students <- function(raw_file, schools) {
  # carregar dados brutos dos alunos
  students_df <- fread(raw_file)
  
  # corrigir escritas erradas do nome São Paulo
  students_df$CIDADE <- str_to_upper(students_df$CIDADE)
  students_df$CIDADE <- str_squish(students_df$CIDADE)
  
  spellings <- c(
    "SAO PAULO",
    "S PAULO",
    "SAO PAULOP",
    "SAO PAULOB",
    "SÃO PAULO - SP",
    "SAOP PAULO",
    "SAOPAULO",
    "SAO PAUCO",
    "SEO PAULO",
    "SAO PAOLO",
    "SAO PAULO S",
    "SAO PUALO",
    "SAO PAAULO",
    "SÃO PAULO SP",
    "SAO PAUKLO",
    "SAO PAUL O",
    "SA PAULO"  ,
    "SAO PAUL"    ,
    "SAO PAUALO"  ,
    "SAO PAUILO"  ,
    "SAO PAULO SP" ,
    "SÃOPAULO"   ,
    "SHO PAULO"   ,
    "SAO PALO",
    "SÃ0 PAULO",
    "SAO PRULO" ,
    "SAO PAULO - SP"    ,
    "SAO PAIULO" ,
    "SAO PAUULO"  ,
    "SÃO PULO" ,
    "SUO PAULO" ,
    "SÃO PAULO"  ,
    "SP" ,
    "SAO APULO",
    "SOA PAULO",
    "SÃO PAULO -SP"   ,
    "SXO PAULO"  ,
    "SAO PAILO"   ,
    "SPAULO" ,
    "SAO PAULO-SP" ,
    "SAO PAULOS",
    "SAO PAUO"  ,
    "S APULO" ,
    "SAO PAULA",
    "SAO PÁULO",
    "SAPO PAULO",
    "S.PAULO",
    "S. PAULO",
    "SAO PAUOO",
    "SAO APAULO",
    "SÇAO PAULO",
    "SÂO PAULO",
    "SAO PALUO" ,
    "SAP PAULO"  ,
    "SÃO PAULO - SPP" ,
    "S P"  ,
    "SZO PAULO"   ,
    "SAO AULO",
    "SÃO PAULO / SP" ,
    "SAO PAULO PAULO" ,
    "SASO PAULO"    ,
    "SAO PPAULO"   ,
    "SRO PAULO"    ,
    "SAO PAULOO"  ,
    "SÃO PAULO/SP" ,
    "SAO PLUO"    ,
    "SÃO APULO"   ,
    "SKO PAULO",
    "SÃO PAULOA",
    "S SÃO PAULO",
    "SAO PAULO CAPITAL"   ,
    "SSAO PAULO"   ,
    "SAOA PAULO"
  )    
  
  students_df[CIDADE %in% spellings, CIDADE := "SAO PAULO"]
  
  # corrigir escritas erradas da sigla SP
  students_df$SIGLA_END_UF <- str_to_upper(students_df$SIGLA_END_UF)
  
  # filtrar estudantes de 1o grau (ensino fundamental)
  # estudantes com matrícula regular ativa (FLAG_SIT_ALUNO == 0)
  # residentes na cidade de São Paulo / SP
  # matriculados em escolas estaduais localizadas no município de São Paulo
  students_filtered_df <- students_df[GRAU == 14 & 
                                        SERIE > 0 &
                                        FLAG_SIT_ALUNO == 0 &
                                        CIDADE == "SAO PAULO" &
                                        SIGLA_END_UF == "SP" &
                                        CD_ESCOLA %in% schools$cod_esc]
  
  
  # renomear colunas
  students_filtered_df <- janitor::clean_names(students_filtered_df)
  
  return(students_filtered_df)
}

# tar_load(state_schools_geo)
# tar_load(state_students_raw)
add_state_enrollments_to_schools <- function(state_schools_geo, state_students_raw) {
  
  students_by_school <- state_students_raw |> 
    count(cd_escola, grau, serie, name = "n_alunos") |> 
    mutate(sg_etapa = "FUNDAMENTAL", 
           sg_serie_ensino = paste0(serie, "º ANO")) |> 
    select(cd_escola, sg_etapa, sg_serie_ensino, n_alunos)
  

  schools_fill <- state_schools_geo |> 
    left_join(students_by_school, by = c("cod_esc" = "cd_escola"), fill = 0)

  return(schools_fill)
  
}


# Estimativas de Crescimento Populacional - SEADE -------------------------



# raw_file <- tar_read(pop_growth_estimates_raw_file)
load_pop_growth_estimates_by_district_seade <- function(raw_file) {
  
  # ler estimativas do SEADE
  pop_df <- fread(raw_file, sep = ";", encoding = "Latin-1", dec = ",") |>
    janitor::clean_names()
  
  # converter para formato longo
  pop_df <- pop_df |> 
    pivot_longer(cols = c(de_00_a_03_anos:de_15_a_17_anos, ate_17_anos, demais_idades),
                 names_to = "faixa_idade", values_to = "populacao") |> 
    select(cod_distr:ano, faixa_idade, populacao, populacao_total) |> 
    mutate(faixa_idade = recode(faixa_idade, "demais_idades" = "acima_de_17_anos"))

  # remover pontos (separadores de milhar) das colunas de população
  pop_df$populacao <- str_remove(pop_df$populacao, "\\.") |> as.integer()
  pop_df$populacao_total <- str_remove(pop_df$populacao_total, "\\.") |> as.integer()

  # calcular a proporção de cada grupo de idade na população total
  pop_df$proporcao <- pop_df$populacao / pop_df$populacao_total

  # reordenar factor de faixas de idade
  pop_df$faixa_idade <- factor(pop_df$faixa_idade, 
                               levels = c("de_00_a_03_anos",
                                          "de_04_e_05_anos",
                                          "de_06_a_10_anos", 
                                          "de_11_a_14_anos",
                                          "de_15_a_17_anos",
                                          "ate_17_anos",
                                          "acima_de_17_anos"))

  # criar faixas de ensino a partir das faixas de idade
  pop_df$faixa_ensino <- factor(pop_df$faixa_idade, 
                                levels = c("de_00_a_03_anos",
                                           "de_04_e_05_anos",
                                           "de_06_a_10_anos", 
                                           "de_11_a_14_anos",
                                           "de_15_a_17_anos",
                                           "ate_17_anos",
                                           "acima_de_17_anos"),
                                labels = c("infantil_creche",
                                           "infantil_pre",
                                           "fundamental_anos_iniciais", 
                                           "fundamental_anos_finais",
                                           "médio",
                                           "básico",
                                           "adulto"))
  
  # reordenar colunas
  pop_df <- pop_df |> select(cod_distr, distrito, ano, faixa_idade, faixa_ensino, 
                             populacao, populacao_total, proporcao)
  
  # calcular taxas de crescimento por faixa de idade e de ensino
  pop_growth_df <- pop_df |> 
    group_by(cod_distr, distrito, faixa_idade) |> 
    arrange(ano) |> 
    mutate(crescimento = populacao - lag(populacao),
           taxa_crescimento = (populacao / lag(populacao)) ^ (1/5) - 1) |> 
    drop_na()
  
  return(pop_growth_df)
}

# hexgrid_students <- tar_read(hexgrid_students)
# pop_growth_estimates <- tar_read(pop_growth_estimates_seade)
estimate_future_population_seade <- function(hexgrid_students, pop_growth_estimates) {
  
  # organizar hexgrid
  hexgrid_2022 <- prepare_hexgrid_for_estimation(hexgrid_students)
  
  # organizar tabela de estimativas de crescimento
  pop_growth_clean <- prepare_growthrates_for_estimation(pop_growth_estimates)
  
  # atualizar população segundo estimativas
  hexgrid_2025 <- estimate_growth(hexgrid_2022, pop_growth_clean, 2025)
  hexgrid_2030 <- estimate_growth(hexgrid_2025, pop_growth_clean, 2030)
  hexgrid_2035 <- estimate_growth(hexgrid_2030, pop_growth_clean, 2035)
  hexgrid_2040 <- estimate_growth(hexgrid_2035, pop_growth_clean, 2040)
  hexgrid_2045 <- estimate_growth(hexgrid_2040, pop_growth_clean, 2045)
  hexgrid_2050 <- estimate_growth(hexgrid_2045, pop_growth_clean, 2050)
  
  # juntar todas tabelas em um único sf
  hexgrid_pop_estimates_by_year <- rbindlist(list(hexgrid_2022, 
                                                  hexgrid_2025, 
                                                  hexgrid_2030, 
                                                  hexgrid_2035,
                                                  hexgrid_2040,
                                                  hexgrid_2045,
                                                  hexgrid_2050)) |> 
    st_as_sf()
  
  
  return(hexgrid_pop_estimates_by_year)
}

# hexgrid <- hexgrid_clean
# growth_rates <- pop_growth_clean
# final_year <- 2025
estimate_growth <- function(hexgrid, growth_rates, final_year) {
  
  hexgrid_upd <- hexgrid |> 
    ## add final estimation year to data.frame
    mutate(ano_final = final_year) |> 
    ## join with growth rates table
    left_join(growth_rates, by = c("code_district", "faixa_ensino", "ano_final")) |> 
    ## compute population by the end of the period
    mutate(n_estudantes_final = n_estudantes * (1 + taxa_crescimento) ^ (ano_final - ano_inicial)) |> 
    mutate(n_estudantes_final = round(n_estudantes_final)) |> 
    ## clean data.frame
    select(h3_address, h3_resolution, code_district, name_district, dre, nm_unidade_educacao,
           sg_etapa, sg_serie_ensino, faixa_ensino, ano = ano_final, n_estudantes = n_estudantes_final, 
           geometry)
  
  
  return(hexgrid_upd)
  
}

prepare_hexgrid_for_estimation <- function(hexgrid) {
  ## população atual corresponde ao ano 2022
  hexgrid$ano <- 2022
  
  ## atualizar faixas de ensino de acordo com a série / ano do aluno
  hexgrid$faixa_ensino <- factor(hexgrid$sg_serie_ensino,
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
                                            "FUNDAMENTAL BILINGUE"),
                                 labels = c("infantil_creche",
                                            "infantil_creche",
                                            "infantil_creche",
                                            "infantil_creche",
                                            "infantil_creche",
                                            "infantil_pre",
                                            "infantil_pre",
                                            "infantil_pre",
                                            "infantil_pre",
                                            "fundamental_anos_iniciais",
                                            "fundamental_anos_iniciais",
                                            "fundamental_anos_iniciais",
                                            "fundamental_anos_iniciais",
                                            "fundamental_anos_iniciais",
                                            "fundamental_anos_finais",
                                            "fundamental_anos_finais",
                                            "fundamental_anos_finais",
                                            "fundamental_anos_finais",
                                            "fundamental_anos_iniciais"))
  
  ## compatibilizar código do distrito (considerar apenas 2 últimos dígitos)
  hexgrid$code_district <- str_sub(hexgrid$code_district, 8, 9)
  
  ## selecionar apenas colunas relevantes
  hexgrid <- hexgrid |> 
    select(h3_address, h3_resolution, code_district, name_district, dre, nm_unidade_educacao,
           sg_etapa, sg_serie_ensino, faixa_ensino, ano, n_estudantes, geometry)
  
  return(hexgrid)
}

prepare_growthrates_for_estimation <- function(growth_rates) {
  ## compatibilizar código do distrito (considerar apenas 2 últimos dígitos)
  growth_rates$code_district <- 
    as.character(growth_rates$cod_distr) |> 
    substr(4, 5)
  
  ## organizar ordem das colunas
  growth_rates <- growth_rates |> 
    ungroup() |> 
    mutate(ano_final = ano, ano_inicial = ano - 5) |> 
    select(code_district, distrito, ano_inicial, ano_final, 
           faixa_idade, faixa_ensino,
           populacao, populacao_total, proporcao, 
           crescimento, taxa_crescimento)
  
  return(growth_rates)
}



# Estimativas de Crescimento Populacional - Censo Escolar -----------------

# census_folder <- tar_read(schools_census_raw_folder)
# sme_districts <- tar_read(sme_districts)
load_enrollments_from_census <- function(census_folder, sme_districts) {
  # arquivos csv na pasta do censo escolar
  csv_files <- list.files(census_folder, pattern = "csv", full.names = TRUE)
  
  # carregar todos os csv's em um único data.frame
  censo_df <- lapply(csv_files, fread, sep = ";", encoding = "Latin-1") |> 
    rbindlist()

  # área de estudo: São Paulo, SP
  censo_df <- censo_df[SG_UF == "SP" & NO_MUNICIPIO == "São Paulo"]
  
  # selecionar somente escolas estaduais com matriculas regulares de ensino infantil e fundamental
  censo_df <- censo_df[IN_REGULAR == 1 & TP_SITUACAO_FUNCIONAMENTO == 1]
  # escolas municipais e privadas conveniadas
  censo_df <- censo_df[TP_DEPENDENCIA == 3 | (TP_DEPENDENCIA == 4 & IN_CONVENIADA_PP == 1)] 
  # escolas que oferecem ensino infantil e fundamental
  censo_df <- censo_df[IN_INF == 1 | IN_FUND == 1]
  
  # totalizar número de escolas e matrículas por distrito
  censo_df[, CO_DISTRITO := as.character(CO_DISTRITO)]
  
  censo_df$CO_ORGAO_REGIONAL |> unique() |> length()
  totais_df <- censo_df |> 
    group_by(NU_ANO_CENSO, CO_DISTRITO) |> 
    summarise(across(.cols = c(IN_INF, IN_INF_CRE, IN_INF_PRE, 
                               IN_FUND, IN_FUND_AI, IN_FUND_AF,
                               QT_MAT_INF, QT_MAT_INF_CRE, QT_MAT_INF_PRE, 
                               QT_MAT_FUND, QT_MAT_FUND_AI, QT_MAT_FUND_AF),
                     .fns = sum, na.rm = TRUE),
              .groups = "drop")
  
  escolas_df <- totais_df |> 
    select(NU_ANO_CENSO, CO_DISTRITO, starts_with("IN")) |> 
    pivot_longer(cols = starts_with("IN"), names_to = "etapa_ensino", values_to = "escolas") |> 
    mutate(etapa_ensino = str_remove(etapa_ensino, "IN_"))
  
  matriculas_df <- totais_df |> 
    select(NU_ANO_CENSO, CO_DISTRITO, starts_with("QT")) |> 
    pivot_longer(cols = starts_with("QT"), names_to = "etapa_ensino", values_to = "matriculas") |> 
    mutate(etapa_ensino = str_remove(etapa_ensino, "QT_MAT_"))
    
  # consolidar totais de escolas e matrículas
  consolidado_df <- left_join(escolas_df, matriculas_df, 
                              by = c("NU_ANO_CENSO", "CO_DISTRITO", "etapa_ensino"))
    
  # corrigir etapas de ensino
  consolidado_df$etapa_ensino <- factor(consolidado_df$etapa_ensino,
                                        levels = c("INF",
                                                   "INF_CRE",
                                                   "INF_PRE",
                                                   "FUND",
                                                   "FUND_AI",
                                                   "FUND_AF"),
                                        labels = c("infantil",
                                                   "infantil_creche",
                                                   "infantil_pre",
                                                   "fundamental",
                                                   "fundamental_anos_iniciais",
                                                   "fundamental_anos_finais"))
  
  # adicionar DRE ao distrito
  dre_of_districts <- sme_districts |> 
    st_set_geometry(NULL) |> 
    select(code_district, dre, nm_unidade_educacao)
  
  consolidado_df <- consolidado_df |> 
    left_join(dre_of_districts, by = c("CO_DISTRITO" = "code_district")) |> 
    select(NU_ANO_CENSO, dre, nm_unidade_educacao, CO_DISTRITO, etapa_ensino, escolas, matriculas)
  
  # limpar data.frame e retornar
  consolidado_df <- janitor::clean_names(consolidado_df)
  return(consolidado_df)
}

# enrollments_by_year <- tar_read(enrollments_by_year)
estimate_growth_by_district_census <- function(enrollments_by_year) {
  # filtrar anos mais recentes do censo escolar
  # remover 2021 devido à pandemia
  enrollments_filtered <- enrollments_by_year |>
    select(-escolas) |>
    filter(nu_ano_censo >= 2016, nu_ano_censo <= 2020)
  
  enrollments_filtered_by_dre <- enrollments_filtered |> 
    group_by(nu_ano_censo, dre, nm_unidade_educacao, etapa_ensino) |> 
    summarise(matriculas = sum(matriculas, na.rm = TRUE), .groups = "drop")
  
  enrollments_nested <- enrollments_filtered |>
    group_by(dre, nm_unidade_educacao, etapa_ensino) |>
    nest()

  enrollments_model <- enrollments_nested |>
    mutate(model = map(data, function(x) lm(matriculas ~ nu_ano_censo, data = x)))

  new_df <- data.frame(nu_ano_censo = seq(2025, 2050, 5))

  enrollments_predict <- enrollments_model |>
    mutate(predictions = map(model,
                             function(x) {
                               matriculas <- predict(x, newdata = new_df)
                               matriculas = round(matriculas)

                               ret_df <- data.frame(nu_ano_censo = new_df$nu_ano_censo,
                                                    matriculas)

                               return(ret_df)
                             })) |>
    unnest(predictions) |> 
    select(dre, nm_unidade_educacao, nu_ano_censo, etapa_ensino, matriculas)

  enrollments_expanded <- rbind(enrollments_filtered_by_dre, enrollments_predict)

  return(enrollments_expanded)
  
  # enrollments_expanded |>
  #   filter(nu_ano_censo <= 2040) |>
  #   ggplot(aes(x=nu_ano_censo, y=matriculas, group = dre)) +
  #   geom_path() +
  #   scale_x_continuous(breaks = 2007:2040, labels = 7:40) +
  #   facet_wrap(~etapa_ensino, scales = "free")
  
}

# hexgrid_students <- tar_read(hexgrid_students)
# pop_growth_estimates_census <- tar_read(pop_growth_estimates_census)
estimate_future_population_census <- function(hexgrid_students, pop_growth_estimates_census) {
  
}


# draft -------------------------------------------------------------------

# 
# 
# tar_load(state_enrollments_raw_file)
# tar_load(state_schools)
# data_df <- fread(state_enrollments_raw_file)
# 
# data_2020 <- fread("../data_raw/dados_estaduais/Matriculas_por_aluno_2020.csv")
# data_2019 <- fread("../data_raw/dados_estaduais/Alunos_Completo_2019_0.csv", nrows = 10)
# 
# 
# 
# download.file(url = "https://dados.educacao.sp.gov.br/sites/default/files/Alunos_Completo_2019_0.csv",
#               destfile = "../data_raw/dados_estaduais/Alunos_Completo_2019_0.csv")
# data_sp_df <- data_df[CIDADE == "SAO PAULO"]
# 
# data_sp_df[CD_ESCOLA %in% state_schools$co_entidade]
# data_sp_df$GRAU |> unique()
# data_sp_df$SERIE |> unique()
# 
# data_sp_df |> 
#   count(BAIRRO, sort=TRUE)
# 
# data_sp_df$CEP
# 
# data_sp_df[, ano_nascimento := year(DTNASC)]
# data_sp_df[, idade := 2021 - ano_nascimento]
# 
# data_sp_df |> 
#   filter(idade <= 20) |> 
#   count(GRAU, sort = TRUE)
# 
# 
# data_sp_df |> 
#   filter(idade <= 20, GRAU %in% c(2, 6, 14, 101)) |>
#   count(GRAU, SERIE)
#   
# 
# data_sp_df |> 
#   count(GRAU, SERIE, idade, sort = TRUE) |> 
#   group_by(GRAU, SERIE) |> 
#   arrange(desc(n)) |> 
#   slice(1) |> 
#   View()
# 
# 
# data_sp_df |> 
#   filter(idade < 18, GRAU %in% c(2, 6, 14, 101)) |>
#   mutate(SERIE = factor(SERIE), idade = factor(idade)) |> 
#   count(GRAU, SERIE, idade, sort = TRUE) |> 
#   ggplot(aes(x=SERIE, y=idade, size = n)) +
#   geom_point() +
#   facet_wrap(~GRAU, scales="free")
#   
# 
# 
# data_df$DTNASC
# year(data_df$DTNASC[1])
# 
# # tar_load(sme_districts)
# # 
# # districts <- left_join(sme_districts, consolidado_df, by = c("code_district" = "co_distrito"))
# # 
# # districts <- st_set_geometry(districts, NULL)
# # 
# # regions <- districts |>
# #   group_by(dre, nm_unidade_educacao, nu_ano_censo, etapa_ensino) |>
# #   summarise(escolas = sum(escolas, na.rm = TRUE),
# #             matriculas = sum(matriculas, na.rm = TRUE),
# #             .groups = "drop")
# # 
# # 
# # regions |> 
# #   # filter(!(dre == "DRE - PE" & nu_ano_censo %in% c(2007, 2021))) |>
# #   filter(nu_ano_censo <= 2020, nu_ano_censo>= 2014) |>
# #   # filter(!(tp_dependencia == 2 & str_detect(etapa_ensino, "infantil"))) |> 
# #   drop_na() |> 
# #   ggplot(aes(x=nu_ano_censo, y=matriculas, color = dre)) +
# #   geom_path() +
# #   scale_x_continuous(breaks = 2007:2021, labels = 7:21) +
# #   facet_wrap(~etapa_ensino, scales = "free")
# # 
# # general <- districts |> 
# #   # filter(!(dre == "DRE - PE" & nu_ano_censo %in% c(2007, 2021))) |>
# #   filter(dre != "DRE - PE") |> 
# #   group_by(nu_ano_censo, tp_dependencia, etapa_ensino) |> 
# #   summarise(escolas = sum(escolas, na.rm = TRUE),
# #             matriculas = sum(matriculas, na.rm = TRUE),
# #             .groups = "drop")
# # 
# # general |> 
# #   # filter(!(dre == "DRE - PE" & nu_ano_censo %in% c(2007, 2021))) |> 
# #   filter(tp_dependencia != 1) |> 
# #   mutate(dep = factor(tp_dependencia)) |> 
# #   drop_na() |> 
# #   ggplot(aes(x=nu_ano_censo, y=matriculas, color = dep, group=dep)) +
# #   geom_path() +
# #   scale_x_continuous(breaks = 2007:2021, labels = 7:21) +
# #   facet_wrap(~etapa_ensino, scales = "free")
# 

# state_schools_geo |>
#   mapview::mapview(xcol = "lon", ycol = "lat", crs=4326)


