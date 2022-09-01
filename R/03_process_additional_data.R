# raw_file <- tar_read(schools_census_raw_file)
# geo_file <- tar_read(schools_census_geo_file)
load_state_schools <- function(raw_file, geo_file) {
  
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

# raw_file <- tar_read(pop_growth_estimates_raw_file)
load_pop_growth_estimates_by_district <- function(raw_file) {
  
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

# students_processed |> 
#   group_by(sg_etapa, sg_serie_ensino) |> 
#   summarise(lq_idade = quantile(idade, 0.25),
#             md_idade = median(idade),
#             uq_idade = quantile(idade, 0.75)) |> View()

# escolas_df |> filter(CO_ENTIDADE %in% c(35483655, 35902615, 35902615)) |> View()

# schools_census |> filter(is.na(lat) | is.na(lon)) |> View()

# library(tidyverse)
# library(data.table)
# 
# ## download school census microdata for 2021
# if (!file.exists(here::here("data_v2/schools", "microdados_censo_escolar_2021.zip"))) {
#   download.file(url = "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2021.zip",
#                 destfile = here::here("data_v2/schools", "microdados_censo_escolar_2021.zip"))
#   
#   unzip(here::here("data_v2/schools", "microdados_censo_escolar_2021.zip"),
#         exdir = here::here("data_v2/schools"))
# }
# 
# 
# ## load data
# schools <- fread(here::here("data_v2/schools/2021/dados/microdados_ed_basica_2021.csv"), 
#                  sep = ";", encoding = "Latin-1") 
# 
# 
# # selecionar somente matriculas em escolas publicas
# # matriculas <- matriculas[TP_DEPENDENCIA %in% c(1, 2, 3, 4)]
# 
# # IN_RESERVA_RENDA = 1
# # cotas para alunos de baixa renda
# # IN_REGULAR	
# # Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio
# 
# # count(matriculas, TP_ETAPA_ENSINO)
# 
# # manter apenas matrículas de ensino infantil e fundamental
# schools <- schools[(IN_INF + IN_INF_CRE + IN_INF_PRE + IN_FUND + IN_FUND_AI + IN_FUND_AF) >= 1]
# schools <- schools[, matriculas := QT_MAT_INF_INT + QT_MAT_FUND_INT]
# 
# 
# schools[, tipo := fifelse(TP_DEPENDENCIA == 4, "privada", "pública")]
# schools[, fundamental := fifelse(IN_FUND == 1 | IN_FUND_AI == 1 | IN_FUND_AF == 1, TRUE, FALSE)]
# schools[, infantil := fifelse(IN_INF == 1 | IN_INF_CRE == 1 | IN_INF_PRE == 1, TRUE, FALSE)]
# schools[, bolsas_estudo := fifelse(IN_RESERVA_RENDA == 1, TRUE, FALSE)]
# 
# schools <- schools[, .(CO_MUNICIPIO, NO_MUNICIPIO, SG_UF, CO_ENTIDADE, NO_ENTIDADE, 
#                        tipo, bolsas_estudo, infantil, fundamental, matriculas,
#                        DS_ENDERECO, NU_ENDERECO, DS_COMPLEMENTO, NO_BAIRRO, CO_CEP)]
# 
# write_csv(schools, here::here("data_v2/schools", "schools_sp.csv"))
# 
# 
# # geocode schools ---------------------------------------------------------
# 
# schools <- read_csv(here::here("data_v2/schools", "schools_sp.csv"))
# 
# library(ggmap)
# 
# 
# schools[, endereco := sprintf("%s %s, %s. Bairro %s. CEP %s. %s, %s",
#                               DS_COMPLEMENTO, DS_ENDERECO, NU_ENDERECO, NO_BAIRRO,
#                               CO_CEP, NO_MUNICIPIO, SG_UF)]
# 
# register_google(key = google_maps_api_key)
# 
# coordenadas_google <- lapply(X=schools$endereco, ggmap::geocode, output = "latlon")
# 
# coordenadas_df <- rbindlist(coordenadas_google)
# 
# schools <- cbind(schools, coordenadas_df)
# 
# write_csv(schools, here::here("data_v2/schools", "schools_sp_geo2.csv"))
# 
# mapview::mapview(schools, xcol="lon", ycol="lat", crs=4326)
# 
# 
