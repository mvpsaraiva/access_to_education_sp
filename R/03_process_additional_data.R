# raw_file <- tar_read(schools_census_raw_file)
load_schools_census <- function(raw_file) {
  
  escolas_df <- fread("../data_raw/microdados_ed_basica_2021.csv",
                      sep = ";", encoding = "Latin-1")
  
  ## área de estudo: São Paulo, SP
  escolas_df <- escolas_df[SG_UF == "SP" & NO_MUNICIPIO == "São Paulo"]

  # selecionar somente escolas estaduais com matriculas regulares
  escolas_df <- escolas_df[IN_REGULAR == 1 & TP_DEPENDENCIA == 2 & TP_SITUACAO_FUNCIONAMENTO == 1]
  
  escolas_df |> select(cd_sit = TP_SITUACAO_FUNCIONAMENTO,
                       depadm = TP_DEPENDENCIA,
                       nomedep = "ESTADUAL",
                       rede = "DIR",
                       dre
                       )
  
  glimpse(escolas_df) |> clipr::write_clip()
  # read schools data in csv format
  escolas_geo_df <- fread(raw_file)
  
  # remove schools with bad geolocation
  escolas_geo_df <- escolas_geo_df |> filter(SG_UF == "SP",
                                             NO_MUNICIPIO == "São Paulo",
                                             !(CO_ENTIDADE %in% c(35483655, 35902615)))
  
  
  

}

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
