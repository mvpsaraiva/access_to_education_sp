#' Dados de alunos e escolas enviados para o BID
#' 
#' Script para processar os dados do cadastro de escolas e estudantes da cidade
#' de São Paulo. 


# setup -------------------------------------------------------------------
source("R/00_setup/00_setup.R")



# load data ---------------------------------------------------------------

filenames <- list.files(paste0(raw_data_path, "bid"), pattern = "2022.csv",
                        full.names = TRUE)


# alunos ------------------------------------------------------------------


## carregar CSV com dados dos alunos
alunos_df <- read_delim(filenames[6], delim = "|", col_types = cols(.default = "c"), locale = locale(encoding = "Latin1")) |> 
    dplyr::select(CD_ALUNO, CD_ESCOLA, NOMESC, CD_TURMA_ESCOLA, SG_ETAPA, SG_SERIE_ENSINO,
                  DT_NASCIMENTO_ALUNO, CD_SEXO_ALUNO, TP_RACA_COR, starts_with("DEF"),
                  CD_NACIONALIDADE_ALUNO, NM_PAIS_MEC, NM_MUNICIPIO_NASC, MUN_ALU, SG_UF,
                  QT_DISTANCIA, QT_DISTANCIA_CARRO, TEG)
total_alunos <- nrow(alunos_df)

alunos_df <- distinct(alunos_df)
total_alunos_unicos <- nrow(alunos_df)

## filtrar por etapas de ensino (Infantil e Fundamental)
etapas <- c("ENS FUND9A", "FUND ESP 9", "ED INF", "ED INF ESP")
alunos_filtered_df <- alunos_df %>% filter(SG_ETAPA %in% etapas)

total_alunos_inf_fund <- nrow(alunos_filtered_df)

## recodificar etapas e séries

# recodificar colunas SG_ETAPA, SG_SERIE_ENSINO e SG_RACA_COR
# converter o campo DT_NASCIMENTO_ALUNO para data
alunos_recoded_df <- alunos_filtered_df %>%
  mutate(SG_ETAPA = recode(SG_ETAPA,
                           "ENS FUND9A" = "FUNDAMENTAL", 
                           "FUND ESP 9" = "FUNDAMENTAL ESPECIAL", 
                           "ED INF"     = "INFANTIL", 
                           "ED INF ESP" = "INFANTIL ESPECIAL"),
         SG_SERIE_ENSINO = recode(SG_SERIE_ENSINO,
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
         DC_RACA_COR = recode(TP_RACA_COR,
                              `1` = "BRANCA",
                              `2` = "PRETA",
                              `3` = "PARDA",
                              `4` = "AMARELA",
                              `5` = "INDIGENA",
                              `6` = "NAO INFORMADA",
                              `7` = "RECUSOU INFORMAR",
                              .missing = "NAO INFORMADA"),
         DT_NASCIMENTO_ALUNO = as.POSIXct(DT_NASCIMENTO_ALUNO))

glimpse(alunos_recoded_df)

# condições socio economicas dos alunos
socio_df <- read_delim(filenames[5], delim = "|", col_types = cols(.default = "c"), locale = locale(encoding = "Latin1")) |> 
  distinct()

alunos_socio_df <- alunos_recoded_df %>% left_join(socio_df, by = "CD_ALUNO") %>%
  replace_na(list(BF = "NAO", FAIXA_RENDA = "NAO INFORMADA")) 

total_alunos_socio <- alunos_socio_df |> filter(FAIXA_RENDA != "NAO INFORMADA") |> nrow()

# coordenadas geográficas das residêcias dos alunos
alunos_geo_df <- read_delim(filenames[1], delim = "|", col_types = "cdd",
                            locale = locale(encoding = "Latin1", decimal_mark = ",", grouping_mark = ".")) |> 
  drop_na() |> 
  distinct()

colnames(alunos_geo_df) <- c("CD_ALUNO", "lon", "lat")

alunos_geo_sf <- alunos_geo_df |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

## remover alunos localizados fora da RMSP
boundary_sf <- read_rds("../data/geo/metropolitan_boundary.rds") |> 
  st_transform(crs = 4326)

alunos_dentro_rmsp <- st_within(alunos_geo_sf, boundary_sf, sparse = FALSE)
alunos_dentro_rmsp <- rowSums(alunos_dentro_rmsp) >= 1 # alunos dentro de algum dos municípios da RM

alunos_dentro_df <- alunos_geo_df[alunos_dentro_rmsp,]

# remover duplicados
alunos_dentro_df <- alunos_dentro_df |> 
  group_by(CD_ALUNO) |> 
  summarise(across(.cols = lon:lat, first), .groups = "drop") 
  
alunos_socio_geo_df <- alunos_socio_df %>% inner_join(alunos_dentro_df, by = "CD_ALUNO")

total_alunos_geo <- nrow(alunos_socio_geo_df)

# clean up data.frame
alunos_clean_df <- alunos_socio_geo_df %>%
  dplyr::select(CD_ALUNO, CD_ESCOLA, NOMESC, CD_TURMA_ESCOLA, SG_ETAPA, SG_SERIE_ENSINO,
         DT_NASCIMENTO_ALUNO, CD_SEXO_ALUNO, TP_RACA_COR, DC_RACA_COR, starts_with("DEF"),
         FAIXA_RENDA, BF, 
         CD_NACIONALIDADE_ALUNO, NM_PAIS_MEC, NM_MUNICIPIO_NASC, MUN_ALU, SG_UF,
         QT_DISTANCIA, QT_DISTANCIA_CARRO, TEG, lat, lon)

# Total de alunos na base de dados
total_alunos

# Número de alunos duplicados
total_alunos - total_alunos_unicos

# Número de alunos de ensino médio e técnico
total_alunos_unicos - total_alunos_inf_fund

# Número de alunos localizados fora da RMSP
total_alunos_inf_fund - total_alunos_geo

# Total de alunos com dados consistentes
total_alunos_geo


# escolas -----------------------------------------------------------------


escolas_df <- read_delim(filenames[3], delim = "|", locale = locale(encoding = "Latin1", decimal_mark = ",", grouping_mark = ".")) %>%
  rename(lat = CD_COORDENADA_GEO_Y, lon = CD_COORDENADA_GEO_X)


# ambientes ---------------------------------------------------------------

# ambientes (salas) disponíveis nas escolas
ambiente_df <- read_delim(filenames[2], delim = "|", locale = locale(encoding = "Latin1"))


# transferências e intenção de trasferência -------------------------------

transferidos_df <- read_delim(filenames[7], delim = "|", col_types = cols(.default = "c"), locale = locale(encoding = "Latin1"))
intencao_transf_df <- read_delim(filenames[4], delim = "|", col_types = cols(.default = "c"), locale = locale(encoding = "Latin1"))



# salvar dados processados ------------------------------------------------


write_rds(alunos_clean_df, "../data/alunos.rds")
write_rds(alunos_geo_df, "../data/alunos_geo.rds")
write_rds(socio_df, "../data/socio.rds")

write_rds(escolas_df, "../data/escolas.rds")
write_rds(ambiente_df, "../data/ambiente.rds")

write_rds(transferidos_df, "../data/transferidos.rds")
write_rds(intencao_transf_df, "../data/intencao_transf.rds")



# draft code --------------------------------------------------------------


transf_df <- transferidos_df %>%
  left_join(escolas_df, by = c("CD_ESCOLA_ATUAL"="CD_ESCOLA")) %>%
  left_join(escolas_df, by = c("CD_ESCOLA_ANTIGA"="CD_ESCOLA"), suffix = c(".new", ".old"))

transferidos_df %>%
  count(CD_ESCOLA_ANTIGA, CD_ESCOLA_ATUAL, sort = TRUE)

transf_clean_df <- transf_df %>%
  filter(CD_ESCOLA_ATUAL != CD_ESCOLA_ANTIGA) %>%
  select(CD_ESCOLA_ATUAL, lat.new, lon.new, CD_ESCOLA_ANTIGA, lat.old, lon.old,
         CD_ALUNO, SG_SERIE_ENSINO)


transf_clean_df %>%
  drop_na() %>%
  sample_n(100) %>%
  ggplot() +
  geom_segment(aes(x=lon.new, y=lat.new, xend=lon.old, yend=lat.old)) +
  coord_map()
  
intencao_transf_df %>%
  select(CD_ESCOLA, CD_ESCOLA_SOL) %>%
  filter(CD_ESCOLA != CD_ESCOLA_SOL)

intencao_transf_df %>%
  count(CD_ESCOLA_SOL, sort=T)

alunos_clean_df |> 
  group_by(CD_ALUNO) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> View()

socio_df |> group_by(CD_ALUNO) |> mutate(n = n()) |> filter(n >= 2) |> View()
