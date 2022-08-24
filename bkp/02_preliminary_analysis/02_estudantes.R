#' Gráficos da população de estudantes
#' 
#' Script para gerar as figuras da seção 3 do relatório (estudantes)


# setup -------------------------------------------------------------------
source("R/00_setup/00_setup.R")


# load data ---------------------------------------------------------------

alunos_raw_df <- read_delim(filenames[6], delim = "|", col_types = cols(.default = "c"), locale = locale(encoding = "Latin1")) |> 
  dplyr::select(CD_ALUNO, CD_ESCOLA, NOMESC, CD_TURMA_ESCOLA, SG_ETAPA, SG_SERIE_ENSINO,
                DT_NASCIMENTO_ALUNO, CD_SEXO_ALUNO, TP_RACA_COR, starts_with("DEF"),
                CD_NACIONALIDADE_ALUNO, NM_PAIS_MEC, NM_MUNICIPIO_NASC, MUN_ALU, SG_UF,
                QT_DISTANCIA, QT_DISTANCIA_CARRO, TEG) %>%
  distinct()


alunos_df <- read_rds("../data/alunos.rds")

alunos_df %>%
  mutate(SG_ETAPA = str_remove(SG_ETAPA, " ESPECIAL")) %>%
  count(SG_ETAPA, SG_SERIE_ENSINO) %>%
  filter(SG_ETAPA == "FUNDAMENTAL")



  filter(!str_detect(SG_ETAPA, "ESPECIAL")) %>%
  ggplot() +
  geom_col(aes(x=SG_SERIE_ENSINO, y=n)) +
  coord_flip() +
  facet_wrap(~SG_ETAPA, scales = "free_y")


