#' Dados do boletim escolar
#' 
#' Script para processar os dados do boletim escolar dos estudantes da cidade
#' de São Paulo. 


# setup -------------------------------------------------------------------
source("R/00_setup.R")

# load data ---------------------------------------------------------------
load_file <- function(f) {
  
  # csv files are already correctly formatted
  if (str_ends(f, ".csv")) {
    df <- read_delim(f, delim = ";", locale = locale(encoding = "Latin1", decimal_mark = ",", grouping_mark = "."))
    df <- janitor::clean_names(df)
    return(df)
  }
  
  if (str_ends(f, ".xlsx")) {
    df <- read_excel(f)
    # df <- janitor::clean_names(df)
    
    cols <- colnames(df)
    cols <- str_split(cols, pattern = ";")[[1]]

    # there are two Excel files with different formatting
    if (ncol(df) == 1) {
      # the Excel with a single column is the easier to fix
      # we just need to split the columns at the ;
      colnames(df) <- "data"
      df <- separate(df, col = data,  sep = ";", into = cols)
    } else {
      # the Excel files with three columns need a few steps to be fixed
      colnames(df) <- c("data", "a", "b")
      
      # data.table syntax is better for the next step
      setDT(df)

      # rows need a different fixing approach, depending on column b being 
      # empty or not
      df[is.na(b), c := paste0(data, ",", a)]
      df[!is.na(b), c := paste0(data, ",", a, ",", b)]
      
      # drop unnecessary columns
      df <- select(df, data = c)
      
      # finally, split the columns at the ;
      df <- separate(df, col = data,  sep = ";", into = cols)
    }
    
    df <- janitor::clean_names(df)
    
    return(df)
  }
    
}

filenames <- list.files(paste0(raw_data_path, "boletim"), full.names = TRUE)

dfs <- lapply(filenames, load_file)
dfs <- discard(dfs, is.null)

boletim_df <- rbindlist(dfs)

# process data ------------------------------------------------------------

boletim_df[, turma_codigo := str_remove_all(turma_codigo, "\"")]
boletim_df[, turma_nome := str_remove_all(turma_nome, "\"")]
boletim_df[, turma_ano := str_remove_all(turma_ano, "\"")]
boletim_df[, aluno_codigo := str_remove_all(aluno_codigo, "\"")]

boletim_df[, aluno_codigo := fifelse(aluno_codigo != "", aluno_codigo, NA_character_)]
boletim_df[, conceito_lingua_portuguesa := fifelse(conceito_lingua_portuguesa != "", conceito_lingua_portuguesa, NA_character_)]

boletim_df[, nota_lingua_portuguesa := str_remove_all(nota_lingua_portuguesa, " ")]
boletim_df[, nota_lingua_portuguesa := str_replace(nota_lingua_portuguesa, ",", ".")]
boletim_df[, nota_lingua_portuguesa := fifelse(nota_lingua_portuguesa != "", nota_lingua_portuguesa, NA_character_)]
boletim_df[, nota_lingua_portuguesa := fifelse(nota_lingua_portuguesa != ".", nota_lingua_portuguesa, NA_character_)]
boletim_df[, nota_lingua_portuguesa := as.numeric(nota_lingua_portuguesa)]

boletim_df[, total_aulas := fifelse(total_aulas != "", total_aulas, NA_character_)]
boletim_df[, total_aulas := as.integer(total_aulas)]

boletim_df[, total_ausencias := fifelse(total_ausencias != "", total_ausencias, NA_character_)]
boletim_df[, total_ausencias := as.integer(total_ausencias)]

boletim_df[, total_compensacoes := fifelse(total_compensacoes != "", total_compensacoes, NA_character_)]
boletim_df[, total_compensacoes := as.integer(total_compensacoes)]

boletim_df[, frequencia := str_remove_all(frequencia, " ")]
boletim_df[, frequencia := str_replace(frequencia, ",", ".")]
boletim_df[, frequencia := fifelse(frequencia != "", frequencia, NA_character_)]
boletim_df[, frequencia := as.numeric(frequencia)]


write_csv(boletim_df, "../data/boletim.csv")
