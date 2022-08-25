
# Study Area --------------------------------------------------------------


# boundary_sf <- tar_read(boundary_muni)
# dem_rst <- tar_read(topography)
create_map_study_area <- function(boundary_sf, dem_rst) {
  # figura 1
  # limites do município
  # elevação
  
  bbox <- st_bbox(boundary_sf)
  
  # limite municipal e feições geográficas básicas
  p_muni <- boundary_sf |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(fill = NA) +
    coord_sf(datum = NA, 
             xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    labs(x = NULL, y = NULL)
    

  # mapa topográfico
  
  ## quebras das classes de elevação
  breaks <- quantile(dem_rst$elevation, seq(0, 1, 0.2))
  dem_rst$elevation_class <- cut(dem_rst$elevation, 
                                 breaks = breaks, 
                                 labels = breaks[2:6],
                                 include.lowest = TRUE)
  
  p_topo <- dem_rst %>%
    ggplot( ) +
    geom_raster(aes(x=x, y=y, fill = elevation_class), alpha = 0.8) +
    geom_sf(data = boundary_sf, fill = NA, size = 1) +
    annotation_scale(style = "ticks", location = "br") +
    # scale_fill_distiller(palette = "BrBG") +
    scale_fill_brewer(palette = "BrBG", direction = -1) +
    labs(x = NULL, y = NULL, fill = "Elevação (m)") +
    coord_sf(datum = NA, expand = FALSE,
             xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_light()
  
  p_figura <- p_muni + p_topo
  
  figure_path <- "output/report_02/fig_01_study_area.png"
  ggsave(plot = p_figura, filename = figure_path,
         width = 16, height = 10, units = "cm", dpi = 300, scale = 1.2)

  return(figure_path)
}

# census <- tar_read(census_tracts)
# bbox <- tar_read(sp_bbox)
create_map_census_tracts <- function(census, bbox) {
  p_census <- census %>%
    ggplot( ) +
    annotation_map_tile(type = "cartolight", zoom = 14, progress = "none") +
    geom_sf(fill = NA) +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA, 
             xlim = c(bbox$xmin, bbox$xmax),
             ylim = c(bbox$ymin, bbox$ymax))

  figure_path <- "output/report_02/fig_02_areal_units.png"
  ggsave(plot = p_census, filename = figure_path,
         width = 10, height = 10, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
}


# hexgrid_08 <- tar_read(hexgrid_res_08)
# hexgrid_09 <- tar_read(hexgrid_res_09)
# hexgrid_10 <- tar_read(hexgrid_res_10)
# bbox <- tar_read(sp_bbox)
create_map_hexgrid <- function(hexgrid_08,
                               hexgrid_09, 
                               hexgrid_10, bbox) {
  
  
  hexgrid <- rbind(hexgrid_08, hexgrid_09, hexgrid_10)
  
  p_grid <- hexgrid %>%
    ggplot( ) +
    annotation_map_tile(type = "cartolight", zoom = 14, progress = "none") +
    geom_sf(fill = NA) +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA,
             xlim = c(bbox$xmin, bbox$xmax),
             ylim = c(bbox$ymin, bbox$ymax)) +
    theme_light() +
    theme(strip.background = element_rect(fill = NA),
          strip.text = element_text(color = "grey10")) +
    facet_wrap(~h3_resolution, 
               labeller = labeller(h3_resolution = function(x) paste("resolução", x))) 
    
  
  figure_path <- "output/report_02/fig_03_hexgrid.png"
  ggsave(plot = p_grid, filename = figure_path,
         width = 16, height = 6, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
}


# Students ----------------------------------------------------------------


## Student Tables ------------------------------------------------------------------


# students_raw <- tar_read(students_raw)
# students_socio <- tar_read(students_socio)
# students_geo <- tar_read(students_geo)
create_table_student_count <- function(students_raw, students_socio, students_geo) {
  total_students <- nrow(students_raw)
  total_students_socio <- nrow(students_socio)
  total_students_geo <- nrow(students_geo)
  total_students_muni <- nrow(students_geo |> filter(dentro_muni == TRUE))
  
  total_df <- data.frame("students" = total_students,
                         "socio" = total_students_socio,
                         "geo" = total_students_geo,
                         "muni" = total_students_muni) |> 
    pivot_longer(cols = everything(), names_to = "subset", values_to = "n_students")
  
  table_path <- "output/report_02/tab_01_number_of_students.xlsx"
  write_xlsx(total_df, table_path)
  
  return(table_path)
}

# students_processed <- tar_read(students_processed)
create_table_students_by_stage <- function(students_processed) {
  summary_df <-
    students_processed |> 
    filter(!(sg_etapa = str_detect(sg_etapa, "ESPECIAL"))) |> 
    group_by(sg_etapa, sg_serie_ensino) |> 
    summarise(n = n(), 
              lq_idade = quantile(idade, 0.25, na.rm = T),
              uq_idade = quantile(idade, 0.75, na.rm = T),
              .groups = "drop")

  table_path <- "output/report_02/tab_02_students_by_stage.xlsx"
  write_xlsx(summary_df, table_path)
  
  return(table_path)
}

# students_processed <- tar_read(students_processed)
create_table_students_by_race <- function(students_processed) {
  summary_df <- students_processed |> 
    count(dc_raca_cor) |> 
    mutate(p = n / sum(n)) |> 
    mutate(p = scales::percent(p, accuracy = 0.1))
  
  table_path <- "output/report_02/tab_03_students_by_race.xlsx"
  write_xlsx(summary_df, table_path)
  
  return(table_path)
}

# students_processed <- tar_read(students_processed)
create_table_students_by_income <- function(students_processed) {
  summary_df <- students_processed |> 
    count(bf, faixa_renda) |> 
    mutate(p = scales::percent(n / sum(n), accuracy = 0.01)) |> 
    pivot_wider(names_from = bf, values_from = c(n, p), 
                values_fill = list(n = 0, p = "0.0%")) |> 
    select(faixa_renda, 
           sem_beneficio = n_NAO,
           sem_beneficio_pc = p_NAO,
           com_beneficio = n_SIM,
           com_beneficio_pc = p_SIM) |> 
    mutate(total = sem_beneficio + com_beneficio,
           total_pc = scales::percent(total / sum(total, na.rm = T), accuracy = 0.01))

  table_path <- "output/report_02/tab_04_students_by_income.xlsx"
  write_xlsx(summary_df, table_path)
  
  return(table_path)
}


## Student Figures -----------------------------------------------------------------

# students_processed <- tar_read(students_processed)
# hexgrid_res_08 <- tar_read(hexgrid_res_08)
# boundary <- tar_read(boundary_muni)
create_map_students_count <- function(students_processed, boundary, hexgrid_res_08) {
  students_by_hex <- students_processed |> 
    count(id_hex_08)
  
  p <- hexgrid_res_08 |> 
    left_join(students_by_hex, by = c("h3_address" = "id_hex_08")) |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(aes(fill = n), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent",
                         limits = c(0, 4000)) +
    theme_minimal() +
    labs(fill = "Número de\nEstudantes")
  
  figure_path <- "output/report_02/fig_04_students_count.png"
  ggsave(plot = p, filename = figure_path,
         width = 8, height = 9, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
}

# students_processed <- tar_read(students_processed)
# hexgrid_res_08 <- tar_read(hexgrid_res_08)
# boundary <- tar_read(boundary_muni)
create_map_students_by_income <- function(students_processed, boundary, hexgrid_res_08) {
  students_by_hex <- students_processed |> 
    group_by(id_hex_08, faixa_renda) |> 
    summarise(n = n(), .groups = "drop_last") |> 
    mutate(p = n / sum(n))
  
  p <- hexgrid_res_08 |> 
    inner_join(students_by_hex, by = c("h3_address" = "id_hex_08")) |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 9, progress = "none") +
    geom_sf(aes(fill = p), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent",
                         breaks = 0.25*0:4, 
                         labels = percent(0.25*0:4)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = "Proporção de\nEstudantes") +
    facet_wrap(~faixa_renda, nrow = 1)

  figure_path <- "output/report_02/fig_05_students_by_income.png"
  ggsave(plot = p, filename = figure_path,
         width = 20, height = 9, units = "cm", dpi = 300, scale = 1.4)
  
  return(figure_path)
}

# students_processed <- tar_read(students_processed)
# hexgrid_res_08 <- tar_read(hexgrid_res_08)
# boundary <- tar_read(boundary_muni)
create_map_students_by_race <- function(students_processed, boundary, hexgrid_res_08) {
  students_by_hex <- students_processed |> 
    mutate(dc_raca_cor = recode(dc_raca_cor,
                                "PRETA" = "NEGRA",
                                "PARDA" = "NEGRA")) |>
    group_by(id_hex_08, dc_raca_cor) |> 
    summarise(n = n(), .groups = "drop_last") |> 
    mutate(p = n / sum(n)) |> 
    filter(dc_raca_cor %in% c("BRANCA", "NEGRA", "AMARELA", "INDIGENA"))
  
  p <- hexgrid_res_08 |> 
    inner_join(students_by_hex, by = c("h3_address" = "id_hex_08")) |> 
    # filter(dc_raca_cor == "BRANCA") |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 9, progress = "none") +
    geom_sf(aes(fill = p), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent",
                         breaks = 0.25*0:4, 
                         labels = percent(0.25*0:4)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = "Proporção de\nEstudantes") +
    facet_wrap(~dc_raca_cor, nrow = 1)
  
  figure_path <- "output/report_02/fig_06_students_by_race.png"
  ggsave(plot = p, filename = figure_path,
         width = 16, height = 9, units = "cm", dpi = 300, scale = 1.4)
  
  return(figure_path)
}



# students_processed <- tar_read(students_processed)
# hexgrid_res_08 <- tar_read(hexgrid_res_08)
# boundary <- tar_read(boundary_muni)
create_map_students_by_teg <- function(students_processed, boundary, hexgrid_res_08) {
  students_by_hex <- students_processed |> 
    group_by(id_hex_08, teg) |> 
    summarise(n = n(), .groups = "drop_last") |> 
    mutate(p = n / sum(n))
  
  p <- hexgrid_res_08 |> 
    inner_join(students_by_hex, by = c("h3_address" = "id_hex_08")) |> 
    filter(teg == "SIM") |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 9, progress = "none") +
    geom_sf(aes(fill = p), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent",
                         breaks = 0.25*0:4, 
                         labels = percent(0.25*0:4)) +
    theme_minimal() +
    labs(fill = "Proporção de\nEstudantes") 
  
  figure_path <- "output/report_02/fig_09_students_by_teg.png"
  ggsave(plot = p, filename = figure_path,
         width = 8, height = 9, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
}

# Schools -----------------------------------------------------------------


## Schools Tables ----------------------------------------------------------


# schools_geo <- tar_read(schools_geo)
create_table_schools_count <- function(schools_geo) {
  summary_df <- schools_geo |>
    group_by(nomedep, rede) |> 
    summarise(n_escolas = n(), n_alunos = sum(qt_alunos, na.rm = T), .groups = "drop") |> 
    mutate(p_escolas = n_escolas / sum(n_escolas),
           p_alunos = n_alunos / sum(n_alunos),
           alunos_escola = n_alunos / n_escolas)
  
  table_path <- "output/report_02/tab_05_number_of_schools.xlsx"
  write_xlsx(summary_df, table_path)
  
  return(table_path)
}


# rooms_geo <- tar_read(rooms_geo)
create_table_rooms_count <- function(rooms_geo) {
  summary_df <- rooms_geo |> 
    mutate(dif_pessoas = qt_ideal_pessoa - qt_real_pessoa) |> 
    group_by(dc_tipo_ambiente) |> 
    summarise(n = n(),
              qt_ideal_pessoa = sum(qt_ideal_pessoa, na.rm = T),
              qt_real_pessoa = sum(qt_real_pessoa, na.rm = T),
              dif_pessoas = mean(dif_pessoas, na.rm = T))
  
  table_path <- "output/report_02/tab_06_number_of_rooms.xlsx"
  write_xlsx(summary_df, table_path)
  
  return(table_path)
}


## Schools Figures ---------------------------------------------------------

# schools_geo <- tar_read(schools_geo)
# hexgrid_res_08 <- tar_read(hexgrid_res_08)
# boundary <- tar_read(boundary_muni)
create_map_schools_count <- function(schools_geo, boundary, hexgrid_res_08) {
  schools_by_hex <- schools_geo |> 
    group_by(id_hex_08, nomedep) |> 
    summarise(n_schools = n(), n_students = sum(qt_alunos, na.rm = TRUE), .groups = "drop")
  
  p_schools <- hexgrid_res_08 |>
    left_join(schools_by_hex, by = c("h3_address" = "id_hex_08")) |>
    drop_na() |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(aes(fill = n_schools), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent",
                         limits = c(0, 25)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = "Número de\nEscolas",
         title = "Escolas") +
    facet_wrap(~nomedep)
  
  p_students <- hexgrid_res_08 |>
    left_join(schools_by_hex, by = c("h3_address" = "id_hex_08")) |>
    drop_na() |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(aes(fill = n_students), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent",
                         limits = c(0, 6500)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = "Número de\nEstudantes",
         title = "Estudantes") +
    facet_wrap(~nomedep)
    
  p <- p_schools + p_students

  figure_path <- "output/report_02/fig_07_schools_count.png"
  ggsave(plot = p, filename = figure_path,
         width = 16, height = 8.5, units = "cm", dpi = 300, scale = 1.4)
  
  return(figure_path)
}

# Distance to schools -------------------------------------------

# students_processed <- tar_read(students_processed)
create_plot_distance_to_school <- function(students_processed) {
  # remover alunos de ensino especial (libras e bilingue)
  students_filtered <- students_processed |> 
    filter(!str_detect(sg_etapa, "ESPECIAL"))
  
  summary_serie <- summarise_distances(students_filtered, "sg_serie_ensino")
  p_serie <- plot_distances(summary_serie, "sg_serie_ensino", "a) Série", show_x = TRUE)
  
  summary_raca <- summarise_distances(students_filtered, "dc_raca_cor")
  p_raca <- plot_distances(summary_raca, "dc_raca_cor", "b) Raça / Cor", show_x = TRUE)
  
  summary_renda <- summarise_distances(students_filtered, "faixa_renda")
  p_renda <- plot_distances(summary_renda, "faixa_renda", "c) Faixa de Renda", show_x = TRUE)
  
  summary_teg <- summarise_distances(students_filtered, "teg")
  p_teg <- plot_distances(summary_teg, "teg", "d) Transporte Escolar Gratuito", show_x = TRUE)
  
  p_combinado <- p_serie + ( p_raca / p_renda / p_teg )
  
  figure_path <- "output/report_02/fig_08_distance_to_school.png"
  ggsave(plot = p_combinado, filename = figure_path,
         width = 16, height = 16, units = "cm", dpi = 300, scale = 1.4)
  
  return(figure_path)
}

# summarise distances for function create_plot_distance_to_school()
summarise_distances <- function(data, variable_name) {
  summary_df <- data |> 
    mutate(is_zero = qt_distancia == 0,
           is_inf = qt_distancia >= 99999,
           is_long = qt_distancia >= 10000) |> 
    mutate(qt_distancia = qt_distancia / 1000) |> 
    group_by(across(all_of(variable_name))) |> 
    summarise(lq_distancia = quantile(qt_distancia, 0.25, na.rm = T),
              md_distancia = median(qt_distancia, na.rm = T),
              uq_distancia = quantile(qt_distancia, 0.75, na.rm = T),
              zero_dist = sum(is_zero, na.rm = T),
              long_dist = sum(is_long, na.rm = T),
              inf_dist = sum(is_inf, na.rm = T)) |> 
    mutate(across(all_of(variable_name), fct_rev) )
  
  return(summary_df)
}

plot_distances <- function(data, variable_name, title_label, show_x = TRUE) {
  
  p <-
    data |> 
    ggplot(aes(x = .data[[variable_name]])) +
    geom_point(aes(y = md_distancia)) +
    geom_linerange(aes(ymin = lq_distancia, ymax = uq_distancia)) +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 10, 0.5),
                       minor_breaks = seq(0, 10, 0.1),
                       limits = c(0, 3)) +
    theme_light() +
    labs(subtitle = title_label, x = NULL)

  if (show_x == TRUE) {
    # coord_flip changes x and y axes
    p <- p + labs(y = "distância até a escola (km)")
  } else {
    p <- p + labs(y = NULL)
  }
  
  return(p)
}

