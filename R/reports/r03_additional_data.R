# sme_districts <- tar_read(sme_districts)
create_map_regions_districts <- function(sme_districts) {
  
  # diretorias regionais de educação e distritos
  districts_sf <- sme_districts |> 
    mutate(nm_unidade_educacao = str_remove(nm_unidade_educacao, "DIRETORIA REGIONAL DE EDUCACAO "))
  
  p_dre_districts <- districts_sf |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(aes(fill = nm_unidade_educacao)) +
    scale_fill_brewer(palette = "Paired") +
    coord_sf(datum = NA) +
    labs(x = NULL, y = NULL,
         fill = "Diretoria Regional de Educação")
  
  # p_dre_districts

  figure_path <- "output/report_03/fig_01_regions_districts.png"
  ggsave(plot = p_dre_districts, filename = figure_path,
         width = 16, height = 10, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
  
  
}

# access <- tar_read(unitary_access)
# hexgrid <- tar_read(hexgrid)
create_map_unitary_access <- function(access, hexgrid) {
  
  access_sf <- left_join(hexgrid, access, by = c("h3_address" = "id"))
  
  p <- access_sf |> 
    ggplot(aes(fill = accessibility)) +
    geom_sf(color = NA) +
    scale_fill_viridis()
  
  figure_path <- "output/report_03/fig_xx_unitary_access.png"
  ggsave(plot = p, filename = figure_path,
         width = 16, height = 9, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
  
}

# unitary_access <- tar_read(unitary_access_r10)
# tmi <- tar_read(tmi_r10)
# hexgrid <- tar_read(hexgrid_res_10)
# boundary <- tar_read(boundary_muni)
create_map_travel_times <- function(unitary_access, tmi, hexgrid, boundary) {
  
  access_sf <- inner_join(hexgrid, unitary_access, by = c("h3_address" = "id"))
  
  p1 <- access_sf |> 
    ggplot(aes(fill = accessibility)) +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40", size = 0.5) +
    scale_fill_viridis() +
    coord_sf(datum = NA) +
    annotation_scale(style = "ticks", location = "br") +
    theme_minimal() +
    labs(fill = "Área\nacessível")
  
  tmi_sf <- inner_join(hexgrid, tmi, by = c("h3_address" = "id")) |> 
    filter(travel_time <= 15)
  
  p2 <- tmi_sf |> 
    ggplot(aes(fill = travel_time)) +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40", size = 0.5) +
    scale_fill_viridis(direction = -1) +
    coord_sf(datum = NA) +
    annotation_scale(style = "ticks", location = "br") +
    theme_minimal() +
    labs(fill = "Tempo de\nviagem")
  
  pc <- p1 + p2 + plot_annotation(tag_levels = "A")
   
  figure_path <- "output/report_03/fig_xx_travel_times.png"
  ggsave(plot = pc, filename = figure_path,
         width = 16, height = 9, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
  
}

# rooms_geo <- tar_read(rooms_geo)
create_plot_problematic_rooms <- function(rooms_geo) {
  
  p <- rooms_geo |>
    ggplot(aes(x=qt_area_util, y=qt_real_pessoa)) +
    geom_point(size = 0.2) +
    scale_color_brewer(palette = "Set1") +
    scale_x_log10(breaks = c(1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 35000),
                  labels = scales::comma_format()) +
    scale_y_log10(breaks = c(1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 35000),
                  labels = scales::comma_format()) +
    theme_light() +
    labs(x = "Área útil", y = "Ocupação")
  
  figure_path <- "output/report_03/fig_xx_problematic_rooms.png"
  ggsave(plot = p, filename = figure_path,
         width = 16, height = 8, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
}


# rooms_fixed <- tar_read(rooms_fixed)
create_plot_fixed_rooms <- function(rooms_fixed) {
  
  p <- rooms_fixed |>
    ggplot(aes(x=qt_area_util_1_2_adj, y=qt_real_pessoa_adj)) +
    geom_point(size = 0.2) +
    geom_abline(color = "red") +
    scale_color_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(0, 160, 20), limits = c(0, 160)) +
    scale_y_continuous(breaks = seq(0, 140, 20), limits = c(0, 140)) +
    theme_light() +
    labs(x = "Área útil (Ajustada)", y = "Ocupação (Ajustada)")
  
  p
  
  figure_path <- "output/report_03/fig_xx_fixed_rooms.png"
  ggsave(plot = p, filename = figure_path,
         width = 16, height = 8, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
}

# rooms_fixed <- tar_read(rooms_fixed)
create_plot_fixed_rooms_histogram <- function(rooms_fixed) {
  
  p_area <- rooms_fixed |> 
    ggplot(aes(qt_area_util_1_2_adj)) +
    geom_histogram(binwidth = 10, color = "grey20", fill = "grey60") +
    scale_x_continuous(breaks = seq(0, 160, 10)) +
    scale_y_continuous(labels = comma_format()) +
    theme_light() +
    labs(x = "Área útil (Ajustada)", y = NULL,
         subtitle = "a) Área útil dos ambientes ("~m^2~")")
    
  p_pessoas <- rooms_fixed |> 
    ggplot(aes(qt_real_pessoa_adj)) +
    geom_histogram(binwidth = 10, color = "grey20", fill = "grey60") +
    scale_x_continuous(breaks = seq(0, 140, 10)) +
    scale_y_continuous(labels = comma_format()) +
    theme_light() +
    labs(x = "Ocupação (Ajustada)", y = NULL,
         subtitle = "b) Ocupação dos ambientes (número de estudantes)")
  
  p_combinado <- p_area / p_pessoas
  
  figure_path <- "output/report_03/fig_xx_fixed_rooms_histogram.png"
  ggsave(plot = p_combinado, filename = figure_path,
         width = 16, height = 8, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
}

# state_schools <- tar_read(state_schools_enrollments)
# hexgrid_res_08 <- tar_read(hexgrid_res_08)
# boundary <- tar_read(boundary_muni)
create_map_state_schools <- function(state_schools, boundary, hexgrid_res_08) {
  
  # Schools
  schools_sf <- state_schools |> 
    select(cod_esc, sg_etapa, lat, lon) |> 
    distinct() |> 
    mutate(presente = "Escolas") |> 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  p_schools <- schools_sf |>
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(size = 0.5, color = "steelblue4") +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(color = "Nível de Ensino") +
    facet_wrap(~presente)

  # Enrollments
  
  state_schools[, serie := as.numeric(str_sub(sg_serie_ensino, 1, 1)) ]
  state_schools[, etapa := ifelse(serie <= 5, 
                                  "Matrículas: Anos Iniciais",
                                  "Matrículas: Anos Finais")]
  
  state_schools$etapa <- factor(state_schools$etapa,
                                levels = c("Matrículas: Anos Iniciais",
                                           "Matrículas: Anos Finais"))
  
  id_hex_08 <- h3jsr::get_parent(state_schools$id_hex_10, res = 8)
  state_schools$id_hex_08 <- id_hex_08
  
  
  enrollments_by_hex <- state_schools |> 
    drop_na() |> 
    group_by(id_hex_08, etapa) |>
    summarise(matriculas = sum(n_alunos, na.rm = TRUE), .groups = "drop")
    
  p_enrollments <-
    hexgrid_res_08 |> 
    left_join(enrollments_by_hex, by = c("h3_address" = "id_hex_08")) |> 
    drop_na() |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(aes(fill = matriculas), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent",
                         limits = c(0, 2500), breaks = c(0, 1250, 2500)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(fill = "Número de\nMatrículas") +
    facet_wrap(~etapa)  
  
  # combine plots
  p_combined <- p_schools + p_enrollments +
    plot_layout(design = "ABB", guides = "collect") & theme(legend.position = "bottom")
  
  
  # save figure
  figure_path <- "output/report_03/fig_xx_state_schools_b.png"
  ggsave(plot = p_combined, filename = figure_path,
         width = 16, height = 11, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
}

create_table_state_enrollments <- function(state_schools_enrollments) {

  total_df <- state_schools_enrollments |> 
    drop_na() |> 
    count(sg_etapa, sg_serie_ensino, wt = n_alunos)
  
  table_path <- "output/report_03/tab_02_number_of_state_students.xlsx"
  write_xlsx(total_df, table_path)
  
  return(table_path)
}


# sme_districts <- tar_read(sme_districts)
# pop_growth_estimates <- tar_read(pop_growth_estimates)
# boundary <- tar_read(boundary_muni)
create_map_pop_growth <- function(sme_districts, pop_growth_estimates, boundary) {
  
  pop_growth_estimates$cod_distr_short <- str_sub(pop_growth_estimates$cod_distr, 4, 6)
  
  sme_districts_growth <- sme_districts |> 
    left_join(pop_growth_estimates, by = c("cd_dist" = "cod_distr_short"))

  sme_districts_filtered <- sme_districts_growth |> 
    filter(ano >= 2020, ano <= 2040, faixa_idade %in% c("de_00_a_03_anos",
                                                       "de_04_e_05_anos",
                                                       "de_06_a_10_anos",
                                                       "de_11_a_14_anos"))
  
  sme_districts_filtered$faixa_idade <- recode(sme_districts_filtered$faixa_idade,
                                               "de_00_a_03_anos" = "0 a 3 anos",
                                               "de_04_e_05_anos" = "4 a 5 anos",
                                               "de_06_a_10_anos" = "6 a 10 anos",
                                               "de_11_a_14_anos" = "11 a 14 anos"
  )
  
  sme_districts_filtered$ano <- paste0(sme_districts_filtered$ano - 5, 
                                       " - ",
                                       sme_districts_filtered$ano)
  
  p <- sme_districts_filtered |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(aes(fill = taxa_crescimento), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40", size = 0.5) +
    coord_sf(datum = NA) +
    annotation_scale(style = "ticks", location = "br") +
    scale_fill_gradient2(low = "orangered3", high = "steelblue3",
                         limits = c(-0.2, 0.2), 
                         breaks = c(-0.086, 0, 0.1, 0.178),
                         labels = scales::percent) +
    theme_minimal() +
    labs(fill = "Taxa de\nCrescimento") +
    facet_grid(faixa_idade ~ ano, switch = "y")

  figure_path <- "output/report_03/fig_xx_pop_growth_rates.png"
  ggsave(plot = p, filename = figure_path,
         width = 16, height = 16, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
  
}

# hexgrid_students_future <- tar_read(hexgrid_students_future)
# boundary <- tar_read(boundary_muni)
# hexgrid_res_08 <- tar_read(hexgrid_res_08)
create_map_pop_growth_estimates <- function(hexgrid_students_future, boundary, hexgrid_res_08) {
  
  # agregar em escala maior
  hex_08 <- h3jsr::get_parent(hexgrid_students_future$h3_address, res = 8)
  hexgrid_students_future$h3_parent <- hex_08
  
  # agrupar por etapa de ensino
  students_by_hex_year <- hexgrid_students_future |> 
    st_set_geometry(NULL) |> 
    filter(!str_detect(sg_etapa, "ESPECIAL"), ano <= 2040) |> 
    group_by(h3_parent, sg_etapa, ano) |> 
    summarise(n_estudantes = sum(n_estudantes, na.rm = TRUE), .groups = "drop")
  
  # calcular diferença
  students_by_hex_year <- students_by_hex_year |> 
    group_by(h3_parent, sg_etapa) |> 
    arrange(ano) |> 
    mutate(diff_estudantes = n_estudantes - first(n_estudantes))
  
  # juntar geometria e dados
  pop_hexgrid <- left_join(hexgrid_res_08, students_by_hex_year, by = c("h3_address" = "h3_parent")) |> 
    filter(ano > 2022) |> 
    drop_na() |> 
    st_as_sf()
    
  p <- pop_hexgrid |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 10, progress = "none") +
    geom_sf(aes(fill = diff_estudantes), color = NA) +
    geom_sf(data = boundary, fill = NA, color = "grey40") +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    scale_fill_gradient2(low = "orangered3", high = "steelblue3",
                         limits = c(-425, 425)
                         # breaks = c(-0.086, 0, 0.1, 0.178)
                         ) +
    theme_minimal() +
    labs(fill = "Diferença\npara 2022") +
    facet_grid(sg_etapa ~ ano, switch = "y")
  
  figure_path <- "output/report_03/fig_xx_future_pop_estimates.png"
  ggsave(plot = p, filename = figure_path,
         width = 16, height = 9, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
  
}
  
# central_neighborhoods <- tar_read(central_neighborhoods)
create_map_central_neighborhoods <- function(central_neighborhoods) {
  
  p <- central_neighborhoods |> 
    ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 14, progress = "none") +
    geom_sf(aes(color = borough)) +
    annotation_scale(style = "ticks", location = "br") +
    coord_sf(datum = NA) +
    theme_minimal() +
    labs(color = "Bairros")
  
  figure_path <- "output/report_03/fig_xx_central_neighborhoods.png"
  ggsave(plot = p, filename = figure_path,
         width = 16, height = 9, units = "cm", dpi = 300, scale = 1.2)
  
  return(figure_path)
  
  
}
# state_schools$co_distrito |> unique() |> sort()

# rooms_fixed |>
#   mutate(diff = qt_area_util_1_2_adj - qt_real_pessoa_adj) |>
#   ggplot(aes(x=qt_area_util_1_2_adj, y=qt_real_pessoa_adj)) +
#   geom_point(alpha = 0.1) +
#   coord_fixed() +
#   geom_abline()

# rooms_geo |>
#   ggplot(aes(x=qt_area_util, y=qt_real_pessoa)) +
#   geom_point() +
#   scale_color_brewer(palette = "Set1") +
#   scale_x_log10(breaks = c(1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 35000),
#                 labels = scales::comma_format()) +
#   scale_y_log10(breaks = c(1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 35000),
#                 labels = scales::comma_format()) +
#   coord_equal()
# 
# 
# rooms_geo |>
#   filter(is.na(qt_area_util) | is.na(qt_real_pessoa)) |>
#   View()
# 
# 
# rooms_step3 |>
#   ggplot(aes(x=qt_area_util_adj, y=qt_real_pessoa_adj)) +
#   geom_point() 
# scale_x_log10() +
#   scale_y_log10()
# scale_x_continuous(breaks = seq(0, 40000, 50)) +
#   scale_y_continuous(breaks = seq(0, 70, 5), minor_breaks = seq(0, 70, 1))
# 
# rooms_step3 |> 
#   select(qt_area_util, qt_real_pessoa) |> 
#   drop_na() |> 
#   cor()
# 
# rooms_geo |>
#   filter(cd_escola == 700010) |>
#   View()
# 
# ambientes_df |>
#   filter(CODESC == 700010) |>
#   View()
# 
# students_geo |>
#   filter(cd_escola == "700010") |>
#   View()
# 
# rooms_step4 |>
#   count(qt_real_pessoa, sort = T)
# 
# rooms_step4 |>
#   count(qt_area_util, sort = T)
# 
# filter(qt_real_pessoa < 60) |>
#   ggplot(aes(qt_real_pessoa)) + geom_histogram() +
#   scale_x_continuous(breaks = seq(0, 150, 5))
# 
# rooms_step4
# 
# rooms_step4 |>
#   filter(qt_area_util < 60) |>
#   ggplot(aes(qt_real_pessoa)) + geom_histogram() +
#   scale_x_continuous(breaks = seq(0, 150, 5))
# 
# rooms_step4 |>
#   select(qt_area_util, qt_real_pessoa) |>
#   summary()

