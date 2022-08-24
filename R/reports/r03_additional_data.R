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
