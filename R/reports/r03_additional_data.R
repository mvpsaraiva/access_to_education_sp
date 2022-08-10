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