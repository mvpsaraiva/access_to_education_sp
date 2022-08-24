# ######  Map origin-destination flows  ------------------
# 
# # h3 centroids coordinates
# h3_od_point <- h3jsr::h3_to_point(h3_address = grid8$id_hex, simple = F)
# h3_od_point <- sfheaders::sf_to_df(h3_od_point, fill = T)
# setDT(h3_od_point)
# 
# # add income info
# setDT(grid8)
# df_od[grid8, on=c('pickup_hex8'='id_hex'),
#       c('quintil', 'decil') := list(i.quintil, i.decil)]
# 
# 
# # add coordinates to od matrix
# df_od[h3_od_point, on=c('pickup_hex8'='h3_address'),
#       c('pickup_x', 'pickup_y') := list(i.x, i.y)]
# 
# df_od[h3_od_point, on=c('dropoff_hex8'='h3_address'),
#       c('dropoff_x', 'dropoff_y') := list(i.x, i.y)]
# 
# 
# 
# 
# 
# ###### build igraph network   ------------------
# 
# df_od[, date_block_2019 := NULL]
# g <- igraph::graph_from_data_frame(d = df_od,
#                                    vertices = h3_od_point[, .(h3_address,x,y)],
#                                    directed = T)
# 
# g_poor <- igraph::graph_from_data_frame(d = subset(df_od, decil < 5),
#                                          vertices = h3_od_point[, .(h3_address,x,y)],
#                                          directed = T)
# 
# g_rich <- igraph::graph_from_data_frame(d = subset(df_od, decil >8),
#                                          vertices = h3_od_point[, .(h3_address,x,y)],
#                                          directed = T)
# 
# 
# 
# 
# # coordinates of vertices
# xy <- cbind(V(g)$x, V(g)$y)
# 
# 
# ### Edge Bundling
# 
# # Edge-Path Bundling
# pbundle <- edge_bundle_path(g, xy,
#                             max_distortion = 12,
#                             weight_fac =  4,
#                             segments = 20)
# 
# pbundle_rich <- edge_bundle_path(g_rich , xy,
#                             max_distortion = 12,
#                             weight_fac =  4,
#                             segments = 20)
# 
# pbundle_poor <- edge_bundle_path(g_poor, xy,
#                             max_distortion = 12,
#                             weight_fac =  4,
#                             segments = 20)
# 
# 2+2
# 
# # fbundle <- edge_bundle_force(g, xy, compatibility_threshold = 0.6)
# # sbundle <- edge_bundle_stub(g, xy)
# # hbundle <- edge_bundle_hammer(g, xy, bw = 0.7, decay = 0.5)
# 
# 2+2
# 
# 
# setDT(pbundle)[, cat := 'all']
# setDT(pbundle_rich)[, cat := 'rich']
# setDT(pbundle_poor)[, cat := 'poor']
# 
# df_bundle <- rbind(pbundle, pbundle_rich, pbundle_poor)
# 
# ### Figure -----------------
# fig_od_all <- ggplot() +
#           geom_sf(data=muni , fill='gray10', color=NA) +
#           geom_path(data = pbundle, aes(x, y, group = group),
#                     col = "#9d0191", size = 0.009, alpha=.05) +
#            geom_path(data = pbundle, aes(x, y, group = group),
#                      col = "white", size = 0.00009, alpha=.01) +
#           theme_classic() +
#           theme(axis.line=element_blank(),
#                 axis.text=element_blank(),
#                 axis.ticks=element_blank(),
#                 axis.title=element_blank())
# 
# fig_od_rich <- ggplot() +
#                 geom_sf(data=muni , fill='gray10', color=NA) +
#                 geom_path(data = pbundle_rich, aes(x, y, group = group),
#                           col = "#9d0191", size = 0.009, alpha=.05) +
#                 geom_path(data = pbundle_rich, aes(x, y, group = group),
#                           col = "white", size = 0.00009, alpha=.01) +
#                 theme_classic() +
#                 theme(axis.line=element_blank(),
#                       axis.text=element_blank(),
#                       axis.ticks=element_blank(),
#                       axis.title=element_blank())
# 
# fig_od_poor <- ggplot() +
#                 geom_sf(data=muni , fill='gray10', color=NA) +
#                 geom_path(data = pbundle_poor, aes(x, y, group = group),
#                           col = "#9d0191", size = 0.009, alpha=.05) +
#                 geom_path(data = pbundle_poor, aes(x, y, group = group),
#                           col = "white", size = 0.00009, alpha=.01) +
#                 theme_classic() +
#                 theme(axis.line=element_blank(),
#                       axis.text=element_blank(),
#                       axis.ticks=element_blank(),
#                       axis.title=element_blank())
# 
# ggsave(fig_od_all,
#        file='../figures/context/od_edges_all_05_01.png',
#        dpi=200,
#        width = 15,
#        height = 13,
#        units = "cm",
#        bg= '#FFFFFF')
# 
# ggsave(fig_od_rich,
#        file='../figures/context/od_edges_rich_05_01.png',
#        dpi=200,
#        width = 15,
#        height = 13,
#        units = "cm",
#        bg= '#FFFFFF')
# 
# ggsave(fig_od_poor,
#        file='../figures/context/od_edges_poor_05_01.png',
#        dpi=200,
#        width = 15,
#        height = 13,
#        units = "cm",
#        bg= '#FFFFFF')
# 
# 
# 
# 
# 
# fig_od <- ggplot() +
#   geom_tile(data = base_df, aes(x, y, fill = hex), color=NA) +
#   scale_fill_identity() +
#   geom_sf(data=muni , color='gray50', fill=NA) +
#   geom_path(data = df_bundle, aes(x, y, group = group),
#             col = "#019d9d", size = 0.009, alpha=.05) + # verde 019d9d ROSA 9d0191
#   geom_path(data = df_bundle, aes(x, y, group = group),
#             col = "white", size = 0.00009, alpha=.01) +
#   facet_wrap(~cat, nrow = 3) +
#   theme_classic() +
#   theme(axis.line=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title=element_blank())
# 
# ggsave(fig_od,
#        file='../figures/context/od_edges_05_01_tile_verde.png',
#        dpi=200,
#        width = 15,
#        height = 23,
#        units = "cm",
#        bg= '#FFFFFF')


# Flows -------------------------------------------------------------------

# students_processed <- tar_read(students_processed)
# schools_geo <- tar_read(schools_geo)
# resolution <- 8
build_flows <- function(students_processed, schools_geo, resolution) {
  # select relevant columns from students
  students_df <- students_processed |> 
    select(cd_aluno, cd_escola, sg_etapa, sg_serie_ensino,
           dc_raca_cor, faixa_renda, teg, dentro_muni, lat, lon)
  
  # select relevant columns from schools
  schools_df <- schools_geo |> 
    select(cd_escola, lat, lon)
  
  # join students and schools to build table of flows
  flows_df <- left_join(students_df, schools_df, by = "cd_escola",
                        suffix = c(".stu", ".sch"))
  
  # get origin hexagons at requested resolution
  origins_sf <- flows_df |> 
    select(cd_aluno, lat.stu, lon.stu) |> 
    st_as_sf(coords = c("lon.stu", "lat.stu"), crs = 4326)
  hex_orig <- h3jsr::point_to_h3(origins_sf, resolution)
  
  # get destination hexagons at requested resolution
  destinations_sf <- flows_df |> 
    select(cd_aluno, lat.sch, lon.sch) |> 
    st_as_sf(coords = c("lon.sch", "lat.sch"), crs = 4326)
  hex_dest <- h3jsr::point_to_h3(destinations_sf, resolution)
  
  # add hexagon ids to flows table
  flows_df$hex_orig <- hex_orig
  flows_df$hex_dest <- hex_dest
  
  # summarise flows by origin and destination hexagon and socioeconomic 
  # characteristics
  aggregated_flows_df <- flows_df |> 
    count(hex_orig, hex_dest, sg_etapa, sg_serie_ensino, 
          dc_raca_cor, faixa_renda, teg, dentro_muni)
  
  return(aggregated_flows_df)
}

# Flows -------------------------------------------------------------------

# flows <- tar_read(flows_res_08)
# boundary <- tar_read(boundary_muni)
create_map_flows <- function(flows) {
  # return(flows)
  # aggregate flows
  flows_df <- flows |>
    # remover viagens de fora do município
    filter(dentro_muni == TRUE) |>
    count(hex_orig, hex_dest, faixa_renda, wt = n, name = "weight")
  
  # get vertices coordinates
  hexes <- c(flows_df$hex_orig, flows_df$hex_dest) |> unique()
  hexes_df <- h3_to_point(hexes, simple = FALSE)
  hexes_df <- sfheaders::sf_to_df(hexes_df, fill = TRUE) |> select(h3_address, x, y)
  
  # build igraph
  g <- igraph::graph_from_data_frame(d = flows_df |> filter(teg == "NAO"),
                                     vertices = hexes_df,
                                     directed = T)
  
  # vertices coordinates
  xy <- cbind(V(g)$x, V(g)$y)
  
  # Edge-Path Bundling
  pbundle_priv <- edge_bundle_path(g, xy,
                                   max_distortion = 12,
                                   weight_fac =  4,
                                   segments = 20)
  
  # is_weighted(g)
  
  # fig_od_all <-
  ggplot() +
    geom_sf(data=boundary , fill='gray10', color=NA) +
    geom_path(data = pbundle_priv, aes(x, y, group = group),
              col = "#9d0191", size = 0.09, alpha=.2) +
    geom_path(data = pbundle_priv, aes(x, y, group = group),
              col = "white", size = 0.009, alpha=.1) +
    theme_classic() +
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank())
  
  
  
  flow_segments_df <- flows_df |> 
    left_join(hexes_df, by = c("hex_orig"="h3_address")) |> 
    left_join(hexes_df, by = c("hex_dest"="h3_address"),
              suffix = c(".o", ".d"))
  
  flow_segments_df |> 
    ggplot() +
    geom_sf(data=boundary , fill='gray60', color=NA) +
    geom_segment(aes(x=x.o, y=y.o, xend=x.d, yend=y.d),
                 col = "#9d0191", size = 0.09, alpha=.05)+
    facet_wrap(~faixa_renda) +
    theme_classic() +
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank())
  
  
}






