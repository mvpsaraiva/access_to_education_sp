# rooms_geo <- tar_read(rooms_geo)
fix_room_capacity <- function(rooms_geo) {

  ## correct outliers
  
  # rooms too big, that seems to be an error when parsing decimal numbers
  # for example, 25.0 m2 may have been exported as 250
  rooms_step1 <- rooms_geo |>
    mutate(qt_area_util_adj = case_when(qt_area_util > 7500 ~ qt_area_util / 1000,
                                        (qt_area_util > 450 & qt_real_pessoa <= 10) | qt_area_util >= 1000 ~ qt_area_util / 100,
                                        (qt_area_util > 80 & qt_real_pessoa <= 18) | qt_area_util > 220 ~ qt_area_util / 10,
                                        TRUE ~ qt_area_util))


  # too many students per room, seems to be the same problem as above
  rooms_step2 <- rooms_step1 |>
    mutate(qt_real_pessoa_adj = case_when(qt_real_pessoa > 10000 ~ qt_real_pessoa / 1000,
                                          qt_real_pessoa > 1000 ~ qt_real_pessoa / 100,
                                          qt_real_pessoa > 100 & qt_area_util_adj < 100 ~ qt_real_pessoa / 10,
                                          TRUE ~ qt_real_pessoa))
  
  # fill NA values for area and students
  # use the most common values for each
  rooms_modes <- rooms_step2 |> 
    count(dc_tipo_ambiente, qt_area_util_adj, qt_real_pessoa_adj, sort = T) |> 
    drop_na() |> 
    group_by(dc_tipo_ambiente) |> 
    arrange(desc(n)) |> 
    slice(1)
  
  rooms_step3 <- rooms_step2 |> 
    left_join(rooms_modes, by = "dc_tipo_ambiente") |> 
    mutate(qt_area_util_adj = if_else(is.na(qt_area_util_adj.x), qt_area_util_adj.y, qt_area_util_adj.x),
           qt_real_pessoa_adj = if_else(is.na(qt_real_pessoa_adj.x), qt_real_pessoa_adj.y, qt_real_pessoa_adj.x)) |> 
    select(-ends_with(".x"), -ends_with(".y"), -n)

  # recalculate area / 1.2
  rooms_step4 <- rooms_step3 |> 
    mutate(qt_area_util_1_2_adj = trunc(qt_area_util_adj / 1.2))
  
  return(rooms_step4)
  
}
