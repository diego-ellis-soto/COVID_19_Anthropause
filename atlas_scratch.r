testdf <- tmp_move[[1]] %>% 
  as.data.frame() %>% 
  rename(x = location.long,
         y = location.lat,
         time = timestamps) %>% 
  mutate(id = 1)

o <- atl_res_patch(testdf)

patch_sf_data <- atl_patch_summary(o,
                                   which_data = "spatial",
                                   buffer_radius = 20
)
sf::st_crs(patch_sf_data) <- 4326
plot(patch_sf_data)
