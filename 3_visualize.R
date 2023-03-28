
source('3_visualize/src/gw_sc_trend_plots.R')

p3_targets <- list(
  tar_target(conus_sc_ts_annual_normalized_png, {
    out_file <- "3_visualize/out/sc_ts_annual_normalized.png"
    p <- ggplot(conus_sc_data_plot_ready_annual_means_normalized, 
                aes(x = yr,
                    y = annual_mean_sc_norm,
                    color = annual_mean_sc_norm,
                    group = site_no
                )) +
      geom_line(alpha=0.25) +
      facet_geo(~ state_abbr, label = "name")  +
      scico::scale_color_scico(begin = 0.25) +
      ylab("Specific\nconductance\n(μS/cm @ 25°C)") +
      ggtitle('Annual mean spec conductivity data across CONUS from NWIS',
              sprintf('From %s to %s, %s total values',
                      start_date, end_date, nrow(conus_sc_data_plot_ready_annual_means_normalized))) +
      theme(panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            strip.text.x = element_text(size = 6)) +
      scale_x_continuous(n.breaks=3)
        # breaks = seq(as.numeric(substr(start_date, 1, 4)),
        #                               as.numeric(substr(end_date, 1, 4)))
    ggsave(out_file, p, height = 10, width = 16)
    return(out_file)
  }, format="file"),
  
  # Generate a map with points for each site representing mean salinity 
  # to parallel Hilary's figure of salinity data from WQP
  tar_target(conus_sc_sites_sf, 
             st_as_sf(conus_sc_sites_surface, 
                      coords = c('dec_long_va', 'dec_lat_va'),
                      crs = 4326) %>% 
               st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')),
  
  tar_target(conus_sc_map_png, {
      out_file <- "3_visualize/out/mean_sc_map.png"
      site_mean_df <- conus_sc_data_plot_ready %>% 
        group_by(site_no, daily_stat) %>% 
        summarize(mean_site_sc = mean(specific_conductance, na.rm=TRUE)) %>% 
        ungroup() %>% 
        mutate(sc_cat = cut(mean_site_sc, c(0, 10, 50, 100, 500, Inf),
                            c('0-10', '10-50', '50-100', '100-500', '>500')))
      combined_data <- site_mean_df %>% 
        left_join(conus_sc_sites_sf) %>% 
        sf::st_as_sf()
      conus_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
      p <- ggplot(combined_data) +
        geom_sf(data = conus_sf, fill="lightgrey") +
        geom_sf(aes(color = sc_cat), alpha=0.75) +
        scico::scale_color_scico_d(palette = 'imola', end = 0.98, 
                                   name = 'Specific\nconductance\n(μS/cm @ 25°C)') +
        facet_grid(daily_stat ~ .) +
        ggtitle('Mean spec conductivity for continuous NWIS sites',
                sprintf('From %s to %s, %s total sites',
                        start_date, end_date, nrow(conus_sc_sites_sf))) +
        theme_void() + coord_sf()
      ggsave(out_file, p, height = 16, width = 10, bg="white")
      return(out_file)
    }, format="file"
  ),
  
  ##### Plots for groundwater SC trends #####
  
  tar_target(lm_plots_ls, calc_and_plot_trend(sc_trend_data_lm, "LM", subtitle_caveat = "Dugan et al., 2017 salt states")),
  tar_target(ma_plots_ls, calc_and_plot_trend(sc_trend_data_ma, "MA", subtitle_caveat = "Dugan et al., 2017 salt states")),
  
  tar_target(trend_plots_compare_1_2, cowplot::plot_grid(lm_plots_ls[[1]], ma_plots_ls[[1]], 
                                                         lm_plots_ls[[2]], ma_plots_ls[[2]], nrow = 2)),
  tar_target(trend_plots_compare_3_4, cowplot::plot_grid(lm_plots_ls[[3]], ma_plots_ls[[3]],
                                                         lm_plots_ls[[4]], ma_plots_ls[[4]], nrow = 2)),
  tar_target(trend_plots_compare_5, cowplot::plot_grid(lm_plots_ls[[5]], ma_plots_ls[[5]], nrow = 1)),
  
  # Plot HUCs and sites
  tar_target(map_sites_huc04s, {
    sites_sf <- st_transform(q_sc_sites_sf, usmap_crs())
    huc04s_sf <- st_transform(nhd_huc04s_sf, usmap_crs())
    
    ggplot() +
      geom_sf(data=conus_nosalt_sf, fill='#b8b8b8', color=NA) +
      geom_sf(data=conus_salt_sf, fill='#f4f4f4', color='#898989') +
      geom_sf(data=huc04s_sf, fill='#daeaf3', color='#38799f', size=3, alpha = 0.65) +
      geom_sf(data=sites_sf, color='#d08b2c', shape=17) +
      theme_void()
  }),
  
  # Map road salt info per HUC
  tar_target(map_huc04s_roadsalt, {
    
    huc04s_sf <- st_transform(nhd_huc04s_sf, usmap_crs()) %>% 
      left_join(road_salt_2015_huc_summary, by = c('huc4' = 'HUC04'))
    
    ggplot() +
      geom_sf(data=conus_nosalt_sf, fill='#b8b8b8', color=NA) +
      geom_sf(data=conus_salt_sf, fill='#f4f4f4', color='#898989') +
      geom_sf(data=huc04s_sf, aes(fill=salt_max), color='black') +
      scale_fill_scico(palette = 'davos', name = 'Salt applied in 2015 (lbs)') +
      theme_void() 
  })
  
)
