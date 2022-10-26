
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
      ggsave(out_file, p, height = 16, width = 10)
      return(out_file)
    }, format="file"
  )
)
