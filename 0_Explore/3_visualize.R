
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
        filter(daily_stat == "mean") %>% 
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
        geom_sf(data = conus_sf, fill="#e9e9e9", color = "white") +
        geom_sf(aes(fill = sc_cat), shape=21, color = "#b8b8b8", alpha=0.75, size = 3) +
        scico::scale_fill_scico_d(palette = 'imola', end = 0.90, 
                                   name = 'Specific\nconductance\n(μS/cm @ 25°C)') +
        # facet_grid(daily_stat ~ .) +
        # ggtitle('Mean specific conductance for NWIS sites with at least 10 years of continuous data',
        #         sprintf('From %s to %s, %s total sites',
        #                 start_date, end_date, length(unique(site_mean_df$site_no)))) +
        theme_void() + coord_sf()
      ggsave(out_file, p, height = 5.5, width = 10, bg="white")
      return(out_file)
    }, format="file"
  ),
  
  ##### Plots for groundwater SC trends #####
  
  # TODO: Turn bars into something more useful - like single values or ratios of +/- rather than bar heights
  tar_target(lm_plots_ls, calc_and_plot_trend(sc_trend_data_lm, "LM", subtitle_caveat = "Dugan et al., 2017 salt states")),
  tar_target(ma_plots_ls, calc_and_plot_trend(sc_trend_data_ma, "MA", subtitle_caveat = "Dugan et al., 2017 salt states")),
  tar_target(mk_plots_ls, calc_and_plot_trend(sc_trend_data_mk, "MK", subtitle_caveat = "Dugan et al., 2017 salt states")),
  
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
  
  # Plot site salt values
  tar_target(map_sites_salt_png, {
    sites_sf <- st_transform(q_sc_sites_sf, usmap_crs()) %>% 
      left_join(sites_with_salt, by = "site_no") %>% 
      rename(salt_5km = road_salt_2015_aggr) %>% 
      filter(!is.na(salt_5km)) %>% 
      # According to the data release it is in pounds, so let's change to tons.
      mutate(salt_5km_kg = salt_5km*0.4536) # lbs --> kg 
    
    out_file <- '3_visualize/out/salt_applied_map.png'
    p <- ggplot() +
      geom_sf(data=conus_nosalt_sf, fill='#b8b8b8', color=NA) +
      geom_sf(data=conus_salt_sf, fill='#f4f4f4', color='#b8b8b8') +
      geom_sf(data=sites_sf, aes(color = log10(salt_5km_kg)), shape=17, size=2, alpha=0.75) +
      scico::scale_color_scico(name = 'Salt applied\nin 2015\n(logged kg)', palette = 'batlow') +
      # ggtitle('NWIS specific conductance sites with road salt applied in 5 km radius.',
      #         sprintf('%s sites', nrow(sites_sf))) +
      theme_void() + 
      coord_sf()
    ggsave(out_file, p, height = 5.5, width = 10, bg="white")
    return(out_file)
  }, format = 'file'),
  
  # Plot sites Ts data
  tar_target(map_sites_trans, {
    sites_sf <- st_transform(q_sc_sites_sf, usmap_crs()) %>% 
      left_join(q_sc_sites_info, by = 'site_no')
    # huc04s_sf <- st_transform(nhd_huc04s_sf, usmap_crs())
    
    ggplot() +
      geom_sf(data=conus_nosalt_sf, fill='#b8b8b8', color=NA) +
      geom_sf(data=conus_salt_sf, fill='#f4f4f4', color='#898989') +
      # geom_sf(data=huc04s_sf, fill='#daeaf3', color='#38799f', size=3, alpha = 0.65) +
      geom_sf(data=sites_sf, aes(color=trans_PERCENTILE), shape=17) +
      scico::scale_color_scico(palette = 'batlow', direction=-1) +
      theme_void()
  }),
  
  # Plot sites DTW data
  tar_target(map_sites_dtw, {
    sites_sf <- st_transform(q_sc_sites_sf, usmap_crs()) %>% 
      left_join(q_sc_sites_info, by = 'site_no')
    # huc04s_sf <- st_transform(nhd_huc04s_sf, usmap_crs())
    
    ggplot() +
      geom_sf(data=conus_nosalt_sf, fill='#b8b8b8', color=NA) +
      geom_sf(data=conus_salt_sf, fill='#f4f4f4', color='#898989') +
      # geom_sf(data=huc04s_sf, fill='#daeaf3', color='#38799f', size=3, alpha = 0.65) +
      geom_sf(data=sites_sf, aes(color=dtw_PERCENTILE), shape=17) +
      scico::scale_color_scico(palette = 'batlow', direction=-1) +
      theme_void()
  }),
  
  # Map road salt info per HUC
  tar_target(map_huc04s_roadsalt, {
    
    huc04s_sf <- st_transform(nhd_huc04s_sf, usmap_crs()) %>% 
      left_join(road_salt_2015_huc_summary, by = c('huc4' = 'HUC04'))
    
    ggplot() +
      geom_sf(data=conus_nosalt_sf, fill='#b8b8b8', color=NA) +
      geom_sf(data=conus_salt_sf, fill='#f4f4f4', color='#898989') +
      geom_sf(data=huc04s_sf, aes(fill=salt_sum), color='black') +
      scale_fill_scico(palette = 'davos', begin=0.20, end=0.90, name = 'Salt applied in 2015 (lbs)') +
      theme_void() + ggtitle('Sum of application values per HUC04')
  }),
  
  tar_target(plot_saltapp_vs_gwconnect, {
    
    trend_data <- sc_trends_with_site_info %>% 
      filter(trend %in% c('positive', 'negative'))
    
    p_ts <- ggplot(trend_data, aes(x=trans_PERCENTILE, y = salt_mean,
                          color = trend)) +
      geom_point() + 
      facet_grid(trend ~ hydro_cond) +
      xlab("Transmissivity, %ile") +
      ylab("Mean salt application, lbs") 
    
    p_dtw <- ggplot(trend_data, aes(x=dtw_PERCENTILE, y = salt_mean,
                           color = trend)) +
      geom_point() + 
      facet_grid(trend ~ hydro_cond) +
      xlab("Depth to water table, %ile") +
      ylab("Mean salt application, lbs")
    
    cowplot::plot_grid(
      p_ts + theme(legend.position="none"), 
      p_dtw + theme(legend.position="none", 
                    axis.title.y = element_blank()), 
      get_legend(p_dtw),
      nrow=1, rel_widths = c(3, 3, 0.5))
  }),
  
  tar_target(map_sites_gwsig_png, {
    
    sites_to_plot <- conus_sc_sites_sf %>% 
      # Filter to only sites with salt applied in 2015
      filter(site_no %in% sites_with_salt$site_no) %>%
      # Further filter only to sites that have a GW category from Hare et al
      left_join(hare_et_al, by="site_no") %>% 
      filter(!is.na(GW_cat)) %>% 
      mutate(GW_cat = gsub(' Signature', '', GW_cat))
    
    conus_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
    p<-ggplot(sites_to_plot) +
      geom_sf(data = conus_sf, fill="#e9e9e9", color = "white") +
      geom_sf(aes(fill = GW_cat), alpha=0.65, size=2.5, shape=21, color="#b8b8b8") +
      scico::scale_fill_scico_d(palette = 'berlin', begin = 0.15, end = 0.85, 
                                name = 'Groundwater\nsignature') +
      # ggtitle('',
      #         sprintf('Dominant groundwater signature per Hare et al 2021; %s total sites',
      #                 length(unique(sites_to_plot$site_no)))) +
      theme_void() + coord_sf()
    ggsave('3_visualize/out/gwsig_map.png', p, height = 5.5, width = 10, bg="white")
  }),
  
  tar_target(timeseries_random_sites_png, {
    
    # Select 10 random sites from those with a lot of records
    # to create timeseries charts for
    sites_longest_record <- conus_sc_data_plot_ready_daily_means_normalized %>% 
      group_by(site_no) %>% 
      tally() %>% 
      arrange(desc(n)) %>% 
      head(50) 
    
    set.seed(14)
    random_sites <- sites_longest_record %>% 
      sample_n(10) %>% 
      pull(site_no)
    
    site_info <- conus_sc_site_metadata %>% 
      filter(site_no %in% random_sites) %>% 
      dplyr::select(site_no, drain_area_va)
    
    # Calculate a 90-day rolling avg 
    conus_sc_data_rollmeans <- conus_sc_data_plot_ready_daily_means_normalized %>% 
      filter(site_no %in% random_sites) %>% 
      left_join(site_info, by = c("site_no")) %>% 
      arrange(state_abbr, site_no, drain_area_va) %>% 
      mutate(facet_label = sprintf("USGS Site %s\nState: %s\nCatchment area (sq mi): %s",
                                   site_no, state_abbr, drain_area_va)) %>% 
      mutate(facet_label = factor(facet_label, ordered = TRUE, labels = unique(.$facet_label))) %>% 
      group_by(site_no, facet_label) %>% 
      mutate(mean_sc_90day = zoo::rollmean(specific_conductance, k=90,
                                           fill = NA, align = "left"))
    
    # Plot the 90-day rolling average
    p <- ggplot(conus_sc_data_rollmeans, 
                aes(x = dateTime, y = mean_sc_90day)) + 
      geom_point(size=0.25) + 
      facet_grid(facet_label ~ ., scales = "free_y") + 
      ylab("Specific\nconductance\n(μS/cm @ 25°C)")+
      xlab('Date') +
      # ggtitle('Rolling 90-day mean specific conductance') + 
      theme_bw() +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
            strip.text.y.right = element_text(angle = 0, vjust = 0.5))
    
    ggsave("3_visualize/out/sc_timeseries.png", p, units = 'px', height = 1000, 
           width = 1200, dpi = 100)  
  })
  
)
