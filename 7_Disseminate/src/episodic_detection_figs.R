
#' @title Make a bunch of mini plots identifying SpC at every site
#' @description Plot SpC values to visualize how we identified whether a site
#' was episodic or not. Creates groups of facet ggplots.
#' 
#' @param ts_sc a tibble of time series data for specific conductance has the  
#' columns `site_no`, `dateTime`, `SpecCond`
#' @param sites_episodic a character vector giving site numbers that met criteria
#' for being an "episodic site". See targets in `4_EpisodicSalinization`. 
#' @param winter_months a numeric vector indicating which months should be 
#' considered "winter"; defaults to `c(12,1,2,3)` or Dec, Jan, Feb, and Mar.
#' @param nrow numeric value indicating the number of rows of facets to plot; defaults to 5
#' 
#' @returns a list of ggplots
#' 
create_episodic_plotlist <- function(ts_sc, sites_episodic, winter_months = c(12,1,2,3), nrow=5) {
  
  # Prepare information for ordering sites and adding horizontal lines to each plot
  site_category <- ts_sc %>% 
    mutate(is_winter = month(dateTime) %in% winter_months) %>% 
    group_by(site_no) %>% 
    summarize(SpC75 = quantile(SpecCond, probs = 0.75, na.rm=TRUE),
              .groups = 'keep') %>% 
    # Order sites based on whether or not they were episodic
    mutate(site_category = ifelse(site_no %in% sites_episodic, 'Episodic', 'Not episodic')) %>% 
    arrange(site_category, site_no) %>% 
    # Setup site number as an ordered factor to control order of facets
    mutate(site_no_ord = factor(site_no, levels = .$site_no)) %>% 
    group_by(site_no_ord) %>% 
    # Prepare each site to be shown in a specific group's set of facets
    mutate(grp_num = ceiling(cur_group_id()/25)) %>% 
    ungroup()
  
  ts_sc_category <- ts_sc %>% 
    mutate(year = year(dateTime),
           is_winter = month(dateTime) %in% winter_months) %>% 
    left_join(site_category, by = 'site_no') %>% 
    # Add a column that gives the category to change the winter color depending
    # on whether or not that site was found to be episodic
    mutate(winter_behavior = ifelse(is_winter & site_category == 'Episodic', 
                                 'Winter date (episodic site)', 
                                 ifelse(is_winter & site_category == 'Not episodic', 
                                        'Winter date (not episodic site)', 
                                        'Non-winter date'))) %>% 
    # Now split SC ts data by group number for plotting
    split(.$grp_num)
  
  # Now split SC ts category data by group number for plotting
  site_category <- site_category %>% split(.$grp_num)
  
  plot_episodic_facets <- function(ts_sc, ts_sc_info) {
    ggplot(data = ts_sc) +
      geom_path(aes(x = dateTime, y = SpecCond, color = winter_behavior, group=year), na.rm = TRUE) +
      scale_color_manual(values = c(`Winter date (episodic site)` = '#0b5394', 
                                    `Winter date (not episodic site)` = 'grey15',
                                    `Non-winter date` = 'grey70')) +
      facet_wrap(vars(site_no_ord), scales='free', nrow=nrow) +
      geom_hline(data = ts_sc_info, aes(yintercept = SpC75), linetype = 'dashed') + 
      ylab('Specific conductance, µS/cm at 25°C') + 
      theme_bw() +
      theme(text = element_text(size=10), 
            axis.text.x = element_text(size=8, angle=20, hjust=1),
            axis.title.y = element_text(vjust = 1.5),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=12),
            legend.position = "bottom",
            strip.background = element_blank(),
            strip.text = element_text(size = 10, face = 'bold'),
            strip.clip = "off",
            plot.margin = unit(c(0.5,0.5,0,0.5), 'lines'))
  }
  
  # Now plot all site's data and save as a list object
  plotlist_out <- map2(ts_sc_category, site_category, plot_episodic_facets)
  return(plotlist_out)
}

#' @title Create a multipanel figure showing how sites line up with the defined criteria
#' @description Plot metric values for each site compared to each of the crtieria
#' used to define what sites qualified as episodic or not. Use this to show how 
#' some sites may be reclassified if we shifted the criteria. This figure was
#' paired with all of the time series to visually validate classifications.
#' 
#' Note that all criteria values are manually entered as horizontal lines and would
#' need to be updated if the values in `4_EpisodicSalinization` changed.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param ts_sc_peak_summary_info a tibble summarizing the values calculated for
#' each of the episodic site classification qualifying criteria, see the function
#' `summarize_salt_peaks()`. Should include the columns `site_no`, `peaks_perc_winterYes` 
#' (the percent of peaks that occurred during winter), `sc_perc_diff` (the percent
#' difference between mean winter and non-winter SpC), `perc_winter_max_higher` 
#' (the percent of years where the annual maximum SpC occurred in winter), and
#' `is_salt_site` (a logical indicating whether a site met the criteria or not).
#' 
#' @returns a character string giving the location of the saved figure file
#' 
generate_episodic_feature_space_figure <- function(out_file, ts_sc_peak_summary_info) {
  
  # Prepare data 
  feature_space_info <- ts_sc_peak_summary_info %>% 
    mutate(episodic_classification = ifelse(is_salt_site, 'Episodic', 'Not episodic')) %>% 
    select(site_no, episodic_classification, everything(), -is_salt_site)
  
  # Create first matchup of the three criteria values used (1 and 2)
  feature_space1_with_legend <- ggplot(feature_space_info, aes(x = peaks_perc_winterYes, y = sc_perc_diff)) +
    
    ylab('Percent winter and non-winter SpC difference') +
    geom_hline(yintercept = 0.10, linewidth = 0.5, linetype = 'dashed', color = 'grey50') + 
    xlab('Percent of global peaks in winter') +
    geom_vline(xintercept = 0.40, linewidth = 0.5, linetype = 'dashed', color = 'grey50') + 
    
    geom_point(aes(color = episodic_classification), size = 1.25, alpha = 0.75, shape=16) +
    scale_color_manual(name = '', values = c(Episodic = '#0b5394', `Not episodic` = 'grey40')) +
    
    theme_bw() +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    theme(panel.grid = element_blank(),
          text = element_text(size=6),
          legend.direction = "horizontal",
          legend.text = element_text(size=8),
          legend.key.size = unit(7, units = 'points'),
          legend.key.spacing = unit(0.5, units = 'lines'),
          legend.justification = 'center') 
  
  # Extract just the legend to use as a shared legend with cowplot later
  the_legend <- cowplot::get_legend(feature_space1_with_legend)
  feature_space1 <- feature_space1_with_legend + theme(legend.position = 'none')
  
  # Now generate the second matchup of the three criteria values used (1 and 3)
  feature_space2 <- ggplot(feature_space_info, aes(x = perc_winter_max_higher, y = sc_perc_diff)) +
    
    ylab('Percent winter and non-winter SpC difference') +
    geom_hline(yintercept = 0.10, linewidth = 0.5, linetype = 'dashed', color = 'grey50') + 
    xlab('Percent of annual maximums in winter') +
    geom_vline(xintercept = 0.75, linewidth = 0.5, linetype = 'dashed', color = 'grey50') + 
    
    geom_point(aes(color = episodic_classification), size = 1.25, alpha = 0.75, shape=16) +
    scale_color_manual(name = '', values = c(Episodic = '#0b5394', `Not episodic` = 'grey40')) +
    
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size=6),
          # Skip legend on this one to use shared legend
          legend.position = 'none')
  
  # Now the third matchup of the three criteria values used (2 and 3)
  feature_space3 <- ggplot(feature_space_info, aes(x = perc_winter_max_higher, y = peaks_perc_winterYes)) +
    
    ylab('Percent of global peaks in winter') +
    geom_hline(yintercept = 0.40, linewidth = 0.5, linetype = 'dashed', color = 'grey50') + 
    xlab('Percent of annual maximums in winter') +
    geom_vline(xintercept = 0.75, linewidth = 0.5, linetype = 'dashed', color = 'grey50') + 
    
    geom_point(aes(color = episodic_classification), size = 1.25, alpha = 0.75, shape=16) +
    scale_color_manual(name = '', values = c(Episodic = '#0b5394', `Not episodic` = 'grey40')) +
    
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size=6),
          # Skip legend on this one to use shared legend
          legend.position = 'none') 
  
  # Now combine them into a single figure with multiple, labeled panels
  png(out_file, width = 6.5, height = 3.25, units = 'in', res = 500)
  print(cowplot::plot_grid(feature_space1, feature_space2, feature_space3, 
                           NULL, the_legend, NULL,
                           nrow=2, label_size=10,
                           rel_heights = c(0.90, 0.10), 
                           labels=sprintf('(%s)', letters[1:3])))
  dev.off()
  
  return(out_file)
}
