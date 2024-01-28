# TODO: DOCUMENTATION
visualize_numeric_attrs <- function(site_attr_data) {
  # Plotting attribute distributions per cluster
  data_to_plot <- site_attr_data %>% 
    dplyr::select(site_category_fact, where(is.numeric)) %>%
    pivot_longer(cols = -site_category_fact, 
                 names_to = 'attribute')
  
  ggplot(data_to_plot, aes(x = site_category_fact, y = value)) +
    geom_boxplot(aes(fill = site_category_fact)) + 
    facet_wrap(vars(attribute), scales = 'free_y') +
    theme_bw() +
    scico::scale_fill_scico_d() +
    xlab('Site Category') + ylab('Attribute value (various scales and units)') +
    ggtitle('Attributes by site categorization',
            subtitle = 'Note that we have not yet implemented baseflow methods') +
    theme(strip.background = element_rect(fill = 'white', color = 'transparent'))
}
