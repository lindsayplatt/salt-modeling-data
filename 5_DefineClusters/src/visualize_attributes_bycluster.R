
# TODO: DOCUMENTATION
visualize_numeric_attrs_by_cluster <- function(site_attr_data) {
  # Plotting attribute distributions per cluster
  data_to_plot <- site_attr_data %>% 
    dplyr::select(clusterf, where(is.numeric)) %>%
    pivot_longer(cols = -clusterf, 
                 names_to = 'attribute')
  
  ggplot(data_to_plot, aes(x = clusterf, y = value)) +
    geom_boxplot(aes(fill = clusterf)) + 
    facet_wrap(vars(attribute), scales = 'free_y') +
    theme_bw() +
    scico::scale_fill_scico_d() +
    xlab('Cluster (out of 7)') + ylab('Attribute value (various scales and units)') +
    ggtitle('Attributes by cluster group',
            subtitle = 'Note that some cluster groups are not represented (those clusters were not the most frequent for any site)') +
    theme(strip.background = element_rect(fill = 'white', color = 'transparent'))
}

# TODO: DOCUMENTATION
visualize_categorical_attrs_by_cluster <- function(site_attr_data, plot_percent = FALSE) {
  
  # Plotting attribute distributions per cluster
  data_to_plot <- site_attr_data %>% 
    dplyr::select(clusterf, where(is.character)) %>%
    pivot_longer(cols = -clusterf, 
                 names_to = 'attribute', 
                 values_to = 'trend_result') %>% 
    # Count total values per bar
    group_by(clusterf, attribute) %>% 
    mutate(total_count = n()) %>% 
    # Add counts per cluster for each trend results
    group_by(clusterf, attribute, total_count, trend_result) %>% 
    tally() %>% 
    ungroup() %>% 
    # Convert to percentages
    mutate(perc = round(n / total_count * 100, 2)) %>% 
    rowwise() %>% 
    mutate(yval = ifelse(plot_percent, perc, n))
  
  ggplot(data_to_plot, aes(x = clusterf, y = yval, fill = trend_result)) +
    geom_col() + 
    facet_wrap(vars(attribute)) +
    theme_bw() +
    ylab(ifelse(plot_percent, 
                "Percent of sites per cluster", 
                "Number of sitse per cluster")) +
    scico::scale_fill_scico_d(name = "Trend result", palette = 'vikO',
                              begin = 0.25, end = 0.75, direction = -1) +
    xlab('Cluster (out of 7)') + 
    ggtitle('Trend results by cluster group',
            subtitle = 'Note that some cluster groups are not represented (those clusters were not the most frequent for any site)') +
    theme(strip.background = element_rect(fill = 'white', color = 'transparent'))
  
}

# TODO: Use ANOVA to evaluate which attributes are different for clusters??
# aov_output <- aov(avgSoilStorage ~ clusterf,
#                   data=tar_read(p5_site_attr_rf))
# # Test for normality of residuals
# hist(aov_output$residuals)
# 
# summary(aov_output)
