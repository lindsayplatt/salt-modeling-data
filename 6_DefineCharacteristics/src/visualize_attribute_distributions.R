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
    scico::scale_fill_scico_d(begin = 0, end = 0.75) +
    xlab('Site Category') + ylab('Attribute value (various scales and units)') +
    ggtitle('Attributes by site categorization') +
    theme(strip.background = element_rect(fill = 'white', color = 'transparent'))
}

# TODO: DOCUMENTATION
add_state_basemap <- function(states_to_include) {
  states_sf <- usmap::us_map(include = states_to_include) %>% 
    st_as_sf(coords = c('x', 'y'), 
             crs = usmap::usmap_crs()) %>% 
    group_by(group, abbr) %>% 
    summarise(geometry = st_combine(geometry), .groups="keep") %>%
    st_cast("POLYGON")
  
  geom_sf(data=states_sf, fill = 'white', color = 'darkgrey')
}

# TODO: DOCUMENTATION
visualize_catgory_sites_map <- function(site_attributes, sites_sf, states_to_include) {
  
  category_sites_sf <- sites_sf %>% 
    left_join(site_attributes, by = 'site_no') %>% 
    st_transform(crs=usmap::usmap_crs()) %>% 
    filter(!is.na(site_category_fact)) %>% 
    mutate(site_category = as.character(site_category_fact)) %>% 
    group_by(site_category) %>% 
    mutate(n_category = n()) %>% 
    ungroup() %>% 
    mutate(category_title = sprintf('%s (n = %s)', site_category, n_category))
  
  ggplot() +
    add_state_basemap(states_to_include) +
    geom_sf(data=category_sites_sf, 
            aes(fill = site_category_fact), 
            alpha=0.75, shape=24, size=2) +
    guides(color = 'none') +
    scico::scale_fill_scico_d(begin = 0, end = 0.75) +
    facet_wrap(vars(category_title), ncol=2) +
    theme_void()
  
}
