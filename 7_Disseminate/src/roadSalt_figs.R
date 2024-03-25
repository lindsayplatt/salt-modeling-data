
#' @title Create a boxplot of road salt attributes by site model category
#' @description Show how road salt may differ in distribution between the episodic
#' classification, the baseflow classification, and the overall distribution.
#' 
#' @param site_attr_data a tibble with one row per site and any number of columns
#' giving different attributes; needs the columns `site_no` and `attr_[attribute]`
#' @param all_site_categories a tibble with one row per site per model to visualize.
#' Expects the columns `site_no`, `model`, `site_category`.
#' 
#' @returns a ggplot object showing boxplots by site category for different models
#' 
create_roadSalt_boxplot <- function(site_attr_data, all_site_categories) {
  
  # Make a data set that includes all sites data in one 'category'
  all_site_no_category <- site_attr_data %>% 
    mutate(model = 'none', site_category = 'All Sites') %>% 
    select(site_no, model, site_category)
  
  # Combine with road salt and order the models by factor
  all_road_salt_data <- all_site_categories %>% 
    bind_rows(all_site_no_category) %>% 
    left_join(site_attr_data, by = 'site_no') %>% 
    mutate(fill_col = case_when(
      model == 'episodic' ~ '#0b5394',
      model == 'baseflow' ~ '#b45f06',
      model == 'none' ~ 'grey50'))  %>% 
    mutate(site_category_fact = factor(site_category, 
                                       levels = c('All Sites', 'Episodic', 'Not episodic',
                                                  'positive', 'none', 'negative'))) 
  
  count_vals <- function(data) {
    tibble(y = 300000,
           label = sprintf('n = %s', length(data)))
  }
  
  ggplot(all_road_salt_data, 
         aes(x = site_category_fact,
             y = attr_roadSaltPerSqKm, 
             fill = fill_col)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_identity(guide = 'none') +
    stat_summary(fun.data = count_vals, geom = "text", vjust=-1) +
    xlab('') + ylab(expression("Road salt applied per "~km^2~", kg")) +
    theme_bw() +
    theme(text = element_text(size=14),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank())
  
}