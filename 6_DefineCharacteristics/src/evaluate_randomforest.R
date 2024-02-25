# TODO: DOCUMENTATION
calculate_attr_importance <- function(rf_model) {
  
  # Prepare importance data
  rf_importance <- rf_model$importance %>% 
    as_tibble(rownames = 'attribute') %>% 
    dplyr::select(-MeanDecreaseGini, -MeanDecreaseAccuracy) %>% 
    pivot_longer(matches('Both|Baseflow|Episodic|Neither|negative|none|positive'), 
                 names_to = 'site_category',
                 values_to = 'importance') 
  
  # Prepare importance SD data
  rf_importance_sd <- rf_model$importanceSD %>% 
    as_tibble(rownames = 'attribute') %>% 
    dplyr::select(-MeanDecreaseAccuracy) %>% 
    pivot_longer(matches('Both|Baseflow|Episodic|Neither|negative|none|positive'), 
                 names_to = 'site_category',
                 values_to = 'importance_sd')
  
  # Prepare mean importance data
  rf_means_importance <- rf_model$importance %>% 
    as_tibble(rownames = 'attribute') %>% 
    mutate(site_category = 'Overall mean')  %>% 
    dplyr::select(attribute, site_category, importance = MeanDecreaseAccuracy) %>% 
    distinct()
  
  # Prepare mean importance SD data
  rf_means_importance_sd <- rf_model$importanceSD %>% 
    as_tibble(rownames = 'attribute') %>% 
    mutate(site_category = 'Overall mean') %>% 
    dplyr::select(attribute, site_category, importance_sd = MeanDecreaseAccuracy) %>% 
    distinct()
  
  # Prepare overall importance table
  bind_rows(rf_importance, rf_means_importance) %>% 
    left_join(bind_rows(rf_importance_sd, rf_means_importance_sd), 
              by = c('attribute', 'site_category')) %>% 
    dplyr::select(attribute, site_category, everything()) %>% 
    mutate(attribute_grp = case_when(
      attribute %in% c('annualPrecip', 'avgSnow', 'freezeDayFirst', 'freezeDayLast') ~ 'meteo',
      attribute %in% c('roadSaltPerSqKm', 'roadSaltCumulativePerSqKm', 'roadSaltRatio') ~ 'salt',
      attribute %in% c('avgDepth2WT', 'avgGWRecharge', 'baseFlowInd',
                       'soilPerm', 'subsurfaceContact', 'zellSanfordDepthToWT') ~ 'gw',
      attribute %in% c('pctAgriculture', 'pctDeveloped', 'pctForested', 
                       'pctOpenWater', 'pctWetland', 'roadDensity') ~ 'landcover',
      grepl('Runoff', attribute) ~ 'basin',
      attribute %in% c('areaCumulativeSqKm', 'areaRatio', 'areaSqKm',
                       'avgBasinSlope', 'medianFlow', 'runoffPrecipRatio') ~ 'basin',
      .default = NA_character_
    ))
  
}

# TODO: DOCUMENTATION
visualize_attr_importance <- function(rf_model_importance) {
  # TODO: save as file
  
  data_to_plot <- rf_model_importance %>% 
    # Order the attributes based on importance values *within* each site category
    # Thanks to https://stackoverflow.com/questions/72147790/ggplot-facet-different-y-axis-order-based-on-value!
    mutate(attribute = tidytext::reorder_within(attribute, importance, within = site_category)) 
  
  # Var importance per site category, ordered by attribute importance
  ggplot(data_to_plot, aes(x = importance, y = attribute,
                           color = attribute_grp)) +
    geom_point() +
    geom_segment(aes(x = importance - importance_sd,
                     xend = importance + importance_sd,
                     yend = attribute)) +
    facet_wrap(vars(site_category), scales = 'free', nrow = 1) +
    tidytext::scale_y_reordered() +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=9),
          strip.background = element_blank(),
          strip.text = element_text(face = 'bold', size = 12))
  
}

# TODO: DOCUMENTATION
# TODO: show partial dependence of two features? 
# https://christophm.github.io/interpretable-ml-book/pdp.html#examples
calculate_partial_dependence <- function(rf_model, site_attr_data) {
  rf_classes <- rf_model$classes
  
  numeric_attrs <- rf_model$importance %>% rownames()
  # TODO: figure out how to evaluate dependence/importance of trends?
  numeric_attrs <- numeric_attrs[!grepl('Trend', numeric_attrs)]
  
  combos <- expand.grid(
    attribute = numeric_attrs, 
    site_category = rf_classes, 
    stringsAsFactors = FALSE)
  
  var_partials <- combos %>% 
    pmap(~{
      pdp::partial(
        rf_model,
        pred.var = .x, 
        which.class = .y,
        train = site_attr_data
      ) %>% setNames(c('attr_val', 'attr_partdep')) %>% 
        mutate(attribute = .x, 
               site_category = .y, .before='attr_val')
    }) %>% 
    bind_rows()
}

# TODO: DOCUMENTATION
visualize_partial_dependence <- function(pdp_data, real_attribute_values) {
  
  # Use the actual values of the attributes to add a rug on the bottom so 
  # we can tell which patterns of dependence for given attributes are 
  # maybe just artifacts of a lack of input data.
  real_attribute_values_to_plot <- real_attribute_values %>% 
    pivot_longer(-site_category_fact, names_to = 'attribute', values_to = 'attr_val') %>% 
    rename(site_category = site_category_fact) %>% 
    filter(attribute %in% unique(pdp_data$attribute))
    
  ggplot(pdp_data, aes(x = attr_val, color = site_category)) +
    facet_wrap(vars(attribute), scales = 'free_x') +
    scico::scale_color_scico_d(begin = 0, end = 0.75) +
    geom_line(aes(y = attr_partdep), linewidth = 1) +
    geom_rug(data=real_attribute_values_to_plot, sides='b') +
    theme_bw() +
    theme(strip.background = element_blank())
}

# TODO: SHAP eval???
# library(kernelshap)
# library(shapviz)
# library(randomForest)
# 
# fit <- tar_read(p5_rf_model_optimal)
# dat <- tar_read(p5_site_attr_rf)
# 
# # Step 1: Calculate Kernel SHAP values
# # bg_X is usually a small (50-200 rows) subset of the data
# set.seed(19)
# s <- kernelshap(fit, dat[-1], bg_X = sample_n(dat, 50)) # Just 50 rows took like 30 min
# s <- kernelshap(fit, dat[-1], bg_X = dat)
# 
# # Step 2: Turn them into a shapviz object
# sv <- shapviz(s)
# 
# # Step 3: Gain insights...
# sv_importance(sv, kind = "bee")
# sv_dependence(sv, v = "roadSalt", color_var = "auto")
