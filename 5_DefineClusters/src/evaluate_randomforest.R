# TODO: DOCUMENTATION
calculate_attr_importance <- function(rf_model) {
  
  # Prepare importance data
  rf_importance <- rf_model$importance %>% 
    as_tibble(rownames = 'attribute') %>% 
    dplyr::select(-MeanDecreaseGini, -MeanDecreaseAccuracy) %>% 
    pivot_longer(matches('[0-9]'), 
                 names_to = 'cluster',
                 values_to = 'importance') 
  
  # Prepare importance SD data
  rf_importance_sd <- rf_model$importanceSD %>% 
    as_tibble(rownames = 'attribute') %>% 
    dplyr::select(-MeanDecreaseAccuracy) %>% 
    pivot_longer(matches('[0-9]'), 
                 names_to = 'cluster',
                 values_to = 'importance_sd')
  
  # Prepare mean importance data
  rf_means_importance <- rf_model$importance %>% 
    as_tibble(rownames = 'attribute') %>% 
    mutate(cluster = 'means')  %>% 
    dplyr::select(attribute, cluster, importance = MeanDecreaseAccuracy) %>% 
    distinct()
  
  # Prepare mean importance SD data
  rf_means_importance_sd <- rf_model$importanceSD %>% 
    as_tibble(rownames = 'attribute') %>% 
    mutate(cluster = 'means') %>% 
    dplyr::select(attribute, cluster, importance_sd = MeanDecreaseAccuracy) %>% 
    distinct()
  
  # Prepare overall importance table
  bind_rows(rf_importance, rf_means_importance) %>% 
    left_join(bind_rows(rf_importance_sd, rf_means_importance_sd), 
              by = c('attribute', 'cluster')) %>% 
    dplyr::select(attribute, cluster, everything()) %>% 
    mutate(attribute_grp = case_when(
      attribute %in% c('avgSnow', 'avgPrecip', 'avgSnowDetrended', 'pctSnow') ~ 'meteo',
      attribute %in% c('TrendSeasonalMK', 'TrendMannKendall', 'TrendMannKendall_DS',
                       'roadSalt', 'meanSoilSalinity') ~ 'salt',
      attribute %in% c('pctSoilOM', 'avgGWRecharge', 'daysInSubsurface', 'avgSoilStorage',
                       'pctSoilClay', 'pctSoilSand', 'pctSoilSilt', 'soilPerm', 
                       'availWaterCap', 'baseFlowInd', 'avgDepth2WT') ~ 'gw',
      attribute %in% c('vegIndAutumn', 'pctLowDev', 'pctHighDev', 'vegIndSummer',
                       'vegIndWinter', 'topoWetInd', 'vegIndSpring', 'numDams2013',
                       'roadStreamXings', 'roadDensity') ~ 'landcover',
      attribute %in% c('meanFlow', 'avgRunoff', 'avgBasinSlope', 'avgStreamSlope') ~ 'basin',
      .default = NA_character_
    ))
  
}

# TODO: DOCUMENTATION
visualize_attr_importance <- function(rf_model_importance, simple = FALSE) {
  # TODO: save as file
  
  # Var importance per cluster, ordered by attribute importance
  plot_list <- rf_model_importance %>% 
    split(.$cluster) %>% 
    map(~{
      # Arrange each cluster's data based on its own order
      data_to_plot <- .x %>% 
        arrange(importance) %>% 
        mutate(attributef = factor(attribute, ordered = T,
                                   levels = unique(.$attribute))) %>% 
        rowwise() %>% 
        mutate(attribute_grp = ifelse(simple, 'black', attribute_grp))
      p <- ggplot(data_to_plot, aes(x = importance, y=attributef,
                                    color = attribute_grp)) +
        geom_point() +
        geom_segment(aes(x = importance - importance_sd,
                         xend = importance + importance_sd,
                         yend = attributef)) +
        guides(color = 'none') +
        theme_bw() +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_text(size=7),
              title = element_text(size=9, face='bold')) +
        ggtitle(sprintf('Cluster %s', unique(.x$cluster)))
      if(!simple) p <- p + facet_grid(attribute_grp ~ ., scales='free_y') 
      if(simple) p <- p + scale_color_identity()
      return(p)
    })
  return(plot_list)
  # Put all together
  # cowplot::plot_grid(plotlist = plot_list, nrow = 1)
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
    cluster = rf_classes, 
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
               cluster = .y, .before='attr_val')
    }) %>% 
    bind_rows()
}

# TODO: DOCUMENTATION
visualize_partial_dependence <- function(pdp_data) {
  ggplot(pdp_data, aes(attr_val, attr_partdep, color = cluster)) +
    facet_wrap(vars(attribute), scales = 'free_x') +
    scico::scale_color_scico_d() +
    geom_line(linewidth = 1) +
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
