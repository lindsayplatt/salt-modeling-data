
#' @title Create a 4-panel view of random forest results
#' @description This function creates a 4-panel figure that includes attribute
#' distributions for different categories as boxplots and maps, the importance
#' of different attributes as determined by the random forest model, and the
#' partial dependence plots. These figures can be used to help evaluate the output.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param partial_dep_ggplot ggplot object output from `visualize_partial_dependence()`
#' @param attributes_boxes_ggplot ggplot object output from `visualize_numeric_attrs()`
#' @param rf_importance_ggplot ggplot object output from `visualize_attr_importance()`
#' @param rf_category_map ggplot object output from `visualize_catgory_sites_map()`
#' 
#' @results a character string giving the location of the saved figure file
#' 
create_summary_view <- function(out_file, partial_dep_ggplot, attributes_boxes_ggplot, 
                                rf_importance_ggplot, rf_category_map) {
  
  rf_plots <- list(partial_dep_ggplot, attributes_boxes_ggplot, 
                   rf_importance_ggplot, rf_category_map)
  png(out_file, width = 22, height = 17, res = 100, units = 'in')
  print(cowplot::plot_grid(plotlist = rf_plots, nrow=2, 
                     rel_heights = c(0.60, 0.40), 
                     rel_widths = c(0.60, 0.40)))
  dev.off()
  
  return(out_file)
}
