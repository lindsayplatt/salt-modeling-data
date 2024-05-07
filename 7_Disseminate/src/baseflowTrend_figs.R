
#' @title Make a bunch of mini plots showing baseflow at every site
#' @description Plot baseflow SpC values along with median annual values and
#' indicate the trend that was calculated (through color) for every site. Creates
#' groups of facet ggplots.
#' 
#' @param ts_sc_bf a tibble of time series data for only days that qualified 
#' as baseflow days so that a trend can be calculated; has the columns 
#' `site_no`, `dateTime`, `SpecCond`.
#' @param trends_sc a tibble with the baseflow trend calculated for each site
#' with the columns `site_no` and `baseflowTrend` ("none", "positive", or "negative")
#' @param nrow numeric value indicating the number of rows of facets to plot; defaults to 5
#' @param dir character value (either `h` or `v`) to pass to `facet_wrap()` to 
#' indicate whether it should fill by row or by column; defaults to `h`.
#' 
#' @returns a list of ggplots
#' 
create_baseflow_trend_plotlist <- function(ts_sc_bf, trends_sc, nrow = 5, dir = "h") {
  # Order trends to prep order of facets
  trends_sc <- trends_sc %>% arrange(desc(baseflowTrend), site_no)
  
  ts_sc_bf_trends <- ts_sc_bf %>% 
    left_join(trends_sc, by = 'site_no') %>% 
    # Setup site number as an ordered factor to control order of facets
    mutate(site_no_ord = factor(site_no, levels = trends_sc$site_no)) %>% 
    group_by(site_no_ord) %>% 
    # Prepare each site to be shown in a specific group's set of facets
    mutate(grp_num = ceiling(cur_group_id()/25)) %>% 
    ungroup()
  
  # Calculate annual median SpC values and retain information needed for plotting
  annual_sc_bf <- ts_sc_bf_trends %>% 
    mutate(year = year(dateTime)) %>% 
    group_by(grp_num, site_no_ord, year, baseflowTrend) %>% 
    summarize(SpecCond = median(SpecCond), .groups="keep") %>%
    ungroup() %>% 
    mutate(dateTime = as.Date(sprintf('%s-06-30', year))) %>% # Add mid-year as the date to plot these
    # Split data by group for plotting
    split(.$grp_num)
  
  # Now split SC baseflow ts data by group number for plotting
  ts_sc_bf_trends <- ts_sc_bf_trends %>% split(.$grp_num)
  
  plot_baseflow_facets <- function(ts_sc_bf, annual_sc_bf) {
    ggplot(ts_sc_bf, aes(x = dateTime, y = SpecCond, color = baseflowTrend)) +
      geom_point(alpha = 0.25, shape=21, size=0.5) +
      geom_point(data = annual_sc_bf, size=1.5) +
      facet_wrap(vars(site_no_ord), scales='free', nrow=nrow, dir=dir) +
      scale_color_manual(values = c(positive = '#b45f06', none = 'grey30', negative = '#663329'),
                         name = '') +
      ylab('Specific conductance, µS/cm at 25°C') + 
      theme_bw() +
      theme(text = element_text(size=10), 
            axis.text.x = element_text(size=8, angle=20, hjust=1),
            axis.title.y = element_text(vjust = 1.5),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            strip.background = element_blank(),
            strip.text = element_text(size = 10, face = 'bold'),
            strip.clip = "off",
            plot.margin = unit(c(0.5,0.5,0,0.5), 'lines'))
  }
  
  # Now plot all site's data and save as a list object
  plotlist_out <- map2(ts_sc_bf_trends, annual_sc_bf, plot_baseflow_facets)
  return(plotlist_out)
}

#' @title Create a plot showing how often a stream had a baseflow day vs stream size
#' @description Show how smaller streams experience baseflow days more frequently
#' than larger streams.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param ts_sc a tibble of daily specific conductance for sites that met all initial
#' data quantity and quality filters. It needs at least the column `site_no` and
#' a row per daily record.
#' @param ts_sc_bf a tibble of time series data for only days that qualified 
#' as baseflow days so that a trend can be calculated; has the columns 
#' `site_no`, `dateTime`, `SpecCond`.
#' @param site_attr_data a tibble with one row per site and any number of columns
#' giving different attributes; needs the columns `site_no` and `attr_[attribute]`
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_bf_vs_q_scatterplot <- function(out_file, ts_sc, ts_sc_bf, site_attr_data) {
  
  spc_ts_total_days <- ts_sc %>% 
    group_by(site_no) %>% 
    summarize(n_days = n(), .groups='keep')
  median_q <- site_attr_data %>% 
    select(site_no, attr_medianFlow)
  spc_bf_info <- ts_sc_bf %>% 
    group_by(site_no) %>% 
    summarize(n_baseflow_days = n()) %>% 
    left_join(median_q, by = 'site_no') %>% 
    left_join(spc_ts_total_days, by = 'site_no') %>% 
    mutate(pctBFdays = round(n_baseflow_days / n_days * 100, digits=2))
  
  p_scatterplot <- ggplot(spc_bf_info,
                          aes(x=log10(attr_medianFlow), y=pctBFdays)) +
    geom_point(size=0.9) +
    xlab(expression('Median Flow, m'^3 * '/s')) + 
    ylab('Baseflow days as percent of total days in record') +
    theme_bw() + 
    theme(axis.title.x = element_text(size = 9, vjust = -1),
          axis.title.y = element_text(size = 9, vjust = 3),
          panel.grid = element_blank())
  
  ggsave(out_file, p_scatterplot, width = 3.25, height = 3.25, units = 'in', dpi = 500)
  return(out_file)
}

#' @title Create a plot showing negative relationship between forest and developed area
#' @description Generate a scatter plot of site attributes with percent forested 
#' on the x-axis and percent developed on the y-axis. This is then split out per
#' baseflow trend class: positive, none, and negative.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param site_attr_data_bf a tibble with one row per site that was used in the
#' baseflow salinization model, and at least the columns `site_no`, `site_category_fact`,
#' `pctForested`, and `pctDeveloped`.
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_dev_vs_forest_scatters <- function(out_file, site_attr_data_bf) {
  
  p <- ggplot(site_attr_data_bf, aes(x=pctForested, y=pctDeveloped)) +
    facet_wrap(vars(site_category_fact)) +
    geom_point(aes(color = site_category_fact)) +
    scale_color_manual(values = c(positive='#b45f06', none='grey40', negative='#663329')) +
    xlim(c(0,100)) + ylim(c(0,100)) +
    ylab('Developed (% area)') +
    xlab('Forested (% area)') +
    theme_bw() +
    theme(strip.text = element_text(size = 10, face = 'bold'),
          strip.background = element_rect(fill = 'white', color = 'transparent'),
          legend.position = "none",
          axis.title.x = element_text(size=10, vjust=-1),
          axis.text = element_text(size=8))
  
  ggsave(out_file, p, width = 6.5, height = 3.25, units = 'in', dpi = 500)
  return(out_file)
}
