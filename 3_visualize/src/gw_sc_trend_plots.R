

calc_and_plot_trend <- function(trend_data, trend_method, subtitle_caveat) {
  
  # Plot patterns
  trend_plot_data <- trend_data %>% 
    select(site_no, Ts, DTW, Ts_plot, DTW_plot, hydro_cond, season, trend) %>% 
    mutate(hydro_cond = sprintf('flow: %s', hydro_cond),
           season = sprintf('season: %s', season)) %>% 
    mutate(alpha_val = ifelse(trend == "positive", 1, 0.5))
  
  p1 <- ggplot(trend_plot_data, aes(x = DTW_plot, y = Ts_plot, color = trend, alpha = alpha_val)) +
    geom_point(size=2,shape=16) +
    scale_alpha_identity() +
    scale_color_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    geom_hline(yintercept = 50) + geom_vline(xintercept = 50) +
    facet_grid(season ~ hydro_cond) +
    xlim(c(0,100)) + ylim(c(0,100)) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab('Depth to water table, %ile') +
    ylab('Transmissivity, %ile') +
    ggtitle('SC trends related to GW connectivity, percentiles',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  p2 <- ggplot(trend_plot_data, aes(x = -log10(DTW), y = log10(Ts), color = trend, alpha = alpha_val)) +
    geom_point(size=2,shape=16) +
    scale_alpha_identity() +
    scale_color_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    facet_grid(season ~ hydro_cond) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab('Depth to water table') +
    ylab('Transmissivity') +
    ggtitle('SC trends related to GW connectivity, real values',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  p3 <- trend_plot_data %>% 
    filter(!is.na(trend)) %>% 
    ggplot(aes(x = log(Ts), color = trend)) +
    stat_ecdf(geom="step") +
    scale_color_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    facet_grid(season ~ hydro_cond) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab('Transmissivity') +
    ylab('Cumulative Distribution') +
    ggtitle('SC trends related to GW connectivity, distribution',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  p4 <- trend_plot_data %>% 
    filter(!is.na(trend)) %>% 
    ggplot(aes(x = -log(DTW), color = trend)) +
    stat_ecdf(geom="step") +
    scale_color_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    facet_grid(season ~ hydro_cond) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    xlab('Depth to water table') +
    ylab('Cumulative Distribution') +
    ggtitle('SC trends related to GW connectivity, distribution',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  p5 <- trend_plot_data %>% 
    mutate(Ts_cond = ifelse(Ts_plot >= 50, 'Ts-high', 'Ts-low'), 
           DTW_cond = ifelse(DTW_plot >= 50, 'DTW-shallow', 'DTW-deep')) %>% 
    mutate(GW_Connectivity = factor(sprintf('%s, %s', Ts_cond, DTW_cond), ordered = TRUE,
                                    levels = c('Ts-low, DTW-deep', 'Ts-low, DTW-shallow', 'Ts-high, DTW-deep', 'Ts-high, DTW-shallow'))) %>% 
    filter(!is.na(trend), !is.na(Ts_cond)) %>% 
    group_by(GW_Connectivity, hydro_cond, season, trend) %>%
    tally() %>%
    ggplot(aes(x = GW_Connectivity, y = n, fill = trend)) +
    # geom_bar(stat="identity", position="dodge", width=0.5) +
    geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_fill_manual(values = c(negative = "#F8766D", positive = "#00BA38", zero = "#619CFF")) +
    facet_grid(season ~ hydro_cond) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45,hjust=1)) +
    xlab('Groundwater connectivity (low --> high)') +
    ylab('Number of sites') +
    ggtitle('SC trends related to GW connectivity',
            sub = sprintf("Trends based on %s method, %s", trend_method, subtitle_caveat)) 
  
  return(list(p1,p2,p3,p4,p5))
}
