
# TODO: DOCUMENTATION
add_conus_basemap <- function() {
  conus_sf <- usmap::us_map(exclude = c("AK", "HI")) %>% 
    st_as_sf(coords = c('x', 'y'), 
             crs = usmap::usmap_crs()) %>% 
    group_by(group, abbr) %>% 
    summarise(geometry = st_combine(geometry), .groups="keep") %>%
    st_cast("POLYGON")
  
  geom_sf(data=conus_sf, fill = 'white', color = 'darkgrey')
}

# TODO: DOCUMENTATION
visualize_cluster_sites_map <- function(site_clusters, sites_sf) {
  
  cluster_sites_sf <- sites_sf %>% 
    st_transform(crs=usmap::usmap_crs()) %>% 
    left_join(site_clusters, by = 'site_no') %>% 
    filter(!is.na(cluster)) %>% 
    mutate(cluster = as.character(cluster)) %>% 
    group_by(cluster) %>% 
    mutate(n_cluster = n()) %>% 
    ungroup() %>% 
    mutate(cluster_title = sprintf('Cluster %s (n = %s)', cluster, n_cluster))

  ggplot() +
    add_conus_basemap() +
    geom_sf(data=cluster_sites_sf, 
            aes(color = cluster), 
            alpha=0.75) +
    guides(color = 'none') +
    facet_wrap(vars(cluster_title), ncol=2) +
    theme_void()
  
}

# sites_nonsalt <- sites_sf %>% 
#   filter(site_no %in% unique(site_clusters$site_no)) %>% 
#   filter(site_no %in% tar_read(p3_ts_sc_nonsalt_sites))
# site_clusters %>% 
#   filter(site_no %in% sites_nonsalt$site_no)
# ggplot() +
#   add_conus_basemap() +
#   geom_sf(data=sites_sf) +
#   geom_sf(data=sites_nonsalt, shape=17, color = 'red', size=2) +
#   theme_void()
# tar_read(p4_dtw_clusters_bySiteYear) %>% 
#   filter(site_no %in% sites_nonsalt$site_no) %>%
#   pull(site_no) %>% unique() %>% length()
# tar_read(p4_dtw_clusters_ts) %>% 
#   filter(site_no %in% sites_nonsalt$site_no) %>% 
#   mutate(siteyr = sprintf('%s-%s', site_no, year)) %>% 
#   ggplot(aes(x = doy, y = SpecCond_norm)) +
#   geom_line(aes(group = siteyr)) +
#   facet_wrap(vars(cluster))
