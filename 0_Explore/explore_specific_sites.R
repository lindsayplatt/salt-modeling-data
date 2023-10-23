# Identify a few sites to zoom in on

library(targets)
library(tidyverse)

tar_load(sc_trends_with_site_info)

# Filter to sites with a positive trend in summer baseflow
# AND high connection to GW
positive_summer_baseQ <- sc_trends_with_site_info %>% 
  # Positive trend in summer baseflow
  filter(season == "summer", trend == "positive", hydro_cond == "base") %>% 
  # High connection with GW
  filter(Ts_high, DTW_low)

# Now extract the winter/spring trends for the same sites
positive_summer_baseQ_but_winterspring_trends <- sc_trends_with_site_info %>% 
  # Filter to just the sites identified above
  filter(site_no %in% positive_summer_baseQ$site_no) %>%
  # Keep only winter/spring
  filter(season %in% c('winter', 'spring'))

positive_summer_baseQ_but_winterspring_trends %>% 
  group_by(season, trend) %>% 
  tally()

ggplot(positive_summer_baseQ_but_winterspring_trends,
       aes(x = DTW, y = Ts, color = trend)) +
  geom_point() +
  facet_grid(hydro_cond ~ season)

# Compare baseflow vs storm conditions in winter
change_base_to_storm <- positive_summer_baseQ_but_winterspring_trends %>% 
  filter(hydro_cond != "all") %>% 
  dplyr::select(site_no, Ts, DTW, season, hydro_cond, trend) %>% 
  pivot_wider(id_cols = c("site_no", "Ts", "DTW", "season"), 
              names_from = hydro_cond,
              values_from = trend) %>% 
  mutate(change = case_when(
    base == "negative" & storm == "negative" ~ "Both base & storm fresher",
    base == "negative" & storm == "positive" ~ "Base fresher, storm saltier",
    base == "positive" & storm == "positive" ~ "Both base & storm saltier",
    base == "positive" & storm == "negative" ~ "Base saltier, storm fresher",
    base == "zero" & storm == "negative" ~ "Base no change, storm fresher",
    base == "zero" & storm == "positive" ~ "Base no change, storm saltier",
    base == "zero" & storm == "zero" ~ "No change for base nor storm",
    base == "negative" & storm == "zero" ~ "Base fresher, storm no change",
    base == "positive" & storm == "zero" ~ "Base saltier, storm no change",
    is.na(base) | is.na(storm) ~ NA)) %>% 
  filter(!is.na(change))

ggplot(change_base_to_storm, aes(x = change, fill = season)) +
  geom_histogram(stat="count", position = position_dodge()) + 
  coord_flip() + xlab("Trends in SC") + 
  ggtitle("Distribution of winter and spring SC trends for\nsites with positive SC trends in summer baseflow")

# Sites I am interested in right now are those where 
# 1. the summer baseflow is positive (indicating the GW sources are saltier),
# 2. the winter/spring baseflow are positive (again indicating saltier GW),
# 3. the winter storm flow is positive (indicating that road salt application is a problem)

# Filter based on 3 criteria above
sites_crit1 <- sc_trends_with_site_info %>% 
  filter(season == "summer", hydro_cond == "base", trend == "positive") %>% 
  pull(site_no)
sites_crit2 <- sc_trends_with_site_info %>% 
  filter(season %in% c("winter", "spring"), hydro_cond == "base", trend == "positive") %>% 
  pull(site_no)
sites_crit3 <- sc_trends_with_site_info %>% 
  filter(season == "winter", hydro_cond == "storm", trend == "positive") %>% 
  pull(site_no)
sites_meeting_criteria <- sc_trends_with_site_info %>% 
  dplyr::select(site_no) %>% distinct() %>% 
  filter(site_no %in% sites_crit1, 
         site_no %in% sites_crit2,
         site_no %in% sites_crit3) %>% 
  pull(site_no)

# Map those sites
sites_sf <- st_transform(tar_read(q_sc_sites_sf), usmap::usmap_crs()) %>% 
  filter(site_no %in% sites_meeting_criteria)
huc04s_sf <- st_transform(tar_read(nhd_huc04s_sf), usmap::usmap_crs())

ggplot() +
  geom_sf(data=tar_read(conus_nosalt_sf), fill='#b8b8b8', color=NA) +
  geom_sf(data=tar_read(conus_salt_sf), fill='#f4f4f4', color='#898989') +
  geom_sf(data=huc04s_sf, fill='#daeaf3', color='#38799f', size=3, alpha = 0.65) +
  geom_sf(data=sites_sf, color='#d08b2c', shape=17) +
  theme_void() + ggtitle("Sites with positive SC trends for all of the following: \n
                          \t  1. summer baseflow
                          \t  2. winter and spring baseflow
                          \t  3. winter stormflow")

# Double check GW connectivity
#  There are plenty with span the connectivity spectrum
sites_meeting_criteria_info <- sc_trends_with_site_info %>% 
  filter(site_no %in% sites_meeting_criteria) %>% 
  mutate(gw_connect = case_when(
    Ts_high & DTW_low ~ "High",
    !Ts_high & !DTW_low ~ "Low",
    .default = NA
  )) %>% 
  dplyr::select(site_no, DTW_plot, Ts_plot, gw_connect) %>% 
  distinct()

# 33 sites with high connectivity, 25 sites with low
sites_meeting_criteria_info %>% 
  group_by(gw_connect) %>% 
  tally()

ggplot(sites_meeting_criteria_info,
       aes(x = DTW_plot, y = Ts_plot)) +
  geom_hline(yintercept=50) + geom_vline(xintercept=50) +
  geom_point() 



# Filter to sites that meet the criteria, have a 
# non-ambiguous indication of GW by my current method,
# and have at least 10 years of data that goes beyond 2020

sites_meeting_data_criteria <- tar_read(q_sc_data_baseq) %>% 
  group_by(site_no) %>% 
  summarize(min_date = min(date),
            max_date = max(date)) %>% 
  mutate(ndays = max_date - min_date,
         nyears = as.numeric(ndays/365)) %>% 
  filter(year(max_date) >= 2020,
         nyears >= 5) %>% 
  pull(site_no)

sites_sf_with_connect <- sites_sf %>% 
  left_join(sites_meeting_criteria_info, by = "site_no") %>% 
  filter(!is.na(gw_connect)) %>% 
  filter(site_no %in% sites_meeting_data_criteria)

# Map these with connectivity indicated
p<-ggplot() +
  geom_sf(data=tar_read(conus_nosalt_sf), fill='#b8b8b8', color=NA) +
  geom_sf(data=tar_read(conus_salt_sf), fill='#f4f4f4', color='#898989') +
  # geom_sf(data=huc04s_sf, fill='#daeaf3', color='#38799f', size=3, alpha = 0.65) +
  geom_sf(data=sites_sf_with_connect, aes(color=gw_connect, text=site_no), shape=17) +
  scale_color_manual(name = "GW Connectivity", 
                     values = c("High" = "#11464a", "Low" = "#d08b2c")) +
  theme_void() + theme(legend.position="bottom") +
  ggtitle("Sites with positive SC trends for all of the following: \n
    \t  1. summer baseflow
    \t  2. winter and spring baseflow
    \t  3. winter stormflow\n", 
          subtitle = sprintf('%s sites on the map', nrow(sites_sf_with_connect))) 
plotly::ggplotly(p, tooltip = "text")

# Visualize sites with HUCs interactively
huc04s_simple_sf<-huc04s_sf %>% 
  st_simplify(dTolerance = 1000) %>% 
  sf::st_cast("MULTIPOLYGON")
p2<-ggplot() +
  geom_sf(data=huc04s_simple_sf, fill='#daeaf3', aes(text=huc4), color='#38799f', size=3, alpha = 0.65) +
  geom_sf(data=sites_sf, aes(text=site_no), shape=17)
plotly::ggplotly(p2, tooltip="text")

##### Explore specific sites ####

# Used the interactive version of the map from above to manually 
# select certain site numbers based on location, estimated GW 
# connectivity, and assumed road salt application levels.

# These WI sites are interesting because there is definitely road
# salt applied in these areas, but two are out west of Madison and 
# have "low connectivity". The other three are near Milwaukee and
# have "high connectivity".
wi_sites_lowconnect <- c('05406500', '05406479')
wi_sites_highconnect <- c('04087050', '04087030', '04087142')

# These sites in NY are interesting because they are listed here to have
# "low GW connectivity" but are seeing increasing salty summer baseflow. 
# They are near the sites from Kincaid and Findlay 2009 but on the west
# site of the Hudson river.
ny_sites_lowconnect <- c('01434021', '0143400680')
## See them on a map together:
# kincaid_site_approx <- tibble(site_no = "Kincaid and Findlay 2009",
#                               lat = 41.793776,
#                               long = -73.724101)
# ny_sites_sf <- dataRetrieval::readNWISsite(ny_sites_lowconnect) %>% 
#   dplyr::select(site_no, lat = dec_lat_va, long = dec_long_va) %>% 
#   bind_rows(kincaid_site_approx) %>% 
#   st_as_sf(coords = c('long', 'lat'), crs=4326) 
# ny_sf <- usmap::us_map(include = c('New York')) %>% 
#   st_as_sf(coords = c('x', 'y'), crs = usmap::usmap_crs()) %>% 
#   group_by(group, abbr) %>% 
#   summarise(geometry = st_combine(geometry), .groups="keep") %>%
#   st_cast("POLYGON") %>% 
#   st_transform(crs=4326)
# ggplot() +
#   geom_sf(data=ny_sf) + 
#   geom_sf(data=ny_sites_sf, aes(color = site_no))

# These sites are being chosen because they might fall within a HUC
# that has fairly high salt applied to roads & high GW connectivity
in_sites_highconnect <- c('05518000', '05517500', '05524500')

ri_sites_highconnect <- c('01115170')
mn_sites_highconnect <- c('05288705')

# Pull timeseries for these sites:
sites_of_interest <- c(
  # wi_sites_lowconnect,
  # wi_sites_highconnect,
  # ny_sites_lowconnect,
  in_sites_highconnect,
  ri_sites_highconnect,
  mn_sites_highconnect
)



q_sc_sites_of_interest <- q_sc_data_baseq %>% 
  filter(site_no %in% sites_of_interest) %>% 
  mutate(state_site = sprintf('%s (%s)', site_no, state_abbr)) %>% 
  mutate(is_baseflow = Q_Storm == 0)

ggplot(q_sc_sites_of_interest, aes(x=date, y=SpecCond, color = is_summer)) +
  geom_point(alpha = 0.5) + 
  facet_grid(state_site ~ ., scales="free_y") +
  ggtitle("SC timeseries")

ggplot(q_sc_sites_of_interest, aes(x=date, y=SC_flow_normalized, color = is_summer)) +
  geom_point(alpha = 0.5) + 
  facet_grid(state_site ~ ., scales="free_y") +
  ggtitle("Flow normalized SC timeseries")

q_sc_sites_of_interest_summer <- q_sc_sites_of_interest %>% 
  filter(is_summer)

ggplot(q_sc_sites_of_interest_summer, aes(x=date, y=SpecCond, color = is_baseflow)) +
  geom_point(alpha = 0.5) + 
  facet_grid(state_site ~ ., scales="free_y") +
  ggtitle("SC timeseries for summertime")

ggplot(q_sc_sites_of_interest_summer, aes(x=date, y=SC_flow_normalized, color = is_baseflow)) +
  geom_point(alpha = 0.5) + 
  facet_grid(state_site ~ ., scales="free_y") +
  ggtitle("Flow normalized SC timeseries for summertime")
