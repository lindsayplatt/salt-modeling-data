# How many sites see their annual maximum SC values in the summer?

library(targets)
library(tidyverse)
library(sf)

tar_load(q_sc_data_baseq)
tar_load(q_sc_sites_sf)
tar_load(sc_trends_with_site_info)

# Filter to sites that meet the criteria, have a 
# non-ambiguous indication of GW by my current method,
# and have at least 10 years of data that goes beyond 2020

sites_meeting_data_criteria <- q_sc_data_baseq %>% 
  group_by(site_no) %>% 
  summarize(min_date = min(date),
            max_date = max(date)) %>% 
  mutate(ndays = max_date - min_date,
         nyears = as.numeric(ndays/365)) %>% 
  filter(year(max_date) >= 2020,
         nyears >= 5) %>% 
  pull(site_no)

# Also identify sites with positive trends
sites_with_pos_winterspring <- sc_trends_with_site_info %>% 
  filter(season %in% c("winter", "spring"), trend == "positive") %>% 
  pull(site_no) %>% unique()
sites_with_pos_summer <- sc_trends_with_site_info %>% 
  filter(season == "summer", trend == "positive") %>% 
  pull(site_no) %>% unique()

##### Explore sites where summertime max SC exceeds other times of the year #####

site_year_max_sc <- q_sc_data_baseq %>% 
  filter(site_no %in% sites_meeting_data_criteria) %>% 
  mutate(year = year(date)) %>% 
  group_by(site_no, year, is_summer) %>% 
  summarize(max_sc = max(SpecCond, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(is_summer = ifelse(is_summer, "summer", "not_summer")) %>% 
  pivot_wider(id_cols = c("site_no", "year"),
              names_from = is_summer,
              values_from = max_sc) %>% 
  mutate(summer_max = summer > not_summer)

# How many site-years where summer is the max?
site_year_max_sc %>% 
  group_by(summer_max) %>% 
  tally()

# How many sites experience a max SC in the summer?
sites_summer_max_sc <- site_year_max_sc %>% 
  filter(summer_max) 

# How many years does that happen?
sites_summer_max_sc_counts <- sites_summer_max_sc %>% 
  group_by(site_no) %>% 
  tally(name = "num_years")

# Where are these sites?
sites_sf <- st_transform(q_sc_sites_sf, usmap::usmap_crs()) %>% 
  filter(site_no %in% sites_summer_max_sc_counts$site_no) %>% 
  left_join(sites_summer_max_sc_counts, by = "site_no")

ggplot() +
  geom_sf(data=tar_read(conus_nosalt_sf), fill='#b8b8b8', color=NA) +
  geom_sf(data=tar_read(conus_salt_sf), fill='#f4f4f4', color='#898989') +
  geom_sf(data=sites_sf, aes(color=num_years), shape=17) +
  theme_void()

##### Do the same comparison but for flow-normalized SC #####

site_year_max_scqnorm <- q_sc_data_baseq %>% 
  filter(site_no %in% sites_meeting_data_criteria) %>% 
  mutate(year = year(date)) %>% 
  group_by(site_no, year, is_summer) %>% 
  summarize(max_scqnorm = max(SC_flow_normalized, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(is_summer = ifelse(is_summer, "summer", "not_summer")) %>% 
  pivot_wider(id_cols = c("site_no", "year"),
              names_from = is_summer,
              values_from = max_scqnorm) %>% 
  mutate(summer_max = summer > not_summer)

# How many site-years where summer is the max?
site_year_max_scqnorm %>% 
  group_by(summer_max) %>% 
  tally()

# How many sites experience a max SC in the summer?
sites_summer_max_scqnorm <- site_year_max_scqnorm %>% 
  filter(summer_max) 

# How many years does that happen?
sites_summer_max_scqnorm_counts <- sites_summer_max_scqnorm %>% 
  group_by(site_no) %>% 
  tally(name = "num_years")

# Where are these sites?
sites_scqnorm_sf <- st_transform(q_sc_sites_sf, usmap::usmap_crs()) %>% 
  filter(site_no %in% sites_summer_max_scqnorm_counts$site_no) %>% 
  left_join(sites_summer_max_scqnorm_counts, by = "site_no")

ggplot() +
  geom_sf(data=tar_read(conus_nosalt_sf), fill='#b8b8b8', color=NA) +
  geom_sf(data=tar_read(conus_salt_sf), fill='#f4f4f4', color='#898989') +
  geom_sf(data=sites_scqnorm_sf, aes(color=num_years), shape=17) +
  theme_void()

##### Again, do the above but first filter to sites that are experiencing a positive winter/spring trend #####

# Where are these sites?
sites_pos_trend_sf <- sites_sf %>% 
  filter(site_no %in% sites_with_pos_winterspring) %>% 
  filter(site_no %in% sites_with_pos_summer)

ggplot() +
  geom_sf(data=tar_read(conus_nosalt_sf), fill='#b8b8b8', color=NA) +
  geom_sf(data=tar_read(conus_salt_sf), fill='#f4f4f4', color='#898989') +
  geom_sf(data=sites_pos_trend_sf, aes(color=num_years), shape=17) +
  theme_void()

# Filter to sites where the summer max exceeded other times of year at least 
# 5 years in the record.
sites_pos_trend_sf_summermax5 <- sites_pos_trend_sf %>% 
  filter(num_years > 5)

ggplot() +
  geom_sf(data=tar_read(conus_nosalt_sf), fill='#b8b8b8', color=NA) +
  geom_sf(data=tar_read(conus_salt_sf), fill='#f4f4f4', color='#898989') +
  geom_sf(data=sites_pos_trend_sf_summermax5, aes(color=num_years), shape=17) +
  theme_void()

# Now plot their timeseries
q_sc_data_baseq %>% 
  filter(site_no %in% sites_pos_trend_sf_summermax5$site_no) %>% 
  mutate(is_baseflow = Q_Storm == 0) %>% 
  mutate(timeofyear = ifelse(is_summer, "Summer", "Not summer")) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(x=date, y=SpecCond, color = timeofyear, group=year)) +
  geom_line() +
  # geom_point(alpha = 0.5) + 
  scale_color_manual(values=c("Summer" = "#cc7722", "Not summer" = "#00a88f")) + 
  facet_grid(site_no ~ ., scales="free_y") +
  theme_bw() +
  ggtitle("SC timeseries")

q_sc_data_baseq %>% 
  filter(site_no %in% sites_pos_trend_sf_summermax5$site_no) %>% 
  mutate(hydro_cond = ifelse(Q_Storm == 0, "baseflow", "stormflow")) %>% 
  mutate(timeofyear = ifelse(is_summer, "Summer", "Not summer")) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(x=date, y=SpecCond, color = hydro_cond)) +
  # geom_line() +
  geom_point(alpha = 0.5) +
  scale_color_manual(values=c("baseflow" = "#cc7722", "stormflow" = "#00a88f")) + 
  facet_grid(site_no ~ timeofyear, scales="free_y") +
  theme_bw() +
  ggtitle("SC timeseries split by season")

q_sc_data_baseq %>% 
  filter(site_no %in% sites_pos_trend_sf_summermax5$site_no) %>% 
  mutate(is_baseflow = Q_Storm == 0) %>% 
  mutate(timeofyear = ifelse(is_summer, "Summer", "Not summer")) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(x=date, y=SC_flow_normalized, color = timeofyear, group=year)) +
  geom_line() +
  # geom_point(alpha = 0.5) +
  scale_color_manual(values=c("Summer" = "#cc7722", "Not summer" = "#00a88f")) + 
  facet_grid(site_no ~ ., scales="free_y") +
  theme_bw() +
  ggtitle("SC flow normalized timeseries")

# View trend data for these sites
sc_trends_with_site_info %>% 
  filter(site_no %in% sites_pos_trend_sf_summermax5$site_no) %>% 
  ggplot(aes(x = DTW_plot, y = Ts_plot, color = trend)) +
  geom_hline(yintercept = 50) + geom_vline(xintercept = 50) +
  geom_point() +
  facet_grid(season ~ hydro_cond)

##### Questions to think about #####

# Are summer storms providing relief?
# Bottleneck issues: Q is trending down but SC is trending up. Where are those places?
# Do wetter than normal springs flush salts from the wintertime? See dips right before summertime in SC
# For baseflow vs storm flow plots, wouldn't you expect to see more of the plots like the 
#   seventh one down, where SC for baseflow is much lower than stormflow IF the GW contributions
#   were fresh during non summer months? 
# Salt retention patterns. How quickly are salts getting flushed? What leads to them sticking around longer?
#   - Dry year vs wet year? 
#   - GW/baseflow as one element to the story? 
#   - Number of big storms in Spring coupled with number of winter storms?


# This isn't feeling super interesting anymore ... maybe focusing on that bottleneck question is 
# interesting given the need to assess climate risk? Could use to identify locations where
# mitigation of salinity is extra important?

##### Explore GW signature based on Hare et al 2021 #####

# Use sites from Hare et al to see if there are patterns with salt for deep vs shallow GW?
hare_et_al <- readxl::read_excel('1_fetch/in/hare_et_al/sourcedatafigs1,2and3.xlsx') 
site_gw_signature <- hare_et_al %>% 
  rename(site_no = SW_ID) %>% 
  filter(site_no %in% unique(tar_read(q_sc_data_baseq)$site_no)) %>%
  mutate(HDI = as.numeric(HydroDisturbIdx),
         PctImpervious = as.numeric(PctImp2011Cat),
         PctSlope = as.numeric(PctWSlope),
         BFI = as.numeric(BFI)) %>% 
  select(site_no, GW_cat, HDI, PctImpervious, PctSlope, BFI)

# TODO: I feel like there is something here but I am just not sure what.
# Histogram is not the best way to visualize this. There could be something
# with using the disturbance index + impervious surface + GW category from
# Hare et al and then combining with SC trends. Just not via histogram.
sc_trends_hare_et_al <- tar_read(sc_trends_with_site_info) %>% 
  right_join(site_gw_signature, by = "site_no")
summer_trends <- sc_trends_hare_et_al %>% filter(season == "summer")

ggplot(summer_trends, aes(x=DTW_plot, y=Ts_plot, color = GW_cat)) +
  geom_hline(yintercept = 50) + geom_vline(xintercept = 50) +
  geom_point() 

ggplot(summer_trends, aes(x=HDI, y=PctImpervious, color = GW_cat)) +
  geom_point() 

# Let's see ... compare trends of saltiness based on atmospheric vs
# deep GW vs shallow GW. Hypothesis (after filtering for sites experiencing
# positive SC trends in winter/spring): 
#   - deep GW sites will have a negative or zero summer baseflow trend because
#       deep GW is primarily contributing to the baseflow and would be less 
#       likely to have been contaminated with salts. Thus, freshwater is the 
#       main addition to the stream during baseflow in summer.
#   - shallow GW sites will have positive or zero summer baseflow trend because
#       shallow GW is more likely to have been contaminated by winter road salts
#       and thus the majority of baseflow would be supplied by somewhat salty
#       GW inputs, rather than freshwater.
#   - ignore atmospheric sites for now

deep_vs_shallow_alltrends <- sc_trends_with_site_info %>% 
  # At GW signature info
  left_join(site_gw_signature, by = "site_no") %>% 
  # Remove atmospheric signature sites
  filter(GW_cat != "Atmospheric Signature") %>% 
  # Remove sites that don't have winter/spring positive trends
  filter(site_no %in% sites_with_pos_winterspring) %>%
  # Remove sites that don't have at least 5 years of data up to 2020
  filter(site_no %in% sites_meeting_data_criteria)
deep_vs_shallow_summer <- deep_vs_shallow_alltrends %>% 
  # Isolate just summer baseflow trends
  filter(season == "summer") %>% 
  # Isolate just baseflow conditions 
  filter(hydro_cond == "base")

# End up with 5 deep and 4 shallow based on filtering above
deep_vs_shallow_summer %>% 
  group_by(GW_cat) %>% 
  tally()

# Now use the remaining sites to pull the full timeseries and visualize
q_sc_deep_vs_shallow_all <- q_sc_data_baseq %>% 
  filter(site_no %in% deep_vs_shallow_summer$site_no) %>% 
  mutate(season = ifelse(month(date) %in% c(12,1,2), 'Winter',
                         ifelse(month(date) %in% c(3,4,5), "Spring",
                                ifelse(month(date) %in% c(6,7,8), "Summer", "Fall"))),
         line_grp = sprintf('%s-%s', year(date), month(date))) %>% 
  left_join(site_gw_signature, by = "site_no") %>% 
  group_by(GW_cat, site_no) %>% 
  mutate(facet_nm = sprintf('%s-%s', gsub(" Signature", "", GW_cat), cur_group_id())) %>% 
  ungroup() 

ggplot(q_sc_deep_vs_shallow_all, aes(x=date, y=SpecCond, color=season, group=line_grp)) +
  geom_line() + 
  facet_grid(facet_nm ~ ., scales="free_y") +
  scale_color_manual(values = c('Winter'='#58afc6', 'Spring'='#78be20', 
                                'Summer'='#e7cf31', 'Fall'='#9e3e26')) +
  ggtitle("Timeseries of specific conductance", 
          subtitle="Sites with positive SC trends in winter/spring & at least 5 yrs of recent data") +
  theme_bw() +
  theme(
      strip.background = element_rect(fill=NA),
      strip.text.y = element_text(face="bold", angle=0)
  )
  # Get the facet label rectangles to be a diff color
  # DOESN'T QUITE WORK DUE TO WEIRD INTERACTION BETWEEN NEEDING `coord_cartesian(clip="off")`
  # AND TRYING TO LET THE Y-AXIS SCALE FREELY. Ended up changing facet names instead
  # geom_rect(aes(
  #   # totally defined by trial-and-error
  #   xmin=(max(q_sc_deep_vs_shallow_all$date)+250), 
  #   xmax=(max(q_sc_deep_vs_shallow_all$date)+350),
  #   ymin=min(SpecCond)-diff(range(SpecCond))*0.05, 
  #   ymax=max(SpecCond)+diff(range(SpecCond))*0.05,     
  #   fill=GW_cat), alpha=0.4, color=NA) +
  # scale_fill_manual(values = c("Deep GW Signature" = "green", "Shallow GW Signature" = "red")) +
  # coord_cartesian(clip="off", xlim=range(q_sc_deep_vs_shallow_all$date)) +
  # theme(
  #   strip.background = element_rect(fill=NA),
  #   strip.text = element_text(face="bold")
  # )

# Let's look more closely at Shallow GW-2 and Deep GW-1
q_sc_deep_vs_shallow_all %>% 
  filter(facet_nm %in% c("Shallow GW-6", "Deep GW-1")) %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  filter(year >= 2017) %>% 
  ggplot(aes(x=doy, y=SpecCond, color=season, group=year)) +
  geom_line() + 
  facet_grid(facet_nm ~ year, scales="free_y") +
  scale_color_manual(values = c('Winter'='#58afc6', 'Spring'='#78be20', 
                                'Summer'='#e7cf31', 'Fall'='#9e3e26')) +
  ggtitle("Timeseries of specific conductance", 
          subtitle="Sites with positive SC trends in winter/spring & at least 5 yrs of recent data") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill=NA),
    strip.text.y = element_text(face="bold", angle=0)
  )

# Compare annual SC patterns across GW signatures and sites
deep_vs_shallow_vs_atmos <- sc_trends_with_site_info %>% 
  # At GW signature info
  left_join(site_gw_signature, by = "site_no") %>% 
  # Remove sites that don't have winter/spring positive trends
  filter(site_no %in% sites_with_pos_winterspring) %>%
  # Remove sites that don't have at least 5 years of data up to 2020
  filter(site_no %in% sites_meeting_data_criteria)

q_sc_data_baseq %>% 
  # filter(site_no %in% deep_vs_shallow_vs_atmos$site_no) %>% 
  filter(site_no %in% c("01104455", "01104475")) %>% 
  left_join(site_gw_signature, by = "site_no") %>% 
  filter(GW_cat != "Atmospheric Signature") %>% 
  mutate(season = ifelse(month(date) %in% c(12,1,2), 'Winter',
                         ifelse(month(date) %in% c(3,4,5), "Spring",
                                ifelse(month(date) %in% c(6,7,8), "Summer", "Fall"))),
         doy = yday(date),
         year = year(date),
         # Used to make sure the lines aren't connecting weirdly
         line_grouping_var = sprintf('%s-%s', site_no, year),
         # Set the SpecCond variable to plot
         # sc_plot = log10(SpecCond),
         sc_plot = log10(SpecCond)) %>% 
  # filter(year >= 2021) %>%
  ggplot(aes(x=doy, y=sc_plot, group=line_grouping_var)) +
  # Background rectangles for seasons
  geom_rect(xmin=0, xmax=60, aes(ymin=min(sc_plot, na.rm=T), ymax=max(sc_plot, na.rm=T)), fill="#58afc6") +
  geom_rect(xmin=60, xmax=152, aes(ymin=min(sc_plot, na.rm=T), ymax=max(sc_plot, na.rm=T)), fill="#78be20") +
  geom_rect(xmin=152, xmax=244, aes(ymin=min(sc_plot, na.rm=T), ymax=max(sc_plot, na.rm=T)), fill="#e7cf31") +
  geom_rect(xmin=244, xmax=335, aes(ymin=min(sc_plot, na.rm=T), ymax=max(sc_plot, na.rm=T)), fill="#9e3e26") +
  geom_rect(xmin=335, xmax=366, aes(ymin=min(sc_plot, na.rm=T), ymax=max(sc_plot, na.rm=T)), fill="#58afc6") +
  geom_line(color = "black", alpha=0.25, lwd=0.75) + 
  facet_grid(GW_cat ~ ., scales="free_y") +
  ylab('Logged specific conductance') +
  ggtitle("Timeseries of specific conductance for two specific sites", 
          subtitle="Sites had positive SC trends in winter/spring & at least 5 yrs of recent data") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill=NA),
    strip.text.y = element_text(face="bold", angle=0)
  )

# Now do the above but only show data for baseflow days.
q_sc_deep_vs_shallow_baseflow <- q_sc_deep_vs_shallow_all %>% 
  # Filter out any days with stormflow
  filter(Q_Storm == 0)
ggplot(q_sc_deep_vs_shallow_baseflow, aes(x=date, y=SpecCond, color=season, group=line_grp)) +
  geom_point() + 
  facet_grid(facet_nm ~ ., scales="free_y") +
  scale_color_manual(values = c('Winter'='#58afc6', 'Spring'='#78be20', 
                                'Summer'='#e7cf31', 'Fall'='#9e3e26')) +
  ggtitle("Sites with positive SC trends in winter/spring") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill=NA),
    strip.text.y = element_text(face="bold", angle=0)
  )

# Now visualize the trends that were determined. Do the sites we have follow the
# patterns that were hypothesized above?
ggplot(deep_vs_shallow_summer, aes(x=GW_cat, fill=trend)) +
  geom_histogram(position=position_dodge(), stat="count")

# Do the above but only for sites with high transmissivity
deep_vs_shallow_summer %>% 
  filter(Ts_plot >= 50) %>% 
  ggplot(aes(x=GW_cat, fill=trend)) +
  geom_histogram(position=position_dodge(), stat="count")

# Do the above but not just for summer baseflow.
deep_vs_shallow_alltrends %>% 
  # filter(hydro_cond == "all") %>% 
  ggplot(aes(x=GW_cat, fill=trend)) +
  geom_histogram(position=position_dodge(), stat="count")

# To avoid the issues with my trend analysis, is there a relationship between
# GW signature and whether SC maximums occur in the summer?
site_year_max_sc %>%
  left_join(site_gw_signature, by = "site_no") %>% 
  group_by(site_no, GW_cat) %>% 
  summarize(n_yrs_summer_max = sum(summer_max, na.rm=TRUE)) %>% 
  filter(!is.na(GW_cat), GW_cat != "Atmospheric Signature")

# Sort-of meets my hypothesis, except there are more positive summer baseflow trends
#   for the Deep GW sites than I would have expected. There could be a few things 
#   happening: rudimentary trend analysis + rudimentary baseflow separation could
#   be impacting. In addition, they have a GW signature but should I pair with
#   the Ts and DTW datasets to get a sense of how connected they are?
# Ecological research questions: 
#   1) are sites with deep GW signatures less susceptible to freshwater salinization 
#       due to the source of their baseflow? 
#   2) how many rivers are at risk of decreasing baseflow and increasing salinization,
#      especially during low-flow conditions when organisms are most vulnerable?
# Can I pull in GW chloride measurements?

# LAG: how long do places with deep vs shallow GW take to recover from storm
# that washes salt? 
# Plot storm flow as bar plots across the top like precip charts
# Use BFI for baseflow? Represents % of flow that is baseflow per site.

x<-q_sc_deep_vs_shallow_all %>%
  filter(facet_nm %in% c("Shallow GW-6", "Deep GW-2",
                         "Shallow GW-8", "Deep GW-4")) %>% 
  mutate(doy = yday(date),
         year = year(date),
         season = ifelse(month(date) %in% c(12,1,2), 'Winter',
                         ifelse(month(date) %in% c(3,4,5), "Spring",
                                ifelse(month(date) %in% c(6,7,8), "Summer", "Fall")))) %>% 
  filter(year >= 2017) %>% 
  mutate(Q_BFI_Base = Q*BFI,
         Q_BFI_Storm = Q*(1-BFI)) %>% 
  select(facet_nm, site_no, date, year, season, Q, Q_Base, Q_BFI_Base, Q_Storm, Q_BFI_Storm, SpecCond)

# Add a coefficient to be able to transform flow onto SC
axis_coeff <- max(x$SpecCond)/max(x$Q_BFI_Storm)
axis_coeff <- max(x$SpecCond)/max(x$Q_Storm)

# sc_timeseries <- 
ggplot(x, aes(x=date, y=SpecCond, color = season, group=year)) +
  # Add second y-axis with stormflow
  scale_y_continuous(name="SpecCond, uS/cm @25C", sec.axis=sec_axis(~.*axis_coeff, name="Stormflow, m3s")) +
  geom_bar(aes(y=Q_Storm*axis_coeff), stat="identity", fill="darkgrey", color=NA, alpha=0.4) +
  geom_line() + 
  facet_grid(facet_nm ~ .) +
  scale_color_manual(values = c('Winter'='#58afc6', 'Spring'='#78be20', 
                                'Summer'='#e7cf31', 'Fall'='#9e3e26')) +
  ggtitle("Timeseries of specific conductance", 
          subtitle="Sites with positive SC trends in winter/spring & at least 5 yrs of recent data") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill=NA),
    strip.text.y = element_text(face="bold", angle=0)
  )

# Instead of a second y-axis, facet the two variables
x %>% 
  select(facet_nm, date, season, year, SpecCond, Q_plot = Q_BFI_Storm) %>% 
  pivot_longer(cols = c("SpecCond", "Q_plot")) %>% 
  mutate(line_color = case_when(
    name == "Q_plot" ~ "darkgrey",
    name == "SpecCond" & season == "Winter" ~ '#58afc6',
    name == "SpecCond" & season == "Spring" ~ '#78be20',
    name == "SpecCond" & season == "Summer" ~ '#e7cf31',
    name == "SpecCond" & season == "Fall" ~ '#9e3e26',
  )) %>% 
  # Rename so that plot facet names look good
  mutate(name = ifelse(name == "Q_plot", "Stormflow\nm3/s", "Specific cond.\nuS/cm @25C")) %>% 
  ggplot(aes(x=date, y=value, color=line_color, group=year)) +
  geom_line() + 
  scale_color_identity() +
  facet_grid(facet_nm+name ~ ., scales="free_y") +
  ggtitle("Timeseries of specific conductance & stormflow (grey) using BFI from Hare et al", 
          subtitle="Selected 2 deep & 2 shallow GW signature sites from those sites with positive SC trends in winter/spring & at least 5 yrs of recent data") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill=NA),
    strip.text.y = element_text(face="bold", angle=0)
  )

##### Deep vs shallow vs atmospheric signatures after storm #####

# Find a site for each category (extra: that are relatively close)
# Identify a winter storm where salt was likely applied (can be
# different storms)
# Plot timeseries of SC vs days after storm
# Look at SC peak/recovery trend

site_gw_signature <- readxl::read_excel('1_fetch/in/hare_et_al/sourcedatafigs1,2and3.xlsx')  %>% 
  rename(site_no = SW_ID) %>% 
  filter(site_no %in% unique(tar_read(q_sc_data_baseq)$site_no)) %>%
  mutate(HDI = as.numeric(HydroDisturbIdx),
         PctImpervious = as.numeric(PctImp2011Cat),
         PctSlope = as.numeric(PctWSlope),
         BFI = as.numeric(BFI)) %>% 
  select(site_no, GW_cat, HDI, PctImpervious, PctSlope, BFI)

# deep_vs_shallow_vs_atmos <- tar_read(sc_trends_with_site_info) %>% 
#   # At GW signature info
#   left_join(site_gw_signature, by = "site_no") %>% 
#   filter(!is.na(GW_cat)) %>% 
#   # Only keep things with pretty high salting use
#   filter(salt_mean >= 25000) %>% 
#   # Used a interactive map to identify three sites that are close:
#   filter(site_no %in% c('01104370', '01104455', '01104475'))

# sites_to_use <- deep_vs_shallow_vs_atmos %>% 
#   select(site_no, GW_cat) %>% 
#   distinct()

# # Map those sites
# sites_to_use_sf <- dataRetrieval::readNWISsite(sites_to_use$site_no) %>% 
#   st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs=4326)
# sites_sf <- st_transform(tar_read(q_sc_sites_sf), usmap::usmap_crs()) %>% 
#   filter(site_no %in% sites_to_use$site_no) %>% 
#   left_join(sites_to_use)
# p<-ggplot() +
#   geom_sf(data=tar_read(conus_nosalt_sf), fill='#b8b8b8', color=NA) +
#   geom_sf(data=tar_read(conus_salt_sf), fill='#f4f4f4', color='#898989') +
#   geom_sf(data=sites_sf, aes(color=site_no), shape=17) +
#   theme_void()
# plotly::ggplotly(p)

sc_data <- tar_read(q_sc_data_baseq) %>% 
  filter(site_no %in% c('01104370', '01104455', '01104475')) %>% 
  left_join(site_gw_signature)

# Identify a winter storm:
x<-sc_data %>% 
  filter(is_winter, Q_Storm > 0) %>% 
  # Back-and-forth on visuals, resulted in me finding a Feb 2018 storm
  filter(date >= as.Date('2018-02-04'),
         date <= as.Date('2018-02-15')) 
ggplot(x, aes(x=date, y=SC_flow_normalized, color=GW_cat)) +
  geom_line() +
  facet_grid(site_no ~ ., scales="free_y")
