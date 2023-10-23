# Time-series clustering?

library(dtwclust)
library(tidyverse)

# Hare et al & Ts
hare_et_al <- readxl::read_excel('1_fetch/in/hare_et_al/sourcedatafigs1,2and3.xlsx') %>%
  select(site_no = SW_ID, GW_cat)
trans_xwalk <- tar_read(q_sc_sites_info) %>% 
  select(site_no, trans_MEAN, trans_PERCENTILE)

# Example from github
data("uciCT")
pc <- tsclust(CharTraj, type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)


# With some SC data
# Each row needs to be a separate time series
ts_long <- tar_read(q_sc_data_baseq) %>% 
  # Filter to sites with high Ts & high DTW
  filter(trans_PERCENTILE >= 75, 
         dtw_PERCENTILE >= 75) %>% 
  # Filter dates for now
  filter(date >= as.Date("2018-10-01"),
         date <= as.Date("2019-09-30")) %>% 
  select(site_no, date, SpecCond) 

ts_list <- ts_long %>% 
  # filter(site_no %in% c("01408000", "01464290")) %>%
  # Fill in missing dates for each site
  complete(site_no, date = unique(ts_long$date)) %>%
  # Split into list where each item is a vector  
  # of the SpecCond timeseries for each site, where
  # NAs are filled in on dates without a value so that
  # all vectors have the same length.
  split(.$site_no) %>%  
  map(pull, var = "SpecCond")
ts_list_rm <- ts_list %>% 
  # For now, remove any vector with NAs because it causes an error
  map(~any(is.na(.))) %>% 
  unlist()
ts_list_adj <- ts_list[!ts_list_rm]

# Run a Dynamic Time-Warping clustering algorithm
pc <- tsclust(ts_list_adj, type = "partitional", k = 2L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 30L)))
plot(pc)

# Run hierarchical version
pc2 <- tsclust(ts_list_adj, type = "hierarchical", k = 2L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              control = hierarchical_control(method = "average"))
plot(pc2)

##### Try clustering on known shallow vs deep site #####


apply_rle <- function(vals) {
  # Use RLE to split values into consecutive runs of the same value
  consecutive_day_grps <- rle(vals)
  
  # Rename the groups
  consecutive_day_grps$values <- sprintf('grp-%s', 1:length(consecutive_day_grps$values))
  
  # Return vector of the same length of `vals` but with group names
  grps <- inverse.rle(consecutive_day_grps)
  return(grps)
}

ts_consec_days <- tar_read(q_sc_data_baseq) %>% 
  # Filter to MA deep vs shallow site that show winter road salting
  filter(site_no %in% c("01095434", "01104455")) %>% 
  # Calculate scaled SC values based on min/max of all site data
  group_by(site_no) %>% 
  mutate(SpecCond_norm = scales::rescale(SpecCond)) %>% 
  ungroup() %>% 
  # Filter to days where baseflow makes up the majority of the flow
  mutate(qfrac_base = Q_Base/Q,
         qfrac_storm = Q_Storm/Q) %>% 
  filter(qfrac_base > qfrac_storm) %>% 
  # Split into timeseries of consecutive days
  group_by(site_no) %>% 
  mutate(next_data_lag = as.numeric(date - lag(date)),
         ts_grp = apply_rle(next_data_lag)) %>% 
  ungroup() %>% 
  mutate(ts_split = sprintf('%s_%s', site_no, ts_grp)) %>% 
  # select(site_no, date, next_data_lag, ts_grp) %>% 
  # Split into list where each item is a vector of the 
  # SpecCond timeseries for each site & ts_grp identified above
  split(.$ts_split) %>%  
  map(pull, var = "SpecCond_norm") %>% 
  # TODO: work on grouping/split because there is an issue with some 
  # dates being given their own grp when they should be in another one
  # Require at least 5 consecutive dates for a timeseries 
  Filter(function(x) length(x) >= 5, .)

# Run a Dynamic Time-Warping clustering algorithm
ts_consec_days_clust <- tsclust(ts_consec_days, type = "partitional", k = 2L, 
                                distance = "dtw_basic", centroid = "pam", 
                                seed = 3247L, trace = TRUE,
                                args = tsclust_args(dist = list(window.size = 30L)))
plot(ts_consec_days_clust)

##### Dynamic Time-Warping with representative annual TS a la Bolotin et al., 2022 #####

ts_bolotin_style <- tar_read(q_sc_data_baseq) %>% 
  # Filter to sites with more than one year of data
  group_by(site_no) %>% 
  mutate(n_unique_yrs = length(unique(year(date)))) %>% 
  filter(n_unique_yrs > 1) %>% 
  # Create a representative annual TS per site based on daily means across years
  mutate(doy = yday(date)) %>% 
  group_by(site_no, doy) %>% 
  summarize(annual_SC = mean(SpecCond, na.rm = TRUE),
            annual_SC_QNorm = mean(SC_flow_normalized, na.rm = TRUE), 
            .groups = "drop") %>%
  # Fill in missing DOY with NAs so that each site has rows 1:366
  group_by(site_no) %>% 
  complete(doy = 1:366) %>% 
  arrange(site_no, doy) %>%  
  # Gap-fill SC values using linear interpolation for gaps <= 4 days
  mutate(annual_SC_gapfilled = zoo::na.approx(annual_SC, maxgap = 4, na.rm = FALSE),
         annual_SC_QNorm_gapfilled = zoo::na.approx(annual_SC_QNorm, maxgap = 4, na.rm = FALSE)) %>% 
  # Remove sites where there are NAs after gap-filling. Don't remove sites with just 1 NA 
  # for doy==366 because it might just be that they don't have any leap year data. 
  mutate(na366 = any(doy == 366 & is.na(annual_SC_gapfilled)),
         num_NAs = sum(is.na(annual_SC_gapfilled))) %>%
  mutate(siteWithTooManyNAs = num_NAs > 1) %>%  
  ungroup() %>% 
  filter(!siteWithTooManyNAs) %>% 
  # Now get rid of the remaining NAs on doy==366 but leave the other 365 days
  # On 6/21/23, 33 sites dropped the doy=366 due to NA
  filter(!is.na(annual_SC_gapfilled))

# Still 375 sites remaining 
length(unique(ts_bolotin_style$site_no))

# SET DTW PARAMS
n_clusters <- 3*2*2 # GW interaction type (3) * pos/neg SC trend (2) * urban/not urban (2)
days_window <- 7

# DTW for SC values
# Split into list where each item is a vector of the annual SC for each site
sc_bolotin_ts_list <- ts_bolotin_style %>% 
  split(.$site_no) %>%  
  map(pull, var = "annual_SC_gapfilled")

dtw_bolotin_sc <- tsclust(sc_bolotin_ts_list, 
                          type = "partitional", 
                          k = n_clusters, 
                          distance = "dtw_basic", 
                          centroid = "pam", 
                          seed = 3247L, 
                          trace = TRUE,
                          args = tsclust_args(dist = list(window.size = days_window)))
plot(dtw_bolotin_sc)

dtw_bolotin_sc_info <- tibble(ts_name = names(sc_bolotin_ts_list),
                              cluster = dtw_bolotin_sc@cluster) %>% 
  separate(ts_name, into = c('site_no', 'year'), sep = "_") %>% 
  left_join(hare_et_al, by = "site_no") %>% 
  left_join(trans_xwalk, by = "site_no")

ggplot(dtw_bolotin_sc_info, aes(x = GW_cat, y = trans_MEAN)) +
  geom_boxplot() +
  facet_grid(cluster ~ .)

# DTW for SC flow normalized values
sc_qnorm_bolotin_ts_list <- ts_bolotin_style %>% 
  split(.$site_no) %>%  
  map(pull, var = "annual_SC_QNorm_gapfilled")

dtw_bolotin_sc_qnorm <- tsclust(sc_qnorm_bolotin_ts_list,
                                type = "partitional", 
                                k = n_clusters, 
                                distance = "dtw_basic", 
                                centroid = "pam", 
                                seed = 3247L, 
                                trace = TRUE,
                                args = tsclust_args(dist = list(window.size = days_window)))
plot(dtw_bolotin_sc_qnorm)

dtw_bolotin_sc_qnorm_info <- tibble(ts_name = names(sc_qnorm_bolotin_ts_list),
                                 cluster = dtw_bolotin_sc_qnorm@cluster) %>% 
  separate(ts_name, into = c('site_no', 'year'), sep = "_") %>% 
  left_join(hare_et_al, by = "site_no") %>% 
  left_join(trans_xwalk, by = "site_no")

ggplot(dtw_bolotin_sc_qnorm_info, aes(x = GW_cat, y = trans_MEAN)) +
  geom_boxplot() +
  facet_grid(cluster ~ .)

##### Dynamic Time-Warping with each annual TS and linear gap-filling #####

ts_gap_filled <- tar_read(q_sc_data_baseq) %>% 
  # Filter to sites with more than one year of data
  group_by(site_no) %>% 
  mutate(n_unique_yrs = length(unique(year(date)))) %>% 
  filter(n_unique_yrs > 1) %>% 
  # Add a year column to use when splitting data and gap-filling
  # This is what differs from Bolotin et al 2022 (not creating annual "representative" timeseries)
  mutate(year = year(date),
         doy = yday(date)) %>%
  # Fill in missing DOY with NAs so that each site-year has rows 1:366
  group_by(site_no, year) %>% 
  complete(doy = 1:366) %>% 
  arrange(site_no, year, doy) %>%  
  # Gap-fill SC values using linear interpolation for gaps <= 4 days
  mutate(SC_gapfilled = zoo::na.approx(SpecCond, maxgap = 4, na.rm = FALSE),
         SC_QNorm_gapfilled = zoo::na.approx(SC_flow_normalized, maxgap = 4, na.rm = FALSE)) %>% 
  # Remove sites where there are NAs after gap-filling. Don't remove sites with just 1 NA 
  # for doy==366 because it might just be that they don't have any leap year data. 
  # In addition, don't remove sites with just 1 NA for doy==1 because it is easy to remove.
  mutate(na366 = any(doy == 366 & is.na(SC_gapfilled)),
         na001 = any(doy == 1 & is.na(SC_gapfilled)),
         num_NAs = sum(is.na(SC_gapfilled))) %>%
  mutate(siteWithTooManyNAs = num_NAs > 1) %>% 
  ungroup() %>% 
  filter(!siteWithTooManyNAs) %>% 
  # Now get rid of the remaining NAs on doy==366 OR doy==1 but leave the other 365 days
  # On 6/21/23, 776 site-years dropped the doy=366 and 2 site-years dropped the doy==1 due to NAs
  filter(!is.na(SC_gapfilled))

# Still 304 sites remaining 
length(unique(ts_gap_filled$site_no))

# Split into list where each item is a vector of the annual SC for each site
sc_gap_filled_ts_list <- ts_gap_filled %>% 
  mutate(site_year = sprintf('%s_%s', site_no, year)) %>% 
  split(.$site_year) %>%  
  map(pull, var = "SC_gapfilled")

sc_qnorm_gap_filled_ts_list <- ts_gap_filled %>% 
  mutate(site_year = sprintf('%s_%s', site_no, year)) %>% 
  split(.$site_year) %>%  
  map(pull, var = "SC_QNorm_gapfilled")

# SET DTW PARAMS
n_clusters <- 12 # GW interaction type (3) * pos/neg SC trend (2) * urban/not urban (2)
days_window <- 14

# DTW for SC values
dtw_gap_filled_sc <- tsclust(sc_gap_filled_ts_list, 
                             type = "partitional", 
                             k = n_clusters, 
                             distance = "dtw_basic", 
                             centroid = "pam", 
                             seed = 3247L, 
                             trace = TRUE,
                             args = tsclust_args(dist = list(window.size = days_window)))
plot(dtw_gap_filled_sc)

# Evaluate using CVIs ... 
# TODO: Do something with this to figure out which number of clusters and windows is best?
cvi(dtw_gap_filled_sc)

dtw_gap_filled_sc_info <- tibble(ts_name = names(sc_gap_filled_ts_list),
                                       cluster = dtw_gap_filled_sc@cluster) %>% 
  separate(ts_name, into = c('site_no', 'year'), sep = "_") %>% 
  left_join(hare_et_al, by = "site_no") %>% 
  left_join(trans_xwalk, by = "site_no")

ggplot(dtw_gap_filled_sc_info, aes(x = GW_cat, y = trans_MEAN)) +
  geom_boxplot() +
  facet_grid(cluster ~ .)

# DTW for SC flow normalized values
dtw_gap_filled_sc_qnorm <- tsclust(sc_qnorm_gap_filled_ts_list, 
                                   type = "partitional", 
                                   k = n_clusters, 
                                   distance = "dtw_basic", 
                                   centroid = "pam", 
                                   seed = 3247L, 
                                   trace = TRUE,
                                   args = tsclust_args(dist = list(window.size = days_window)))
plot(dtw_gap_filled_sc_qnorm)

# TODO: start to look at clusters based on site attributes
# Start with GW interaction & Ts since those are readily available

dtw_gap_filled_sc_qnorm_info <- tibble(ts_name = names(sc_qnorm_gap_filled_ts_list),
       cluster = dtw_gap_filled_sc_qnorm@cluster) %>% 
  separate(ts_name, into = c('site_no', 'year'), sep = "_") %>% 
  left_join(hare_et_al, by = "site_no") %>% 
  left_join(trans_xwalk, by = "site_no")

ggplot(dtw_gap_filled_sc_qnorm_info, aes(x = GW_cat, y = trans_MEAN)) +
  geom_boxplot() +
  facet_grid(cluster ~ .)
