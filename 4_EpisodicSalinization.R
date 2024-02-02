# Targets for identifying sites that exhibit "episodic salinization"

source('4_EpisodicSalinization/src/find_event_peaks.R')
source('4_EpisodicSalinization/src/summarize_sc_peaks.R')
source('4_EpisodicSalinization/src/ts_normalization.R')

# TODO: change the name once we are happy with skipping clustering. Don't want
# to overwrite those targets yet since they were *LENGTHY* to build.
p4b_targets <- list(
  
  # Normalize the specific conductance before calculating peaks
  tar_target(p4b_ts_sc_norm, normalize_data_bysite(p3_ts_sc_qualified, 'SpecCond')),
  
  # Calculate event peaks for all sites 
  tar_target(p4_ts_sc_peaks, {
    p4b_ts_sc_norm %>% 
      split(.$site_no) %>%
      map(~find_event_peaks(ts_data = .x,
                            date_colname = 'dateTime',
                            param_colname = 'SpecCond_norm',
                            # TODO: We could look at adjusting what makes it a "peak"
                            sb_pk_thresh = 0.000005,
                            sf_pk_thresh = 0.1) 
      ) %>% bind_rows()
  }),
  
  # Now summarize the peak information and filter to just those sites that meet 
  # our criteria for exhibiting "episodic" patterns in winter.
  tar_target(p4_ts_sc_peak_summary, 
             summarize_salt_peaks(p4_ts_sc_peaks, min_winter_perc = 0.40, min_perc_diff = 0.10)),
  tar_target(p4_episodic_sites, filter(p4_ts_sc_peak_summary, is_salt_site)$site_no)
  
  # TODO: revisit three that are still questionable - c('02042500', '04024000', '04067500')
  
  # Hilary's original list:
  # tar_target(p4b_episodic_sites, c("01095434", "01104415", "01104420", "01104455", "01104460", 
  #                                 "01645704", "01645762", "01646000", "01646305", "01654000", "01656903", 
  #                                 "03098600", "03099500", "03106000", "03293000", "03302000", "04119400", 
  #                                 "04142000", "04157005", "04165500", "04166500", "04176500", "04199500", 
  #                                 "04200500", "04208000", "05289800", "06894000", "01104370", "01400500", 
  #                                 "01408000", "01408029", "01481500", "01573695", "01573710", "01649190", 
  #                                 "01649500", "01650800", "03254550", "03262001", "03277075", "040851385", 
  #                                 "040871488", "06893390", "06893620", "06893820", "06893970"))
  
)
