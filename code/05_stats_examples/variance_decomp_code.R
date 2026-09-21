

region_time_bw <- df %>%
  group_by(Region) %>% # group by region
  summarize(
    across(
      all_of(region_time_vars),
      ~ mean(.x, na.rm = TRUE), # mean per analyte per region
      .names = '{.col}_bw_region' # name this "bw_region"
    ),
    .groups = 'drop'
  )

# join to the df
df <- df %>%
  left_join(region_time_bw, by = 'Region')

# calculate over time differences (d_rt = X_rt - mean(X_r))
for (v in region_time_vars) {
  df[[paste0(v, '_over_rt')]] <-
    df[[v]] - df[[paste0(v, '_bw_region')]] 
}