### Time to CRSR function
time_to_crsr <- function(data, database){

  # Get the raw CRSR data using the get CRSR func
  raw_crsr_df <- crsr_func(data = data, database = database)

  # Get the gose data for wlst and discharge_gose data
  gose_df <- gose_func(data = data)

  # Get diagnosis date and icu discahrge date
  icu_date_df <- demo_func(data = data, database = database) %>%
    select(record_id, primary_adm_dx_onset, icu_dch_date)

  crsr_recov_min_dt <- raw_crsr_df %>%
    select(record_id, date, crsr_total, cs_group) %>%
    group_by(record_id) %>%
    filter(crsr_total >= 8 ) %>%
    slice_min(date, with_ties = FALSE) %>%
    ungroup() %>%
    select(-crsr_total, -cs_group) %>%
    rename(min_date = date)

  crsr_wlst_df <- gose_df %>%
    select(record_id, wlst, discharge_gose)

  time_crsr_df <- raw_crsr_df %>%
    select(-time, -cs_group) %>%
    left_join(crsr_wlst_df, by = "record_id") %>%
    left_join(crsr_recov_min_dt, by = "record_id") %>%
    left_join(icu_date_df, by = "record_id") %>%
    group_by(record_id) %>%
    filter(date >= primary_adm_dx_onset) %>%
    filter(date <= icu_dch_date) %>%
    mutate(crsr_recov = ifelse(any(crsr_total >= 8), "yes", "no")) %>%
    mutate(crsr_status = case_when(crsr_recov == "yes" ~ 1,
                                   crsr_recov == "no" & !discharge_gose == 1 ~ 2,
                                   crsr_recov == "no" & wlst == 0 & discharge_gose == 1 ~ 0,
                                   crsr_recov == "no" & wlst == 1 ~ 2),
           crsr_time_to = case_when(crsr_status == 1 ~ as.numeric(min_date - primary_adm_dx_onset),
                                    crsr_status == 0 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                                    crsr_status == 2 & wlst == 1 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                                    crsr_status == 2 & wlst == 0 ~ 31)) %>%
    select(-date, -crsr_total) %>%
    select(record_id, crsr_recov, crsr_status, crsr_time_to) %>%
    distinct()

  return(time_crsr_df)
}

################################################################################
################################################################################
################################################################################

### Time to Command score function
time_to_command_foll <- function(data, database){

  # Get the raw CRSR data using the get CRSR func
  raw_command_df <- command_score_func(data = data, database = database)

  # Get the gose data for wlst and discharge_gose data
  gose_df <- gose_func(data = data)

  # Get diagnosis date and icu discahrge date
  icu_date_df <- demo_func(data = data, database = database) %>%
    select(record_id, primary_adm_dx_onset, icu_dch_date)

  command_recov_min_dt <- raw_command_df %>%
    select(record_id, date, command_score) %>%
    group_by(record_id) %>%
    filter(command_score >= 4 ) %>%
    slice_min(date, with_ties = FALSE) %>%
    ungroup() %>%
    rename(min_date = date) %>%
    select(-command_score)

  command_wlst_df <- gose_df %>%
    select(record_id, wlst, discharge_gose)

  time_command_df <- raw_command_df %>%
    select(-time) %>%
    left_join(command_wlst_df, by = "record_id") %>%
    left_join(command_recov_min_dt, by = "record_id") %>%
    left_join(icu_date_df, by = "record_id") %>%
    group_by(record_id) %>%
    filter(date >= primary_adm_dx_onset) %>%
    filter(date <= icu_dch_date) %>%
    mutate(command_recov = ifelse(any(command_score >= 4), "yes", "no")) %>%
    mutate(command_status = case_when(command_recov == "yes" ~ 1,
                                      command_recov == "no" & !discharge_gose == 1 ~ 2,
                                      command_recov == "no" & wlst == 0 & discharge_gose == 1 ~ 0,
                                      command_recov == "no" & wlst == 1 ~ 2),
           command_time_to = case_when(command_status == 1 ~ as.numeric(min_date - primary_adm_dx_onset),
                                       command_status == 0 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                                       command_status == 2 & wlst == 1 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                                       command_status == 2 & wlst == 0 ~ 31)) %>%
    select(-date, -command_score) %>%
    select(record_id, command_recov, command_status, command_time_to) %>%
    distinct()

  return(time_command_df)
}

################################################################################
################################################################################
################################################################################

#Time to MCSp function

time_to_mcsp <- function(data, database){

  # Get the raw CRSR data using the CRSR func
  raw_crsr_df <- crsr_func(data = data, database = database)

  # Get the gose data for wlst and discharge_gose data
  gose_df <- gose_func(data = data)

  # Get diagnosis date and icu discahrge date
  icu_date_df <- demo_func(data = data, database = database) %>%
    select(record_id, primary_adm_dx_onset, icu_dch_date)

  mcsp_recov_min_dt <- raw_crsr_df %>%
    select(record_id, date, crsr_total, cs_group) %>%
    group_by(record_id) %>%
    filter(cs_group == "MCSp" | cs_group == "CS") %>%
    slice_min(date, with_ties = FALSE) %>%
    ungroup() %>%
    select(-crsr_total, -cs_group) %>%
    rename(min_date = date)

  crsr_wlst_df <- gose_df %>%
    select(record_id, wlst, discharge_gose)

  time_mcsp_df <- raw_crsr_df %>%
    select(-time, -crsr_total) %>%
    left_join(crsr_wlst_df, by = "record_id") %>%
    left_join(mcsp_recov_min_dt, by = "record_id") %>%
    left_join(icu_date_df, by = "record_id") %>%
    group_by(record_id) %>%
    filter(date >= primary_adm_dx_onset) %>%
    filter(date <= icu_dch_date) %>%
    mutate(mcsp_recov = ifelse(any(cs_group == "MCSp" | cs_group == "CS"), "yes", "no")) %>%
    mutate(mcsp_status = case_when(mcsp_recov == "yes" ~ 1,
                                   mcsp_recov == "no" & !discharge_gose == 1 ~ 2,
                                   mcsp_recov == "no" & wlst == 0 & discharge_gose == 1 ~ 0,
                                   mcsp_recov == "no" & wlst == 1 ~ 2),
           mcsp_time_to = case_when(mcsp_status == 1 ~ as.numeric(min_date - primary_adm_dx_onset),
                                    mcsp_status == 0 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                                    mcsp_status == 2 & wlst == 1 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                                    mcsp_status == 2 & wlst == 0 ~ 31)) %>%
    select(-date, -cs_group) %>%
    select(record_id, mcsp_recov, mcsp_status, mcsp_time_to) %>%
    distinct()

  return(time_mcsp_df)

}


################################################################################
################################################################################
################################################################################

# Time to MCSm function

time_to_mcspm <- function(data, database){

  # Get the raw CRSR data using the CRSR func
  raw_crsr_df <- crsr_func(data = data, database = database)

  # Get the gose data for wlst and discharge_gose data
  gose_df <- gose_func(data = data)

  # Get diagnosis date and icu discahrge date
  icu_date_df <- demo_func(data = data, database = database) %>%
    select(record_id, primary_adm_dx_onset, icu_dch_date)

  mcspm_recov_min_dt <- raw_crsr_df %>%
    select(record_id, date, crsr_total, cs_group) %>%
    group_by(record_id) %>%
    filter(cs_group == "MCSm" | cs_group == "MCSp" | cs_group == "CS") %>%
    slice_min(date, with_ties = FALSE) %>%
    ungroup() %>%
    select(-crsr_total, -cs_group) %>%
    rename(min_date = date)

  crsr_wlst_df <- gose_df %>%
    select(record_id, wlst, discharge_gose)

  time_mcspm_df <- raw_crsr_df %>%
    select(-time, -crsr_total) %>%
    left_join(crsr_wlst_df, by = "record_id") %>%
    left_join(mcspm_recov_min_dt, by = "record_id") %>%
    left_join(icu_date_df, by = "record_id") %>%
    group_by(record_id) %>%
    filter(date >= primary_adm_dx_onset) %>%
    filter(date <= icu_dch_date) %>%
    mutate(mcspm_recov = ifelse(any(cs_group == "MCSm" | cs_group == "MCSp" | cs_group == "CS"), "yes", "no")) %>%
    mutate(mcspm_status = case_when(mcspm_recov == "yes" ~ 1,
                                    mcspm_recov == "no" & !discharge_gose == 1 ~ 2,
                                    mcspm_recov == "no" & wlst == 0 & discharge_gose == 1 ~ 0,
                                    mcspm_recov == "no" & wlst == 1 ~ 2),
           mcspm_time_to = case_when(mcspm_status == 1 ~ as.numeric(min_date - primary_adm_dx_onset),
                                     mcspm_status == 0 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                                     mcspm_status == 2 & wlst == 1 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                                     mcspm_status == 2 & wlst == 0 ~ 31)) %>%
    select(-date, -cs_group) %>%
    select(record_id, mcspm_recov, mcspm_status, mcspm_time_to) %>%
    distinct()

  return(time_mcspm_df)

}

################################################################################
################################################################################
################################################################################

# Time to GOS-E function

time_to_gose <- function(data, database){

  # Get the gose data for wlst and discharge_gose data
  gose_df <- gose_func(data = data)

  # Get diagnosis date and icu discahrge date
  icu_date_df <- demo_func(data = data, database = database) %>%
    select(record_id, primary_adm_dx_onset, icu_dch_date)

  time_gose_df <- gose_df %>%
    select(-best_gose) %>%
    left_join(icu_date_df, by = "record_id") %>%
    pivot_longer(cols = !c(record_id, primary_adm_dx_onset, icu_dch_date, wlst)) %>%
    group_by(record_id) %>%
    mutate(recovered = ifelse(any(value >= 4, na.rm = TRUE), 1, 0),
           first_occ = which(value >= 4)[1],
           dead = ifelse(recovered == 0 & wlst == 0, which(value == 1)[1], NA)) %>%
    select(-c(name, value)) %>%
    distinct() %>%
    ungroup() %>%
    mutate(status = case_when( (is.na(first_occ) & !is.na(dead)) ~ 0,
                               (!is.na(first_occ) & is.na(dead)) ~ 1,
                               (is.na(first_occ) & is.na(dead)) ~ 2),
           time = case_when(status == 2 & wlst == 0 ~ 360,
                            status == 2 & wlst == 1 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                            status == 1 & first_occ == 1 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                            status == 1 & first_occ == 2 ~ 90,
                            status == 1 & first_occ == 3 ~ 180,
                            status == 1 & first_occ == 4 ~ 360,
                            status == 0 & dead == 1 ~ as.numeric(icu_dch_date - primary_adm_dx_onset),
                            status == 0 & dead == 2 ~ 90,
                            status == 0 & dead == 3 ~ 180,
                            status == 0 & dead == 4 ~ 360 )) %>%
    select(record_id, recovered, status, time) %>%
    rename(gose_recov = recovered,
           gose_status = status,
           gose_time_to = time)

  return(time_gose_df)

}





