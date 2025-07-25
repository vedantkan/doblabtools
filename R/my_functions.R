library(tidyverse)

### Function to get gose from all the data

gose_func <- function(data){
  require(tidyverse)
  data <- tibble::as_tibble(data)
  df <- data %>%
    select(record_id, wlst, discharge_gose, follow_up_period.factor, gose) %>%
    filter(!(is.na(follow_up_period.factor) & is.na(gose) & is.na(wlst) & is.na(discharge_gose) )) %>%
    group_by(record_id) %>%
    fill(discharge_gose, .direction = "updown") %>%
    fill(wlst, .direction = "updown") %>%
    mutate(gose_3m = ifelse(follow_up_period.factor == "3 Month", gose, NA),
           gose_6m = ifelse(follow_up_period.factor == "6 Month", gose, NA),
           gose_12m = ifelse(follow_up_period.factor == "12 Month", gose, NA),
           gose_ot = ifelse(follow_up_period.factor == "Other", gose, NA)) %>%
    fill(gose_3m, .direction = "updown") %>%
    fill(gose_6m, .direction = "updown") %>%
    fill(gose_12m, .direction = "updown") %>%
    fill(gose_ot, .direction = "updown") %>%
    select(-gose, -follow_up_period.factor) %>%
    distinct() %>%
    ungroup() %>%
    mutate(best_gose = pmax(discharge_gose, gose_3m, gose_6m, gose_12m, gose_ot, na.rm = TRUE))

  return(df)
}

mrs_func <- function(data){
  require(tidyverse)
  data <- tibble::as_tibble(data)
  df <- data %>%
    select(record_id, wlst, discharge_mrs, follow_up_period.factor, mrs) %>%
    filter(!(is.na(follow_up_period.factor) & is.na(mrs) & is.na(wlst) & is.na(discharge_mrs) )) %>%
    group_by(record_id) %>%
    fill(discharge_mrs, .direction = "updown") %>%
    fill(wlst, .direction = "updown") %>%
    mutate(mrs_3m = ifelse(follow_up_period.factor == "3 Month", mrs, NA),
           mrs_6m = ifelse(follow_up_period.factor == "6 Month", mrs, NA),
           mrs_12m = ifelse(follow_up_period.factor == "12 Month", mrs, NA),
           mrs_ot = ifelse(follow_up_period.factor == "Other", mrs, NA)) %>%
    fill(mrs_3m, .direction = "updown") %>%
    fill(mrs_6m, .direction = "updown") %>%
    fill(mrs_12m, .direction = "updown") %>%
    fill(mrs_ot, .direction = "updown") %>%
    select(-mrs, -follow_up_period.factor) %>%
    distinct() %>%
    ungroup() %>%
    mutate(best_mrs = pmin(discharge_mrs, mrs_3m, mrs_6m, mrs_12m, mrs_ot, na.rm = TRUE))

  return(df)
}

command_score_func <- function(data, database = "consciousness", type = "all"){

  require(tidyverse)
  data <- tibble::as_tibble(data)

  if(database == "consciousness"){
    date <- "test_datetime"
    time <- "test_datetime"
  }
  else if(database == "reconfig"){
    date <- "clinical_assess_date"
    time <- "clinical_assess_time"
  }
  else if(database == "aphasia"){
    date <- "test_date"
    time <- "test_date"
  }

  if(date == time){
    df <- data %>%
      select(record_id, !!sym(date), command_score) %>%
      filter(!(is.na(command_score) )) %>%
      mutate(date = as.Date(stringr::str_sub(!!sym(date), 1, 10)),
             time = stringr::str_sub(!!sym(date), 12, ),
             .after = !!sym(date)) %>%
      select(-!!sym(date)) %>%
      group_by(record_id) %>%
      arrange(date, .by_group = TRUE) %>%
      mutate(command_yn = ifelse(any(command_score >= 4), 1, 0)) %>%
      ungroup()
  }
  else{
    df <- data %>%
      select(record_id, !!sym(date), !!sym(time), command_score) %>%
      filter(!(is.na(command_score))) %>%
      group_by(record_id) %>%
      rename(date = !!sym(date),
             time = !!sym(time)) %>%
      arrange(date, .by_group = TRUE) %>%
      mutate(command_yn = ifelse(any(command_score >= 4), 1, 0)) %>%
      ungroup()
  }

  if(type == "raw"){
    df <- df %>%
      select(-command_yn)
  }
  else if(type == "yesno"){
    df <- df %>%
      select(record_id, command_yn) %>%
      group_by(record_id) %>%
      distinct() %>%
      ungroup()
  }
  else if(type == "all"){
    df <- df
  }

  return(df)
}

crsr_func <- function(data, database = "consciousness", type = "all"){

  require(tidyverse)
  data <- tibble::as_tibble(data)

  if(database == "consciousness"){
    date <- "test_datetime"
    time <- "test_datetime"
  }
  else if(database == "reconfig"){
    date <- "clinical_assess_date"
    time <- "clinical_assess_time"
  }
  else if(database == "aphasia"){
    date <- "test_date"
    time <- "test_date"
  }

  if(date == time){
    df <- data %>%
      select(record_id, !!sym(date), crsr_auditory, crsr_visual, crsr_motor,
             crsr_oromotor_verbal, crsr_communication, crsr_arousal, crsr_total,
             brainstem_response_sedation, eeg_sedation) %>%
      filter(!is.na(crsr_total)) %>%
      mutate(date = as.Date(stringr::str_sub(!!sym(date), 1, 10)),
             time = stringr::str_sub(!!sym(time), 12, ),
             .after = !!sym(date)) %>%
      select(-!!sym(date))
  }
  else{
    df <- data %>%
      select(record_id, !!sym(date), !!sym(time), crsr_auditory, crsr_visual, crsr_motor,
             crsr_oromotor_verbal, crsr_communication, crsr_arousal, crsr_total,
             brainstem_response_sedation, eeg_sedation) %>%
      filter(!is.na(crsr_total)) %>%
      rename(date = !!sym(date),
             time = !!sym(time))
  }

  df <- df %>%
    mutate(cs_group = case_when(
      crsr_arousal == 0 & crsr_auditory < 3 & crsr_visual < 2 & crsr_motor < 3 & crsr_oromotor_verbal <= 1 & crsr_communication == 0 ~ "Coma",
      crsr_arousal > 0 & crsr_auditory < 3 & crsr_visual < 2 & crsr_motor < 3 & crsr_oromotor_verbal < 3 & crsr_communication < 1 ~ "VS",
      (crsr_visual %in% 1:4 | (crsr_motor > 2 & crsr_motor < 6)) & (crsr_auditory < 3 & crsr_oromotor_verbal < 3 & crsr_communication < 1) ~ "MCSm",
      (crsr_auditory >= 3 | crsr_oromotor_verbal == 3 | crsr_communication == 1) & (crsr_communication < 2 & crsr_motor < 6) ~ "MCSp",
      crsr_communication == 2 | crsr_motor == 6 ~ "CS")) %>%
    rename(brain_sed = brainstem_response_sedation,
           eeg_sed = eeg_sedation) %>%
    select(record_id, date, time, crsr_total, cs_group, brain_sed, eeg_sed) %>%
    group_by(record_id) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(crsr_yn = ifelse(any(crsr_total >= 8), 1, 0),
           .after = crsr_total)

  if(type == "raw"){
    df <- df %>%
      select(-crsr_yn)
  }
  else if(type == "yesno"){
    df <- df %>%
      select(record_id, crsr_yn) %>%
      group_by(record_id) %>%
      distinct() %>%
      ungroup()
  }
  else if(type == "all"){
    df <- df
  }

  return(df)

}

demo_func <- function(data, database = "consciousness"){

  require(tidyverse)
  data <- tibble::as_tibble(data)

  if(database == "consciousness"){
    etiology <- "primary_adm_dx"
  }
  else if(database == "reconfig" | database == "aphasia"){
    etiology <- "ich_etiology_smashu"
  }

  df <- data %>%
    select(record_id, dob, sex.factor, race.factor, handedness.factor, years_of_education.factor,
           primary_adm_dx_onset, (ends_with("_date") & contains(c("hosp", "icu")) & !starts_with("outside_")), !!sym(etiology),
           gcs_tot_adm) %>%
    group_by(record_id) %>%
    fill(all_of(colnames(.)[-1])) %>%
    distinct() %>%
    ungroup() %>%
    mutate(age = as.numeric(difftime(primary_adm_dx_onset, dob, units = "days")/365.25),
           .after = dob)

  if("icu_admission_date" %in% colnames(df)){
    df <- df %>%
      rename("icu_adm_date" = "icu_admission_date")
  }

  if("primary_adm_dx" %in% colnames(df)){
    df <- df %>%
      mutate(primary_adm_dx = case_when(primary_adm_dx == 1 ~ "sah",
                                        primary_adm_dx == 2 ~ "ich",
                                        primary_adm_dx == 3 ~ "ca",
                                        primary_adm_dx == 4 ~ "ais",
                                        primary_adm_dx == 5 ~ "tbi",
                                        primary_adm_dx == 6 ~ "sdh",
                                        primary_adm_dx == 7 ~ "Encephalitis",
                                        primary_adm_dx == 9 ~ "Brain Tumor",
                                        primary_adm_dx == 10 ~ "Other",
                                        primary_adm_dx == 11 ~ "Status Epilepticus",
                                        primary_adm_dx == 12 ~ "Sepsis",
                                        primary_adm_dx == 13 ~ "Polyneuropathy",
                                        primary_adm_dx == 14 ~ "Encephalopathy",
                                        .default = NA)) %>%
      mutate(new_dx = case_when(primary_adm_dx %in% c("sah", "ich", "ais", "Brain Tumor") ~ "struc_wo_tbi",
                                primary_adm_dx %in% c("tbi", "sdh") ~ "struct_w_tbi",
                                primary_adm_dx %in% c("ca","Encephalitis", "Other", "Status Epilepticus",
                                                      "Sepsis", "Polyneuropathy", "Encephalopathy") ~ "non_structure",
                                .default = NA))
  }
  else if(!"primary_adm_dx" %in% colnames(df)){
    df <- df %>%
      mutate(primary_adm_dx = "ich",
             new_dx = "struc_wo_tbi")
  }

  if(length(unique(df$record_id)) == nrow(df) ){
    return(df)
  }
  else{
    return(NA)
  }

}


