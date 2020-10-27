download_tx2036_files <- function(){
  download.file(url = "https://texas-2036.github.io/covid-data/bed_and_vents.csv",
                destfile = "data/dshs-tx-2036-old-data.csv")
  
  download.file(url = "https://raw.githubusercontent.com/texas-2036/covid_tracker/master/clean_data/dshs/hospitals/new_hosp_data_dshs.csv",
                destfile = "data/dshs-tx-2036-new-data.csv")
  
  download.file(url = "https://dshs.texas.gov/coronavirus/CombinedHospitalDataoverTimebyTSA.xlsx",
                destfile = "data/dshs-hosp-data.xlsx")
}

get_capacities_and_usage <- function(max_date_to_use= '2021-12-28', download=T){
  if(download){
    download_tx2036_files()
  }
  
  old_df <- read_csv("data/dshs-tx-2036-old-data.csv") %>% mutate(date = as.Date(date))
  new_df <- read_csv("data/dshs-tx-2036-new-data.csv")
  
  # Get TSA capacity numbers ------------------------------------------------
  state_capacity <- old_df %>% 
    filter(date == "2020-07-21") %>% 
    summarize(hospital_capacity = sum(total_beds_available) + sum(total_laboratory_confirmed),
              icu_capacity = sum(icu_beds_available) + sum(lab_con_icu),
              icu_ratio = sum(lab_con_icu)/sum(total_laboratory_confirmed))
  
  
  tsa_capacities <- old_df %>% 
    filter(date == "2020-07-21") %>% 
    mutate(hospital_capacity = total_beds_available + total_laboratory_confirmed,
           icu_capacity = icu_beds_available + lab_con_icu,
           icu_ratio = lab_con_icu/total_laboratory_confirmed) %>% 
    select(tsa, hospital_capacity, total_beds_available, total_laboratory_confirmed, icu_beds_available, lab_con_icu, icu_capacity, icu_ratio) %>% 
    bind_rows(tibble(tsa = "Z") %>% 
                bind_cols(state_capacity))
  
  # Create TSA mapping table ------------------------------------------------
  state_label <- "Statewide"
  
  
  tsa_map <- old_df %>%
    distinct(tsa, location) %>% 
    rename(tsa_letter = tsa, tsa_name_wrong = location) %>% 
    mutate(tsa_name = ifelse(tsa_name_wrong == "Witchita Falls", "Wichita Falls", tsa_name_wrong)) %>% 
    mutate(tsa_plot_label = paste0("(", tsa_letter, ") ", tsa_name)) %>% 
    bind_rows(tibble(tsa_letter = "Z",
                     tsa_name_wrong = state_label,
                     tsa_name = state_label,
                     tsa_plot_label = state_label))
  
  
  # Get TSA regional time-series data ---------------------------------------
  tsa_ts <- old_df %>% 
    select(tsa, date, 
           total_hospitalized = total_laboratory_confirmed, 
           total_icu = lab_con_icu) %>% 
    filter(date < "2020-07-22") %>% 
    bind_rows(new_df %>% 
                filter(tsa != "Total") %>% 
                mutate(total_hospitalized = tot_confirmed_covid_patients_in_hosp) %>%
                filter(date <= max_date_to_use) %>% 
                # mutate(total_hospitalized = ifelse(is.na(tot_confirmed_covid_patients_in_hosp),0,tot_confirmed_covid_patients_in_hosp) + 
                #          ifelse(is.na(tot_suspected_covid_patients_in_hosp),0,tot_suspected_covid_patients_in_hosp)) %>% 
                select(tsa, date, total_hospitalized) %>% 
                # mutate(total_hospitalized = ifelse(date >="2020-07-22" & date <= "2020-07-31", NA, total_hospitalized)) %>% 
                mutate(total_icu = NA))
  
  tsa_ts <- tsa_ts %>% 
    bind_rows(tsa_ts %>% 
                group_by(date) %>% 
                summarize(total_hospitalized = sum(total_hospitalized),
                          total_icu = sum(total_icu)) %>% 
                mutate(tsa = "Z")) %>% 
    nest(hosp_ts = c("date", "total_hospitalized", "total_icu"))
  
  
  # Get TSA population stats ------------------------------------------------
  tsa_pop_stats <- read_csv("data/TSAPopulation_RiskGroups.csv") %>% 
    mutate(population = `0-4` + `5-17` + `18-49` + `50-64` + `65+`) %>% 
    group_by(TSA) %>% 
    summarize(population = sum(population))
  
  tsa_pop_stats <- tsa_pop_stats %>% 
    bind_rows(tsa_pop_stats %>% summarize(TSA = "Z", population = sum(population)))
  
  tsa_map %>% 
    left_join(tsa_capacities, by = c("tsa_letter" = "tsa")) %>% 
    left_join(tsa_pop_stats, by = c("tsa_letter" = "TSA")) %>% 
    left_join(tsa_ts, by = c("tsa_letter" = "tsa")) 
  
  # browser()
  # Use actual DSHS data ----------------------------------------------------
  ## Uses DSHS hospitalization data directly
  # browser()
  dshs_hosp_data <- readxl::read_xlsx("data/dshs-hosp-data.xlsx", sheet = 4, skip = 2)%>% 
    filter(!is.na(`TSA AREA`)) %>%
    rename(tsa_letter = `TSA ID`,
           tsa_name = `TSA AREA`) %>% 
    gather(date, total_hospitalized, -tsa_letter, -tsa_name) %>% 
    mutate(tsa_letter = str_replace(tsa_letter, '\\.', '')) %>% 
    mutate(date = ifelse(date == '39668', '2020-08-08', date),
           date = ifelse(date == '44059', '2020-08-16', date)) %>% 
    left_join(readxl::read_xlsx("data/dshs-hosp-data.xlsx", sheet = 7, skip = 2) %>% 
                filter(!is.na(`TSA AREA`)) %>%
                rename(tsa_letter = `TSA ID`,
                       tsa_name = `TSA AREA`) %>% 
                gather(date, total_icu, -tsa_letter, -tsa_name) %>% 
                mutate(tsa_letter = str_replace(tsa_letter, '\\.', ''))) %>% 
    left_join(readxl::read_xlsx("data/dshs-hosp-data.xlsx", sheet = 8, skip = 2) %>% 
                filter(!is.na(`TSA AREA`)) %>%
                rename(tsa_letter = `TSA ID`,
                       tsa_name = `TSA AREA`) %>% 
                gather(date, available_beds, -tsa_letter, -tsa_name) %>% 
                mutate(tsa_letter = str_replace(tsa_letter, '\\.', ''))) %>% 
    left_join(readxl::read_xlsx("data/dshs-hosp-data.xlsx", sheet = 9, skip = 2) %>% 
                filter(!is.na(`TSA AREA`)) %>%
                rename(tsa_letter = `TSA ID`,
                       tsa_name = `TSA AREA`) %>% 
                gather(date, available_icus, -tsa_letter, -tsa_name) %>% 
                mutate(tsa_letter = str_replace(tsa_letter, '\\.', '')) ) %>% 
    mutate(date = ymd(date)) %>% 
    mutate(tsa_letter = ifelse(tsa_letter == 'Total', 'Z', tsa_letter),
           tsa_name = ifelse(tsa_name == 'Statewide Total', 'Statewide', tsa_name))
  
  tsa_map %>% 
    left_join(dshs_hosp_data %>% 
                filter(date == max(date)) %>% 
                mutate(hospital_capacity = total_hospitalized + available_beds,
                       icu_capacity = total_icu + available_icus,
                       icu_ratio = total_icu/total_hospitalized) %>% 
                select(tsa_letter,tsa_name,hospital_capacity, icu_capacity, icu_ratio)) %>% 
    left_join(tsa_pop_stats, by = c("tsa_letter" = "TSA")) %>% 
    left_join(dshs_hosp_data %>% 
                nest(hosp_ts = c('date', 'total_hospitalized', 'total_icu', 'available_beds', 'available_icus'))) 
  
}


fix_icu_numbers <- function(tsa_proj, dshs_df){
  tsa_proj %>% 
    unnest(data) %>% 
    left_join(dshs_df %>%
                mutate(state = 'icu'), by = c("tsa" = "tsa_name_wrong", "state")) %>% 
    mutate(ratio = ifelse(is.na(icu_ratio), 1, icu_ratio)) %>% 
    mutate(mean = ratio*mean,
           median = ratio*median,
           lo = ratio*lo,
           hi = ratio*hi,
           min = ratio*min,
           max = ratio*max,
           traj1 = ratio*traj1,
           traj2 = ratio*traj2,
           traj3 = ratio*traj3,
           traj4 = ratio*traj4,
           traj5 = ratio*traj5,
           traj6 = ratio*traj6,
           traj7 = ratio*traj7,
           traj8 = ratio*traj8,
           traj9 = ratio*traj9,
           traj10 = ratio*traj10
    ) %>% 
    select(tsa:rt_hi)
}



get_smoothed_ci =   function(states, alpha=0.05) {
  states_mean = apply(states, 1, mean,na.rm=T)
  states_lo = apply(states, 1, function(x){quantile(x,alpha/2,na.rm=T)})
  states_hi = apply(states, 1, function(x){quantile(x,1-alpha/2,na.rm=T)})
  states_median = apply(states, 1, function(x){quantile(x,0.5,na.rm=T)})
  states_min = apply(states, 1, function(x){ min(x)})
  states_max = apply(states, 1, function(x){max(x)})
  
  tibble(mean=states_mean,
         median = states_median,
         lo=states_lo, 
         hi=states_hi,
         min = states_min,
         max = states_max)
}

get_variable_tibble <- function(varname, 
                                smoothed_states,
                                new_covars,
                                nspaghetti = 10){
  ## Assumes smoothed states is already sorted for unique in first columns
  # spaghettis <- smoothed_states[varname,,1:nspaghetti]
  spaghettis <- smoothed_states[varname,,sample(1:500, size = nspaghetti, replace = F)]
  colnames(spaghettis) <- paste0("traj", seq(1:nspaghetti))
  spaghettis <- spaghettis %>% 
    as_tibble()
  
  new_covars %>% 
    select(date, day, future) %>% 
    as_tibble() %>% 
    bind_cols(get_smoothed_ci(smoothed_states[varname,,]) %>% 
                mutate(state = varname) %>% 
                select(state, mean:max),
              spaghettis)
}

get_summary_stats <- function(smoothed_states, new_covars, dshs_df, current_tsa){
  r0_vec <- new_covars %>% 
    select(date, day, future) %>% 
    as_tibble() %>% 
    bind_cols(smoothed_states['Rt',,] %>% as_tibble()) %>% 
    filter(future) %>% 
    head(1) %>% 
    select(V1:V500) %>% 
    t() %>% 
    as.numeric()
  
  state_names <- smoothed_states[,1,1] %>% names()
  infectious_states <- c(state_names[grepl("^PA_*", state_names)],
                         state_names[grepl("^PY_*", state_names)],
                         state_names[grepl("^IA_*", state_names)],
                         state_names[grepl("^IY_*", state_names)])
  
  prev_est <- new_covars %>% 
    select(date, day, future) %>% 
    as_tibble() %>% 
    bind_cols(apply(smoothed_states[infectious_states,,], MARGIN = c(2,3), FUN = sum)  %>% 
                as_tibble()) %>% 
    filter(!future) %>% 
    tail(1) %>% 
    select(V1:V500) %>% 
    t() %>% 
    as.numeric()
  
  # browser()
  cum_incidence <- new_covars %>% 
    select(date, day, future) %>% 
    as_tibble() %>% 
    bind_cols(get_smoothed_ci(smoothed_states['NI',,])) %>% 
    filter(future) %>% 
    head(1) %>% 
    select(cum_inc_mean = mean,
           cum_inc_lo = lo,
           cum_inc_hi = hi)
  
  

  # Calculate the capacity exceed status ------------------------------------
  capacity <- dshs_df %>% 
    filter(tsa_name_wrong == current_tsa)
  
  ## Look at hospital exceeding
  hdf <- new_covars %>% 
    as_tibble() %>% 
    bind_cols(smoothed_states['total_H',,] %>% as_tibble()) %>% 
    filter(future) %>% 
    slice(21) %>% 
    select(day, V1:V500) %>% 
    gather(key, value, V1:V500)
  
  if(nrow(hdf %>% 
          filter(value > capacity$hospital_capacity)) == 0 ){
    hospital_3week_exceed_prob <- 0
  } else {
    hospital_3week_exceed_prob <- (hdf %>% 
                                     filter(value > capacity$hospital_capacity) %>% 
                                     distinct(key) %>% 
                                     nrow()) / 500
  }
  
  idf <- new_covars %>% 
    as_tibble() %>% 
    bind_cols(smoothed_states['total_H',,] %>% as_tibble()) %>% 
    filter(future) %>% 
    slice(21) %>% 
    select(day, V1:V500) %>% 
    gather(key, value, V1:V500) %>% 
    mutate(value = value * capacity$icu_ratio) 
  
  if(nrow(idf %>% 
          filter(value > capacity$icu_capacity)) == 0 ){
    icu_3week_exceed_prob <- 0
  } else {
    icu_3week_exceed_prob <- (idf %>% 
                                filter(value > capacity$icu_capacity) %>% 
                                distinct(key) %>% 
                                nrow()) / 500
  }
  
  
  tibble(
    rt_mean = mean(r0_vec),
    rt_median  = median(r0_vec),
    rt_lo = quantile(r0_vec, 0.025,na.rm=T) %>% unname(),
    rt_hi = quantile(r0_vec, 0.975,na.rm=T) %>% unname(),
    prob_epidemic_decline = sum(r0_vec<1,na.rm=T) / length(r0_vec),
    prev_mean = mean(prev_est),
    prev_lo = quantile(prev_est, 0.025,na.rm=T) %>% unname(),
    prev_hi= quantile(prev_est, 0.975,na.rm=T) %>% unname(),
    hosp_prob_exceed = hospital_3week_exceed_prob,
    hosp_median = median(hdf$value, na.rm=T),
    hosp_lb = quantile(hdf$value, 0.025) %>% unname(),
    hosp_ub = quantile(hdf$value, 0.975) %>% unname(),
    icu_prob_exceed = icu_3week_exceed_prob,
    icu_median = median(idf$value, na.rm=T),
    icu_lb = quantile(idf$value, 0.025, na.rm=T) %>% unname(),
    icu_ub = quantile(idf$value, 0.975, na.rm=T) %>% unname()
  ) %>% 
    bind_cols(cum_incidence)
}

get_statewide_summary <- function(smoothed_states, new_covars, dshs_df, current_tsa){
  
  
  state_names <- smoothed_states[,1,1] %>% names()
  infectious_states <- c(state_names[grepl("^PA_*", state_names)],
                         state_names[grepl("^PY_*", state_names)],
                         state_names[grepl("^IA_*", state_names)],
                         state_names[grepl("^IY_*", state_names)])
  
  prev_est <- new_covars %>% 
    select(date, day, future) %>% 
    as_tibble() %>% 
    bind_cols(apply(smoothed_states[infectious_states,,], MARGIN = c(2,3), FUN = sum)  %>% 
                as_tibble()) %>% 
    filter(!future) %>% 
    tail(1) %>% 
    select(V1:V500) %>% 
    t() %>% 
    as.numeric()
  
  # browser()
  cum_incidence <- new_covars %>% 
    select(date, day, future) %>% 
    as_tibble() %>% 
    bind_cols(get_smoothed_ci(smoothed_states['NI',,])) %>% 
    filter(future) %>% 
    head(1) %>% 
    select(cum_inc_mean = mean,
           cum_inc_lo = lo,
           cum_inc_hi = hi)
  
  
  first_new_day <- new_covars %>% 
    filter(future) %>% 
    slice(2) %>% 
    pull(day)
  # new_covars %>%
  #   mutate(week_number = case_when(day < first_new_day & day >= first_new_day-7 ~ "week1",
  #                                  day < first_new_day+7 & day >= first_new_day ~ "week2",
  #                                  T ~ NA_character_)) %>% 
  #   as_tibble() %>%  
  #   bind_cols(smoothed_states['NI',,] %>% as_tibble()) %>% 
  #   mutate_at(vars(V1:V500), function(x){c(0, diff(x))}) %>% 
  #   filter(future) %>% 
  #   slice(-1) %>% 
  #   filter(day <= min(day) + 14)  %>% 
  #   select(date, V1:V500) %>% 
  #   gather(key, value, V1:V500) %>% 
  #   ggplot(aes(date, value, group = key)) + geom_line(alpha = .1)
  # browser()
  prob_increasing <- new_covars %>%
    mutate(week_number = case_when(day < first_new_day & day >= first_new_day-7 ~ "week1",
                                   day < first_new_day+7 & day >= first_new_day ~ "week2",
                                   T ~ NA_character_)) %>% 
    as_tibble() %>%  
    bind_cols(smoothed_states['NI',,] %>% as_tibble()) %>% 
    mutate_at(vars(V1:V500), function(x){c(0, diff(x))}) %>% 
    filter(!is.na(week_number)) %>% 
    select(week_number:V500) %>% 
    group_by(week_number) %>% 
    summarize_all(sum) %>% 
    t() %>% 
    as_tibble() %>% 
    slice(-1) %>% 
    rename(week1 = V1,
           week2 = V2) %>% 
    summarize(prob_increasing = sum(week2>week1)/n()) %>% pull(prob_increasing)
  
  capacity <- dshs_df %>% 
    filter(tsa_letter == "Z")
  
  hdf <- new_covars %>% 
    as_tibble() %>% 
    bind_cols(smoothed_states['total_H',,] %>% as_tibble()) %>% 
    filter(future) %>% 
    slice(21) %>% 
    select(day, V1:V500) %>% 
    gather(key, value, V1:V500)
  
  if(nrow(hdf %>% 
          filter(value > capacity$hospital_capacity)) == 0 ){
    hospital_3week_exceed_prob <- 0
  } else {
    hospital_3week_exceed_prob <- (hdf %>% 
                                     filter(value > capacity$hospital_capacity) %>% 
                                     distinct(key) %>% 
                                     nrow()) / 500
  }
  
  idf <- new_covars %>% 
    as_tibble() %>% 
    bind_cols(smoothed_states['total_H',,] %>% as_tibble()) %>% 
    filter(future) %>% 
    slice(21) %>% 
    select(day, V1:V500) %>% 
    gather(key, value, V1:V500) %>% 
    mutate(value = value * capacity$icu_ratio) 
  
  if(nrow(idf %>% 
          filter(value > capacity$icu_capacity)) == 0 ){
    icu_3week_exceed_prob <- 0
  } else {
    icu_3week_exceed_prob <- (idf %>% 
                                filter(value > capacity$icu_capacity) %>% 
                                distinct(key) %>% 
                                nrow()) / 500
  }
  
  tibble(
    rt_mean = NA,
    rt_median  = NA,
    rt_lo = NA,
    rt_hi = NA,
    prob_epidemic_decline = 1- prob_increasing,
    prev_mean = mean(prev_est),
    prev_lo = quantile(prev_est, 0.025,na.rm=T) %>% unname(),
    prev_hi= quantile(prev_est, 0.975,na.rm=T) %>% unname(),
    hosp_prob_exceed = hospital_3week_exceed_prob,
    hosp_median = median(hdf$value, na.rm=T),
    hosp_lb = quantile(hdf$value, 0.025) %>% unname(),
    hosp_ub = quantile(hdf$value, 0.975) %>% unname(),
    icu_prob_exceed = icu_3week_exceed_prob,
    icu_median = median(idf$value, na.rm=T),
    icu_lb = quantile(idf$value, 0.025, na.rm=T) %>% unname(),
    icu_ub = quantile(idf$value, 0.975, na.rm=T) %>% unname()
  ) %>% 
    bind_cols(cum_incidence)
}

get_statewide_df <- function(path, dshs_df, example_tsa) {
  load(example_tsa)
  load(path)
  # browser()
  bind_rows(get_variable_tibble("Rt", smoothed_states, new_covars) %>% 
              mutate(mean = NA,
                     median = NA,
                     lo = NA,
                     hi = NA,
                     min = NA,
                     max = NA,
                     traj1 = NA,
                     traj2 = NA,
                     traj3 = NA,
                     traj4 = NA,
                     traj5 = NA,
                     traj6 = NA,
                     traj7 = NA,
                     traj8 = NA,
                     traj9 = NA,
                     traj10 = NA),
            get_variable_tibble("total_new_H", smoothed_states, new_covars),
            get_variable_tibble("total_H", smoothed_states, new_covars),
            get_variable_tibble("total_H", smoothed_states, new_covars) %>% 
              mutate(state = "icu")) %>% 
    mutate(tsa = "Statewide") %>% 
    select(tsa, date:traj10) %>% 
    nest(data = c(date, day, future, state, mean, median, lo, hi, min, max, traj1, 
                  traj2, traj3, traj4, traj5, traj6, traj7, traj8, traj9, traj10)) %>% 
    bind_cols(get_statewide_summary(smoothed_states, new_covars, dshs_df, current_tsa = "Statewide"))
}

get_tsa_df <- function(path, dshs_df, example_tsa = 'data/results/res-10.8-Abilene.Rda'){
  if(grepl("Texas", path)) {
    get_statewide_df(path, dshs_df, example_tsa)
  } else {
    load(path)
    ## First rearrange to get non-duplicated trajectories in first columns
    # i = duplicated( smoothed_states["total_H",dim(covars)[1],])
    # smoothed_states = smoothed_states[,,c( which(!i), which(i))]
    # browser()
    bind_rows(get_variable_tibble("Rt", smoothed_states, new_covars),
              get_variable_tibble("total_new_H", smoothed_states, new_covars),
              get_variable_tibble("total_H", smoothed_states, new_covars),
              get_variable_tibble("total_H", smoothed_states, new_covars) %>% 
                mutate(state = "icu")) %>% 
      mutate(tsa = TSA) %>% 
      select(tsa, date:traj10) %>% 
      nest(data = c(date, day, future, state, mean, median, lo, hi, min, max, traj1, 
                    traj2, traj3, traj4, traj5, traj6, traj7, traj8, traj9, traj10)) %>% 
      bind_cols(get_summary_stats(smoothed_states, new_covars, dshs_df, current_tsa = TSA)) 
  }
}


plot_tsa_variable <- function(varname, 
                              varlabel, 
                              tsa_df, 
                              dshs_df,
                              retrospective_days = 60,
                              days_to_project = 21){
  
  max_day <- tsa_df %>% 
    filter(!future) %>% 
    pull(day) %>% 
    max()  
  
  # tsa_df
  # browser()
  if(varname == "Rt") {
    # browser()
    days_to_project <- 7
    tsa_df <- tsa_df %>% filter(tsa != "Statewide")
  }
  future_color <- "#dba800"
  plotting_df <- tsa_df %>% 
    filter(state == varname) %>% 
    filter(day <= max_day+days_to_project, day >= max_day-retrospective_days)
  # browser()
  plotting_df %>% 
    ggplot(aes(date, median, color = future, fill = future)) + 
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha=.2, color = NA) +
    facet_wrap(~tsa_plot_label, scales = "free_y", nrow = 6) +
    background_grid(major = "xy", minor="y") +
    theme(strip.background = element_blank()) +
    scale_color_manual(values = c("grey", "black"), guide = F) +
    scale_fill_manual(values = c("grey", future_color), guide=F) +
    # scale_linetype_manual(values = c(1, 3), guide=F) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.y = element_text(size = 20)) +
    labs(x = "", y = varlabel) -> tsa_plot
  
  if(varname == "total_H"){
    # browser()
    max_date <- unique(plotting_df$date[which(plotting_df$day == (max_day))])
    tsa_plot <- tsa_plot +
      geom_hline(data = dshs_df %>%
                   group_by(tsa_plot_label) %>%
                   filter(date == max(date)) %>%
                   filter(tsa!= "Z") %>% 
                   ungroup(), aes(yintercept = total_beds_available+total_laboratory_confirmed), lty = 2) +
      geom_point(data = dshs_df %>% 
                   filter(date <= max_date+days(days_to_project), date >= max_date-days(retrospective_days)), 
                 aes(date, total_laboratory_confirmed), size = .5, inherit.aes=FALSE)
  }
  if(varname == "icu"){
    # browser()
    max_date <- unique(plotting_df$date[which(plotting_df$day == (max_day))])
    tsa_plot <- tsa_plot +
      geom_hline(data = dshs_df %>%
                   group_by(tsa_plot_label) %>%
                   filter(date == max(date)) %>%
                   filter(tsa!= "Z") %>% 
                   ungroup() , aes(yintercept = icu_beds_available+lab_con_icu), lty = 2) +
      geom_point(data = dshs_df %>% 
                   filter(date <= max_date+days(days_to_project), date >= max_date-days(retrospective_days)), 
                 aes(date, lab_con_icu), size = .5, inherit.aes=FALSE)
  }
  if(varname != "Rt"){
    tsa_plot <- tsa_plot + 
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj1),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj2),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj3),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj4),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj5),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj6),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj7),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj8),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj9),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plotting_df %>% 
                  filter(future), aes(x = date, y = traj10),
                inherit.aes = FALSE, color = "grey") +
      geom_line() 
  }
  if(varname == "Rt") {
    tsa_plot = tsa_plot + geom_hline(yintercept = 1, lty =2)
  }
  
  
  file_path <- paste0("produced_figures/tsa_proj-", Sys.Date())
  if(!dir.exists(file_path)){
    dir.create(file_path)
  }
  save_plot(paste0(file_path, "/", varname, ".png"), tsa_plot, base_height = 12.5, base_asp = 0.9)
}


get_individual_fig <- function(plot_df, 
                               plot_label,
                               varname, 
                               varlabel,            
                               dshs_df,
                               retrospective_days,
                               days_to_project){
  # browser()
  
  max_day <- plot_df %>% 
    filter(!future) %>% 
    pull(day) %>% 
    max()  
  
  future_color <- "#dba800"
  plot_df %>% 
    ggplot(aes(date, median, color = future, fill = future)) + 
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha=.2, color = NA) +
    # facet_wrap(~tsa_plot_label, scales = "free_y", nrow = 6) +
    background_grid(major = "xy", minor="y") +
    theme(strip.background = element_blank()) +
    scale_color_manual(values = c("grey", "black"), guide = F) +
    scale_fill_manual(values = c("grey", future_color), guide=F) +
    # scale_linetype_manual(values = c(1, 3), guide=F) +
    geom_line() +
    theme(#axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.y = element_text(size = 16),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "", y = varlabel, title = plot_label) -> tsa_plot
  
  dshs_df <- dshs_df %>% unnest(cols = "hosp_ts")
  
  if(varname != "Rt"){
    tsa_plot <- tsa_plot + 
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj1),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj2),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj3),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj4),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj5),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj6),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj7),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj8),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj9),
                inherit.aes = FALSE, color = "grey") +
      geom_line(data = plot_df %>% 
                  filter(future), aes(x = date, y = traj10),
                inherit.aes = FALSE, color = "grey") +
      geom_line() 
  }
  if(varname == "total_H"){
    # browser()
    max_date <- unique(plot_df$date[which(plot_df$day == (max_day))])
    
    # browser()
    
    tsa_plot <- tsa_plot +
      geom_point(data = dshs_df %>% 
                   filter(date <= max_date+days(days_to_project), 
                          date >= max_date-days(retrospective_days),
                          tsa_plot_label == plot_label), 
                 aes(date, total_hospitalized), size = .5, inherit.aes=FALSE)
      
    if(plot_label != "Statewide"){
      capacity <- dshs_df %>%
        filter(tsa_plot_label == plot_label) %>%
        filter(date == max(date)) %>% 
        pull(hospital_capacity)
      max_data_point <- max(dshs_df %>% 
                               filter(date <= max_date+days(days_to_project), 
                                      date >= max_date-days(retrospective_days),
                                      tsa_plot_label == plot_label) %>% 
                               pull(total_hospitalized))
      if(max_data_point > capacity*1.25){
        tsa_plot <- tsa_plot + 
          geom_hline(yintercept = capacity, lty = 2) +
          coord_cartesian(ylim = c(0, max_data_point*1.25))     
      } else{
        tsa_plot <- tsa_plot + 
          geom_hline(yintercept = capacity, lty = 2) +
          coord_cartesian(ylim = c(0, capacity*1.25)) 
      }
    }
    
    
  }
  if(varname == "icu"){
    # browser()
    max_date <- unique(plot_df$date[which(plot_df$day == (max_day))])
    # print(max_date + days(days_to_project))
    
  
    tsa_plot <- tsa_plot +
      geom_point(data = dshs_df %>% 
                   filter(date <= max_date+days(days_to_project), 
                          date >= max_date-days(retrospective_days),
                          tsa_plot_label == plot_label), 
                 aes(date, total_icu), size = .5, inherit.aes=FALSE)
    
    if(plot_label != "Statewide"){
      capacity <- dshs_df %>%
        filter(tsa_plot_label == plot_label) %>%
        filter(date == max(date)) %>% 
        pull(icu_capacity)
      
      max_data_point <- max(dshs_df %>% 
                              filter(date <= max_date+days(days_to_project), 
                                     date >= max_date-days(retrospective_days),
                                     tsa_plot_label == plot_label) %>% 
                              pull(total_icu), na.rm=T)
      
      if(max_data_point > capacity*1.25){
        tsa_plot <- tsa_plot + 
          geom_hline(yintercept = capacity, lty = 2) +
          coord_cartesian(ylim = c(0, max_data_point*1.25))     
      } else{
        tsa_plot <- tsa_plot + 
          geom_hline(yintercept = capacity, lty = 2) +
          coord_cartesian(ylim = c(0, capacity*1.25)) 
      }
    }
  }
  
  if(varname == "Rt") {
    tsa_plot = tsa_plot + geom_hline(yintercept = 1, lty =2)
  }
  
  tsa_plot + expand_limits(y=c(0, NA))
}


plot_individual_tsa_variable <- function(varname, 
                                varlabel, 
                                tsa_df, 
                                dshs_df,
                                retrospective_days = 60,
                                days_to_project = 21){

  # tsa_df
  # browser()
  if(varname == "Rt") {
    # browser()
    days_to_project <- 7
    # retrospective_days = 10000
    tsa_df <- tsa_df %>% filter(tsa != "Statewide")
  }
  
  
  # browser()
  max_day <- tsa_df %>% 
    filter(!future) %>% 
    pull(day) %>% 
    max()  
  
  # browser()
  plotting_df <- tsa_df %>% 
    filter(state == varname) %>% 
    filter(day <= max_day+days_to_project, day >= max_day-retrospective_days)
  
  plot_df <- plotting_df %>% 
    nest(-tsa_letter, -tsa_name, -tsa_plot_label) %>% 
    arrange(tsa_plot_label) %>% 
    mutate(fig = map2(data, tsa_plot_label, get_individual_fig, 
                      varname = varname, 
                      varlabel = varlabel,            
                      dshs_df = dshs_df,
                      retrospective_days = retrospective_days,
                      days_to_project = days_to_project))
  
  file_path <- paste0("produced_figures/tsa_proj-", Sys.Date())
  if(!dir.exists(file_path)){
    dir.create(file_path)
  }
  
  pdf(paste0(file_path, "/", varname, "-individual.pdf"), onefile = TRUE, height = 5, width = 7)
  for(i in 1:nrow(plot_df)){
    print(plot_df$fig[[i]])
  }
  dev.off()
  
  change_plot_to_facet <- function(x){
    x + theme(plot.title = element_text(size = 10),
              axis.title.y = element_text(size = 6),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              plot.margin = unit(c(10, 10, 0, 10), "pt")) +
      labs(y = NULL)
  }

  y_label <- ggdraw() +
    draw_label(
      varlabel,
      # fontface = 'regular',
      x = 0.5,
      angle = 90,
      hjust = 0.5
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 0)
    )

  plot_df %>%
    mutate(fig2 = map(fig, change_plot_to_facet)) %>%
    pull(fig2) %>%
    plot_grid(plotlist = ., nrow = 6) %>%
    plot_grid(y_label, ., rel_widths = c(1, 20)) %>%
    save_plot(filename = paste0(file_path, "/", varname, ".png"), plot = ., base_height = 12, base_asp = 0.9)
}
