library(tidyverse)
library(lubridate)
library(cowplot)
theme_set(theme_cowplot())

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
    select(date, day, future, PC1) %>% 
    as_tibble() %>% 
    bind_cols(get_smoothed_ci(smoothed_states[varname,,]) %>% 
                mutate(state = varname) %>% 
                select(state, mean:max),
              spaghettis)
}

get_summary_stats <- function(smoothed_states, new_covars, capacities, current_tsa){
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
  
  capacity <- capacities %>% 
    filter(tsa_label == current_tsa)
  
  
  hdf <- new_covars %>% 
    as_tibble() %>% 
    bind_cols(smoothed_states['total_H',,] %>% as_tibble()) %>% 
    filter(future) %>% 
    slice(21) %>% 
    select(day, V1:V500) %>% 
    gather(key, value, V1:V500) %>% 
    filter(value > capacity$hospital_capacity)
  
  if(nrow(hdf) == 0 ){
    hospital_3week_exceed_prob <- 0
  } else {
    hospital_3week_exceed_prob <- (hdf %>% distinct(key) %>% nrow()) / 500
  }
  
  idf <- new_covars %>% 
    as_tibble() %>% 
    bind_cols(smoothed_states['total_H',,] %>% as_tibble()) %>% 
    filter(future) %>% 
    slice(21) %>% 
    select(day, V1:V500) %>% 
    gather(key, value, V1:V500) %>% 
    mutate(value = value * capacity$icu_ratio) %>% 
    filter(value > capacity$icu_capacity)
  
  if(nrow(idf) == 0 ){
    icu_3week_exceed_prob <- 0
  } else {
    icu_3week_exceed_prob <- (idf %>% distinct(key) %>% nrow()) / 500
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
    icu_prob_exceed = icu_3week_exceed_prob
  ) %>% 
    bind_cols(cum_incidence)
}


get_tsa_df <- function(path, capacities){
  if(grepl("Texas", path)) {
    get_statewide_df(path, capacities)
  } else {
    load(path)
    # browser()
    ## First rearrange to get non-duplicated trajectories in first columns
    # i = duplicated( smoothed_states["total_H",dim(covars)[1],])
    # smoothed_states = smoothed_states[,,c( which(!i), which(i))]
    # browser()
    bind_rows(get_variable_tibble("Rt", smoothed_states, new_covars),
              get_variable_tibble("total_new_H", smoothed_states, new_covars),
              get_variable_tibble("total_H", smoothed_states, new_covars),
              get_variable_tibble("", smoothed_states, new_covars),
              get_variable_tibble("total_H", smoothed_states, new_covars) %>% 
                mutate(state = "icu")) %>% 
      mutate(tsa = TSA) %>% 
      select(tsa, date:traj10) %>% 
      nest(data = c(date, day, future, state, mean, median, lo, hi, min, max, traj1, 
                    traj2, traj3, traj4, traj5, traj6, traj7, traj8, traj9, traj10)) %>% 
      bind_cols(get_summary_stats(smoothed_states, new_covars, capacities, current_tsa = TSA)) 
  }
}

get_tsa_b1 <- function(path){
  if(grepl("Texas", path)) {
    get_statewide_df(path, capacities)
  } else {
    load(path)
    # browser()
    ## First rearrange to get non-duplicated trajectories in first columns
    # i = duplicated( smoothed_states["total_H",dim(covars)[1],])
    # smoothed_states = smoothed_states[,,c( which(!i), which(i))]
    # browser()
    bind_rows(get_variable_tibble("b1", smoothed_states, new_covars),
              get_variable_tibble("b2", smoothed_states, new_covars),
              get_variable_tibble("Beta", smoothed_states, new_covars),
              get_variable_tibble("Z", smoothed_states, new_covars)) %>% 
      mutate(tsa = TSA)
  }
}



dshs_df <- read_csv("data/dshs-tx-2036-data.csv") %>% 
  select(date, tsa, tsa_label=location, total_beds_available, icu_beds_available, total_laboratory_confirmed, lab_con_icu) %>% 
  mutate(date = as.Date(date)) %>% 
  # mutate(tsa_label = ifelse(tsa_label == "Witchita Falls", "Wichita Falls", tsa_label)) %>% 
  # filter(tsa_label != "Midland/Odessa",
  #        tsa_label != "San Angelo") %>% 
  filter(date <= '2020-07-21') %>% 
  mutate(tsa_plot_label = paste0("(", tsa, ") ", tsa_label))


tsa_capacities <- dshs_df %>% 
  filter(date == max(date)) %>% 
  mutate(hospital_capacity = total_beds_available + total_laboratory_confirmed,
         icu_capacity = icu_beds_available + lab_con_icu,
         icu_ratio = lab_con_icu/total_laboratory_confirmed) %>% 
  select(tsa, tsa_label, tsa_plot_label, hospital_capacity, icu_capacity, icu_ratio) 


vec_files <- list.files(path = "data/results", full.names = T)
vec_files <- vec_files[vec_files!= "data/results/res-mob-dt-Texas.Rda"]

tsa_proj <- vec_files %>% 
  map(get_tsa_df, capacities = tsa_capacities) %>% 
  bind_rows() %>% 
  mutate(tsa = ifelse(tsa == "Witchita Falls", "Wichita Falls", tsa))


tsa_cases <- read_csv("data/tsa_county_mapping.csv", col_types = c("cccc")) %>%
  left_join(read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
              filter(state == "Texas", !is.na(fips)) %>% 
              group_by(county) %>% 
              filter(date == max(date)) %>% 
              ungroup() %>% 
              select(fips, cases), by = "fips") %>% 
  group_by(tsa_name) %>% 
  summarize(total_cases = sum(cases,na.rm=T))


tsa_proj %>% 
  left_join(tsa_cases, by = c("tsa" = "tsa_name")) %>% 
  mutate(reporting_rate = total_cases / cum_inc_mean) %>% 
  mutate(tsa = forcats::fct_reorder(tsa, reporting_rate)) %>% 
  ggplot(aes(tsa, reporting_rate)) + 
    geom_point() +
    coord_flip()

tsa_proj %>% 
  left_join(tsa_cases, by = c("tsa" = "tsa_name")) %>% 
  mutate(reporting_rate = total_cases / cum_inc_mean,
         reporting_rate_lo = total_cases / cum_inc_lo,
         reporting_rate_hi = total_cases / cum_inc_hi) %>% 
  mutate(tsa = forcats::fct_reorder(tsa, reporting_rate)) %>% 
  ggplot(aes(tsa, reporting_rate)) + 
    geom_point() +
    geom_errorbar(aes(ymin = reporting_rate_lo, ymax = reporting_rate_hi)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "", y = "Reporting Rate") +
    background_grid(major = "x", minor = "x")
  
tsa_pop_stats <- read_csv("data/TSAPopulation_RiskGroups.csv") %>% 
  mutate(population = `0-4` + `5-17` + `18-49` + `50-64` + `65+`) %>% 
  group_by(TSA) %>% 
  summarize(population = sum(population)) %>% 
  left_join(read_csv("data/tsa_county_mapping.csv", col_types = c("cccc")) %>% 
              distinct(tsa_id, tsa_name) %>% 
              mutate(tsa_id = str_replace(tsa_id, pattern = "[.]", "")), by = c("TSA" = "tsa_id"))

tsa_proj %>% 
  inner_join(tsa_pop_stats, by = c("tsa" = "tsa_name")) %>% 
  mutate(cum_inc_mean = cum_inc_mean/population,
         cum_inc_lo = cum_inc_lo/population,
         cum_inc_hi = cum_inc_hi/population) %>% 
  mutate(tsa = forcats::fct_reorder(tsa, cum_inc_mean)) %>% 
  ggplot(aes(tsa, cum_inc_mean)) + 
  geom_point() +
  geom_errorbar(aes(ymin = cum_inc_lo, ymax = cum_inc_hi)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Cumulative incidence") +
  background_grid(major = "x", minor = "x")


tsa_b1s <- vec_files %>% 
  map(get_tsa_b1) %>% 
  bind_rows()


tsa_b1s %>% 
  select(tsa, date, PC1, state, mean) %>% 
  spread(state, mean) %>% 
  mutate(logbeta0 = log(Beta) - b1*PC1 - Z) %>% 
  group_by(tsa) %>%
  summarize(median(logbeta0, na.rm=T))


tsa_b1s %>% 
  filter(!future, state == "b1") %>% 
  group_by(tsa) %>% 
  summarize(start = mean[1], end = -mean[n()]) 


tsa_b1s %>% 
  filter(!future, state == "b1") %>% 
  ggplot(aes(date, -mean, group = tsa)) + 
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Mobility transmission coefficient")
  # stat_smooth(se=F)
  # geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .1)

tsa_b1s %>% 
  filter(state == "b2",
         !future) %>% 
  ggplot(aes(date, 1/exp(mean), group = tsa)) + 
    geom_line() +
    labs(x = "", y = "Hospital duration") +
    coord_cartesian(ylim = c(0,15))
