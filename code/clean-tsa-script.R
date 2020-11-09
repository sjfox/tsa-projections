## https://texas-2036.github.io/covid-data/bed_and_vents.csv
library(tidyverse)
library(cowplot)
library(lubridate)
library(gridExtra)
library(grid)

theme_set(theme_cowplot())

TSA.res.path = "../RDA"
run.name = "10.8"

source("code/tsa-projection-fxns.R")


dshs_df <- get_capacities_and_usage()

vec_files <- list.files(path = TSA.res.path, full.names = T,pattern=run.name)
#vec_files <- vec_files[grepl("res-10.8", vec_files)]
# vec_files <- vec_files[which(vec_files!= "data/results/res-mob-dt-Texas.Rda")]
tsa_proj <- vec_files %>%
  purrr::map( get_tsa_df, dshs_df = dshs_df, example_tsa = vec_files[1]) %>% 
  bind_rows()


tsa_df <- fix_icu_numbers(tsa_proj, dshs_df) %>% 
  # filter(tsa != "Lower Rio Grande Valley") %>% 
  left_join(dshs_df, by = c("tsa" = "tsa_name_wrong"))


#pdf( file="file1.pdf",height = 12, width = 12*0.9)
plot_individual_tsa_variable("Rt", expression(paste("Effective reproduction number ", italic("R(t)"))), tsa_df, dshs_df, retrospective_days = 120) %>% print()
plot_individual_tsa_variable("total_new_H","COVID-19 Hospital Admissions", tsa_df, dshs_df, retrospective_days = 120) %>% print()
plot_individual_tsa_variable("total_H","COVID-19 Hospitalizations", tsa_df, dshs_df, retrospective_days = 120) %>% print()
plot_individual_tsa_variable("icu","COVID-19 ICU Patients", tsa_df, dshs_df, retrospective_days = 120) %>% print()
#dev.off()

#cat("done grid plots\n")

#pdf("file2.pdf",height = 12, width = 12*0.9)


tsa_proj %>% 
  # filter(tsa != "Lower Rio Grande Valley") %>% 
  left_join(dshs_df, by = c("tsa" = "tsa_name_wrong")) %>% 
  mutate(prev_mean = prev_mean/population*1000,
         prev_lo = prev_lo/population*1000,
         prev_hi = prev_hi/population*1000,
         cum_inc_mean = scales::percent(cum_inc_mean/population, accuracy = 0.1),
         cum_inc_lo = scales::percent(cum_inc_lo/population, accuracy = 0.1),
         cum_inc_hi = scales::percent(cum_inc_hi/population, accuracy = 0.1)
  ) %>% 
  mutate(rt = paste(round(rt_mean, 2), " (", round(rt_lo, 2), "-", round(rt_hi, 2), ")", sep = ""),
         prev = paste(round(prev_mean, 1), " (", round(prev_lo, 1), "-", round(prev_hi, 1), ")", sep = ""),
         prob_epidemic_increase = scales::percent(1-prob_epidemic_decline, accuracy = 1),
         # cum_i = paste(round(cum_inc_mean, 1), " (", round(cum_inc_lo, 1), "-", round(cum_inc_hi, 1), ")", sep = "")
         cum_i = paste(cum_inc_mean, " (", cum_inc_lo, "-", cum_inc_hi, ")", sep = "")
  ) %>% 
  select(tsa_letter, tsa_name, rt, prob_increase = prob_epidemic_increase, prev, cum_i) %>% 
  arrange(tsa_letter) -> x


  write_csv(x,paste0("produced_figures/tsa_proj-", Sys.Date(), "/tsa-table1.csv"))

mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.9)),
    colhead = list(fg_params=list(cex = 0.9)),
    rowhead = list(fg_params=list(cex = 0.9))
)

myt <- gridExtra::tableGrob( x, theme = mytheme)
grid.newpage()
grid.draw(myt)


# Plot table 2 ------------------------------------------------------------

tsa_proj %>% 
  # filter(tsa != "Lower Rio Grande Valley") %>% 
  left_join(dshs_df, by = c("tsa" = "tsa_name_wrong")) %>% 
  mutate(
    hosp_prob = scales::percent(hosp_prob_exceed, accuracy = 1),
    hosp_state = paste(round(hosp_median), " (", round(hosp_lb), "-", round(hosp_ub), ")", sep = ""),
    icu_prob = scales::percent(icu_prob_exceed, accuracy = 1),
    icu_state = paste(round(icu_median), " (", round(icu_lb), "-", round(icu_ub), ")", sep = "")
  ) %>% 
  select(tsa_letter, tsa_name, hosp_prob, hosp_state, icu_prob, icu_state) %>% 
  arrange(tsa_letter) -> y

  write_csv(y,paste0("produced_figures/tsa_proj-", Sys.Date(), "/tsa-table2.csv"))



mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.9)),
    colhead = list(fg_params=list(cex = 0.9)),
    rowhead = list(fg_params=list(cex = 0.9))
)

myt <- gridExtra::tableGrob( y, theme = mytheme)
grid.newpage()
grid.draw(myt)


# tsa_proj %>% 
#   left_join(dshs_df %>% 
#               distinct(tsa, tsa_label) %>% 
#               select(tsa_city = tsa_label,
#                      tsa_letter = tsa), by = c("tsa" = "tsa_city")) %>% 
#   left_join(tsa_pop_stats, by = c("tsa_letter" = "TSA")) %>% 
#   summarize(sum(cum_inc_mean))
# 
# c(0.15, .32, .125, .25, .6, .1, .2, .28, .6, .11, .2, .15, .15, .3, .4, .17) %>% 
#   mean()


#dev.off()

