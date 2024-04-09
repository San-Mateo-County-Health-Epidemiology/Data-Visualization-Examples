#################################################
#
# Age adjusted deaths - denominators
# this will give you the counts per sub-population by year and has the 2000 census weights per category
#
#################################################

rm(list = ls())

library(tidyverse) 
library(writexl)
library(janitor)
library(zoo)
library(smcepi)

# load data: ------------------------------------
## dof P3 ----
dof_old <- read.csv("J:/Epi Data/DoF Population Projections/P3_Complete released on 7.14.2021/P3_Complete.csv")
dof_new <- read.csv("J:/Epi Data/DoF Population Projections/P3_Complete released on 7.19.2023/P3_Complete released on 7.19.2023.csv")

dof_new_start <- dof_new %>%
  summarize(start_year = min(year)) %>%
  pull(start_year)

dof <- dof_old %>%
  filter(year < dof_new_start) %>%
  bind_rows(dof_new)

# make tables -------------------------------------
## dof p3 2000 - 2060
dof_all <- dof %>%
  rename(age = agerc) %>%
  filter(fips == "6081") %>%
  mutate(age_cat_all = case_when(age %in% 0:9 ~ "0-9",
                                 age %in% 10:19 ~ "10-19", 
                                 age %in% 20:29 ~ "20-29",
                                 age %in% 30:39 ~ "30-39",
                                 age %in% 40:49 ~ "40-49",
                                 age %in% 50:59 ~ "50-59",
                                 age %in% 60:69 ~ "60-69",
                                 age %in% 70:79 ~ "70-79",
                                 age >= 80 ~ "80+"),
         race_cat = case_when(race7 == 1 ~ "white",
                              race7 == 2 ~ "black",
                              race7 == 3 ~ "aian",
                              race7 == 4 ~ "asian",
                              race7 == 5 ~ "pi",
                              race7 == 6 ~ "multirace",
                              race7 == 7 ~ "latino"),
         sex_cat = str_to_lower(sex)) %>%
  filter(!is.na(age_cat_all)) 

# line chart --------------------------------
by_time <- dof_all %>%
  group_by(year) %>%
  summarize(pop = sum(perwt),
            .groups = "keep") %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = pop), linewidth = 2) +
  labs(title = "San Mateo County Population: 2010-2060") + 
  scale_y_continuous(limits = c(0, 800000)) + 
  theme_smc()
by_time

by_race_time <- dof_all %>%
  group_by(year, race_cat) %>%
  summarize(pop = sum(perwt),
            .groups = "keep") %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = pop, group = race_cat, color = race_cat), linewidth = 1) +
  labs(title = "San Mateo County Population: 2010-2060") + 
  scale_y_continuous(limits = c(0, 400000)) + 
  theme_smc()
by_race_time

by_race_time_facet <- dof_all %>%
  group_by(year, race_cat) %>%
  summarize(pop = sum(perwt),
            .groups = "keep") %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = pop), linewidth = 1) +
  facet_wrap(~race_cat,
             nrow = 4) +
  labs(title = "San Mateo County Population: 2010-2060") + 
  scale_y_continuous(limits = c(0, NA)) + 
  theme_smc()
by_race_time_facet
