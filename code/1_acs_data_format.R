##### Facebook migration project
##### 1. Format and save ACS data

### Note: These data were downloaded from IPUMS USA. 
### ACS 1 year with the following variables: YEAR; STATEFIP; PERWT; SEX; AGE; BPL 
### for all years 2001-2016


# 0. Packages -------------------------------------------------------------

library(tidyverse)
library(boot)
library(narcan) # this is a working package. Used to get fips codes/names dictionary. https://github.com/mkiang/narcan
library(here)


# 1. Read in data  ---------------------------------


### Note: These data were downloaded from IPUMS USA. 
### ACS 1 year with the following variables: YEAR; STATEFIP; PERWT; SEX; AGE; BPL 
### for all years 2001-2016

acs_file <- "path/to/your/file"
dc <- read_csv(acs_file)

# for this project, just keep males. 
# starting age group is 15-19.

dc <- dc %>% 
  filter(SEX==1, 
         AGE>14) %>% 
  mutate(age_group = as.numeric(as.character(cut(AGE, 
                                                 breaks = c(seq(15, 80, by = 5), Inf),
                                                 labels = seq(15, 80, by = 5), right = FALSE
  ))))


# get proportions of migrants by age group
# filter so that birthplace only refers to international locations

acs_prop_age <- dc %>% 
  group_by(age_group, YEAR, STATEFIP, BPL) %>% 
  summarise(no_mig = sum(PERWT)) %>% 
  group_by(age_group, YEAR, STATEFIP) %>% 
  mutate(prop_mig = no_mig/(sum(no_mig)), pop = sum(no_mig)) %>% 
  rename(bpl = BPL, year = YEAR) %>%
  filter(bpl>99, age_group < 60) %>% 
  ungroup()

## make statefip 1-9 be 01-09
acs_prop_age$state_fip <- ifelse(acs_prop_age$STATEFIP<10, paste0("0", acs_prop_age$STATEFIP), as.character(acs_prop_age$STATEFIP))

# read in US state codes to get the names
rep_pattern <- narcan::st_fips_map$name
names(rep_pattern) <- narcan::st_fips_map$fips
acs_prop_age$state <- stringr::str_replace_all(acs_prop_age$state_fip, pattern = rep_pattern)

# read in birthplace codes and get names
bpl_names <- read_csv(here("data", "origin_names.csv"))

acs_prop_age <- acs_prop_age %>% 
  left_join(bpl_names %>% 
              rename(bpl = bpl_no))

# 2. Calculate standard errors -----------------------------------------

# see: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/accuracy/ for dfs and formulas
design_factors <- tibble(year = 2001:2016, df = c(rep(2.9, 4), rep(2.2, 4), rep(2, 3), rep(1.9, 5)))

# mean and variance 
# http://www.biostat.jhsph.edu/~tlouis/bio752-753/651-2/files/deltaMethod.pdf

acs_prop_age <- acs_prop_age %>% 
  left_join(design_factors) %>% 
  mutate(se = case_when(
    year < 2005 ~ 1.2*df*sqrt(142/pop*prop_mig*(1-prop_mig)), 
    year >= 2005 ~ df*sqrt(99/pop*prop_mig*(1-prop_mig)),
    TRUE ~ 0),
    se_log = se/prop_mig,
    se_logit = se/(prop_mig*(1-prop_mig)))


# 3. Calculate total populations ------------------------------------------

total_pops <- dc %>% 
  group_by(age_group, YEAR, STATEFIP, BPL) %>% 
  summarise(no_mig = sum(PERWT)) %>% 
  group_by(age_group, YEAR, STATEFIP) %>% 
  mutate(prop_mig = no_mig/(sum(no_mig)), pop = sum(no_mig)) %>% 
  rename(bpl = BPL, year = YEAR) %>%
  filter(age_group < 60) %>% 
  arrange(year, STATEFIP, age_group, bpl) %>% 
  group_by(year, STATEFIP, age_group) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(age_group, year, STATEFIP, pop)

total_pops$state_fip <- ifelse(total_pops$STATEFIP<10, paste0("0", total_pops$STATEFIP), as.character(total_pops$STATEFIP))
total_pops$state <- stringr::str_replace_all(total_pops$state_fip, pattern = rep_pattern)


# 4. Save file for fitting Facebook model ---------------------------------

# for this we only need 2016

acs_for_fb <- acs_prop_age %>% filter(year==2016)

saveRDS(acs_for_fb, here("data", "acs_for_fb.RDS"))


# 5. Save file for combined model -----------------------------------------

saveRDS(acs_prop_age, here("data", "acs_for_model.RDS"))


# 6. Save total populations -----------------------------------------------

saveRDS(total_pops, here("data", "acs_total_pop.RDS"))


# 7. Do the same thing for the year 2017 ----------------------------------

acs_file_17 <- "path/to/your/file"

d17 <- read_csv(acs_file_17)

d17 <- d17 %>% 
  filter(SEX==1, AGE>14, AGE<60) %>% 
  mutate(age_group = as.numeric(as.character(cut(AGE, 
                                                 breaks = c(seq(15, 80, by = 5), Inf),
                                                 labels = seq(15, 80, by = 5), right = FALSE
  ))))


# get proportions of mexicans by age group

acs_prop_age_17 <- d17 %>% 
  group_by(age_group, YEAR, STATEFIP, BPL) %>% 
  summarise(no_mig = sum(PERWT)) %>% 
  group_by(age_group, YEAR, STATEFIP) %>% 
  mutate(prop_mig = no_mig/(sum(no_mig)), pop = sum(no_mig)) %>% 
  rename(bpl = BPL, year = YEAR) %>%
  filter(bpl>99, age_group < 60) %>% 
  ungroup()

## make statefip 1-9 be 01-09
acs_prop_age_17$state_fip <- ifelse(acs_prop_age_17$STATEFIP<10, paste0("0", acs_prop_age_17$STATEFIP), as.character(acs_prop_age_17$STATEFIP))

# read in codes to get the names
rep_pattern <- narcan::st_fips_map$name
names(rep_pattern) <- narcan::st_fips_map$fips
acs_prop_age_17$state <- stringr::str_replace_all(acs_prop_age_17$state_fip, pattern = rep_pattern)


## need to get name of bpl
bpl_names <- read_csv(here("data", "origin_names.csv"))

# join 
acs_prop_age_17 <- acs_prop_age_17 %>% 
  left_join(bpl_names %>% rename(bpl = bpl_no)) %>% 
  select(age_group, year,  bpl,origin, state, prop_mig, pop)


### TOTAL POPS (for results later)

total_pops_17 <- d17 %>% 
  group_by(age_group, YEAR, STATEFIP, BPL) %>% 
  summarise(no_mig = sum(PERWT)) %>% 
  group_by(age_group, YEAR, STATEFIP) %>% 
  mutate(prop_mig = no_mig/(sum(no_mig)), pop = sum(no_mig)) %>% 
  rename(bpl = BPL, year = YEAR) %>%
  filter(age_group < 60) %>% 
  arrange(year, STATEFIP, age_group, bpl) %>% 
  group_by(year, STATEFIP, age_group) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(age_group, year, STATEFIP, pop)

total_pops_17$state_fip <- ifelse(total_pops_17$STATEFIP<10, paste0("0", total_pops_17$STATEFIP), as.character(total_pops_17$STATEFIP))

# read in codes to get the names
rep_pattern <- narcan::st_fips_map$name
names(rep_pattern) <- narcan::st_fips_map$fips
total_pops_17$state <- stringr::str_replace_all(total_pops_17$state_fip, pattern = rep_pattern)


## need to get standard errors 

# see: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/accuracy/ for dfs and formulas
design_factors <- tibble(year = 2001:2017, df = c(rep(2.9, 4), rep(2.2, 4), rep(2, 3), rep(1.9, 6)))

# mean and variance 
# http://www.biostat.jhsph.edu/~tlouis/bio752-753/651-2/files/deltaMethod.pdf

acs_prop_age_17 <- acs_prop_age_17 %>% 
  left_join(design_factors) %>% 
  mutate(se = case_when(
    year < 2005 ~ 1.2*df*sqrt(142/pop*prop_mig*(1-prop_mig)), 
    year >= 2005 ~ df*sqrt(99/pop*prop_mig*(1-prop_mig)),
    TRUE ~ 0),
    se_log = se/prop_mig,
    se_logit = se/(prop_mig*(1-prop_mig)))

saveRDS(acs_prop_age_17, here("data", "acs_prop_age_17.RDS"))
saveRDS(total_pops_17, here("data", "acs_total_pop_17.RDS"))
