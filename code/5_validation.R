##### Facebook migration project
##### 4. Run some validation tests


# 0. Packages and functions -------------------------------------------------------------

library(tidyverse)
library(here)
library(boot)
library(narcan)
library(geofacet)
library(zoo)
source(here("code","plottrace.R"))



# 1. Read in ACS data -----------------------------------------------------

# specify country of origin
origin_country <- "India"

acs_prop_age_17 <- readRDS(here("data", "acs_prop_age_17.RDS"))
acs_prop_age_17 <- acs_prop_age_17 %>% filter(origin == origin_country) # only country of interest
total_pops_17 <- readRDS(here("data", "acs_total_pop_17.RDS"))


# 2. Compare with model ---------------------------------------------------

p_all <- read_csv(file = here("output", paste0(origin_country, "_results_ci.csv")))

res_mod <- p_all %>% 
  filter(year==2017) %>%  
  select(age_group:age_group_cat, state) %>% 
  left_join(acs_prop_age_17)

res_mod <- as_tibble(res_mod)

## RMSE by age
rmse_mod_age <- res_mod %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (median - prop_mig)^2) %>% 
  group_by(age_group) %>% 
  summarise(combined = sqrt(sum(diff_sq, na.rm = T)/n()))

## RMSE by state
rmse_mod_state <- res_mod %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (median - prop_mig)^2) %>% 
  group_by(state) %>% 
  summarise(combined = sqrt(sum(diff_sq, na.rm = T)/n())) %>% 
  arrange(-combined)

res_mod %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (median - prop_mig)^2) %>% 
  summarise(combined = sqrt(sum(diff_sq, na.rm = T)/n())) %>% 
  arrange(-combined)


# 3. Compare with just Facebook data ---------------------------------------------------------

fb_prop_age_new_mex <- read_rds("data/fb_for_model.RDS") 

res_fb <- fb_prop_age_new_mex %>% 
  filter(year==2017) %>% 
  group_by(age_group, state) %>% 
  summarise(estimated_prop = mean(estimated_prop)) %>% 
  left_join(acs_prop_age_17) %>% 
  select(-year, -bpl, -df, -se_logit) 

## RMSE by age
rmse_fb_age <- res_fb %>% 
  mutate(diff_sq = (estimated_prop - prop_mig)^2) %>% 
  group_by(age_group) %>% 
  summarise(fb = sqrt(sum(diff_sq, na.rm = T)/n()))

## RMSE by state
rmse_fb_state <- res_fb %>% 
  mutate(diff_sq = (estimated_prop - prop_mig)^2) %>% 
  group_by(state) %>% 
  summarise(fb = sqrt(sum(diff_sq, na.rm = T)/n())) %>% 
  arrange(-fb)

res_fb %>% 
  ungroup() %>% 
  mutate(diff_sq = (estimated_prop - prop_mig)^2) %>% 
  summarise(fb = sqrt(sum(diff_sq, na.rm = T)/n())) %>% 
  arrange(-fb)




# 4. Compare with no FB data ----------------------------------------------

p_all <- read_csv(file = here("output", paste0(origin_country, "_results_nf_ci.csv")))

res_nf <- p_all %>% 
  filter(year==2017) %>% 
  select(age_group:age_group_cat, state) %>% 
  left_join(acs_prop_age_17)

res_nf <- as_tibble(res_nf)

## RMSE by age
rmse_nf_age <- res_nf %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (median - prop_mig)^2) %>% 
  group_by(age_group) %>% 
  summarise(acs = sqrt(sum(diff_sq, na.rm = T)/n()))

## RMSE by state
rmse_nf_state <- res_nf %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (median - prop_mig)^2) %>% 
  group_by(state) %>% 
  summarise(acs = sqrt(sum(diff_sq, na.rm = T)/n())) %>% 
  arrange(-acs)

res_nf %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (median - prop_mig)^2) %>% 
  summarise(acs = sqrt(sum(diff_sq, na.rm = T)/n())) %>% 
  arrange(-acs)

# 5. Compare with moving average ---------------------------------------------

res_ma <- acs_prop_age %>% 
  bind_rows(acs_prop_age_17) %>% 
  group_by(age_group, state) %>% 
  mutate(ma3 = rollmean(x = lag(prop_mig), 3, align = "right", fill = NA))

## RMSE by age
rmse_ma_age <- res_ma %>% 
  filter(year==2017) %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (ma3 - prop_mig)^2) %>% 
  group_by(age_group) %>% 
  summarise(ma = sqrt(sum(diff_sq, na.rm = T)/n()))

## RMSE by state
rmse_ma_state <- res_ma %>% 
  filter(year==2017) %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (ma3 - prop_mig)^2) %>% 
  group_by(state) %>% 
  summarise(ma = sqrt(sum(diff_sq, na.rm = T)/n())) %>% 
  arrange(-ma)

res_ma %>% 
  ungroup() %>% 
  filter(year==2017) %>% 
  select(-df, -bpl, -se_logit, -year) %>% 
  mutate(diff_sq = (ma3 - prop_mig)^2) %>% 
  summarise(ma = sqrt(sum(diff_sq, na.rm = T)/n())) %>% 
  arrange(-ma)


# 6. Combine -----------------------------------------------------------------


rmse_age <- rmse_mod_age %>% 
  left_join(rmse_fb_age) %>% 
  left_join(rmse_nf_age) %>% 
  left_join(rmse_ma_age)

rmse_state <- rmse_mod_state %>% 
  left_join(rmse_fb_state) %>% 
  left_join(rmse_nf_state) %>% 
  left_join(rmse_ma_state)



rmse_age <- rmse_age %>% 
  gather(model, value, -age_group) 


# 7. Plot -----------------------------------------------------------------



rmse_age %>% 
  mutate(model = factor(rmse_age$model,levels = c("fb", "ma", "acs", "combined"))) %>% 
  ggplot(aes(age_group, value, fill = model)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  #ggtitle("RMSE by model and age group") + 
  ylab("RMSE") + xlab("age") +
  theme_bw(base_size = 14) + scale_fill_viridis_d()
ggsave(here("fig", paste0(origin_country,"_rsme_age.pdf")), width = 8, height = 6)


rmse_state <- rmse_state %>% 
  gather(model, value, -state) 

rmse_state %>% 
  mutate(model = factor(rmse_state$model,levels = c("fb", "ma", "acs", "combined"))) %>% 
  ggplot(aes(model, y = value, fill = model)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  #ggtitle("RMSE by model") + 
  ylab("RMSE") + xlab("model") +
  facet_geo(~state, scale = "free_y") + 
  theme_bw(base_size = 14) + scale_fill_viridis_d() +
ggsave(here("fig", paste0(origin_country,"_rsme_state.pdf")), width = 17, height = 8)




