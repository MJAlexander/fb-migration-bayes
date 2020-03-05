

# 1. Read in data and results ---------------------------------------------


# specify country of origin
origin_country <- "India"
acs_prop_age <- readRDS(here("data", "acs_for_model.RDS"))
acs_prop_age <- acs_prop_age %>% filter(origin == origin_country) # only country of interest
total_pops <- readRDS(here("data", "acs_total_pop.RDS"))

years <- 2001:2016
age_groups <- unique(acs_prop_age$age_group)

# read in results

p_all <- read_csv(here("output", paste0(origin_country, "_results_ci.csv")))



# 2. Plot -----------------------------------------------------------------



ggplot(p_all %>% filter(year %in% c(2008, 2018)), aes(as.numeric(as.character(age_group)), 
                                                      median, group = as.factor(year))) + 
  geom_line(aes(color = as.factor(year))) + 
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = as.factor(year)), alpha = 0.5)+
  #geom_point(aes(age_group, prop_mig, color = "ACS")) + 
  facet_geo(~state)+
  xlab("age group") + ylab("proportion of population") + 
  #ggtitle("Age distributions of Mexican migrants, 2008 and 2018")+
  theme_bw(base_size = 14) + scale_color_discrete(name = "Year")  + 
  scale_fill_brewer(name = "Year", palette = "Set1") 
ggsave(here("fig", paste0(origin_country, "_age_state_0818.pdf")), width = 17, height = 8)

# might be nice to plot total over time



p_all %>% 
  fill(pop, .direction = "down") %>% 
  mutate(est_mig = pop*median,
         est_low = pop*lower,
         est_high = pop*upper) %>% 
  group_by(year, state) %>% 
  summarise(est_mig_tot = sum(est_mig), prop = est_mig_tot/sum(pop),
            prop_lower = sum(est_low)/sum(pop),
            prop_upper = sum(est_high)/sum(pop)) %>% 
  ggplot(aes(year, prop)) + geom_line(lwd = 0.9) + 
  geom_ribbon(aes(ymin = prop_lower, ymax = prop_upper), alpha = 0.2) + 
  facet_geo(~state) + 
  #ggtitle("Mexican migrants, proportion of population, 2001--2018")+
  theme_bw(base_size = 11)  + 
  ylab("proportion")
#ggsave("./usa_mex/fig/geo_facet_tot_2018.pdf", width = 13, height = 8)


# specific states

p_all %>% filter(state=="California") %>% 
  rowwise() %>% 
  mutate(y_lower = max(prop_mig-1.96*se, 0)) %>% 
  ggplot(aes(year, median)) +
  facet_wrap(~age_group_cat)+
  geom_point(aes(year, prop_mig, color = "ACS")) +
  geom_ribbon(aes(x = year, ymax = prop_mig+1.96*se, ymin = y_lower), fill = "red", alpha = 0.2)+
  geom_line(aes(color = "estimate"), size = 1.1) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5)+
  ylab("proportion") + 
  #ggtitle("Mexican migrants by age, California 2001--2018") +
  geom_point(data = fb_prop_age_new_mex %>% filter(state=="California") %>% 
               mutate(age_group_cat = paste(age_group, age_group+4, sep = "-")),
             aes(year, (estimated_prop), col = 'FB'), size = 1.5) +
  scale_color_manual(name = "", values = c("ACS" = "red", "estimate" = "black", "FB" = "blue")) +
  theme_bw(base_size = 18)
ggsave(here("fig", paste0(origin_country, "_CA_age_ts.pdf")), width = 10, height = 6)

p_all %>% filter(state=="Georgia") %>% 
  rowwise() %>% 
  mutate(y_lower = max(prop_mig-1.96*se, 0)) %>% 
  ggplot(aes(year, median)) +
  facet_wrap(~age_group_cat)+
  geom_point(aes(year, prop_mig, color = "ACS")) +
  geom_ribbon(aes(x = year, ymax = prop_mig+1.96*se, ymin = y_lower), fill = "red", alpha = 0.2)+
  geom_line(aes(color = "estimate"), size = 1.1) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5)+
  ylab("proportion") + 
  #ggtitle("Mexican migrants by age, California 2001--2018") +
  geom_point(data = fb_prop_age_new_mex %>% filter(state=="Georgia") %>% 
               mutate(age_group_cat = paste(age_group, age_group+4, sep = "-")),
             aes(year, (estimated_prop), col = 'FB'), size = 1.5) +
  scale_color_manual(name = "", values = c("ACS" = "red", "estimate" = "black", "FB" = "blue")) +
  theme_bw(base_size = 18)
ggsave(here("fig", paste0(origin_country, "_GA_age_ts.pdf")), width = 10, height = 6)

p_all %>% filter(state=="Connecticut") %>% 
  rowwise() %>% 
  mutate(y_lower = max(prop_mig-1.96*se, 0)) %>% 
  ggplot(aes(year, median)) +
  facet_wrap(~age_group_cat)+
  geom_point(aes(year, prop_mig, color = "ACS")) +
  geom_ribbon(aes(x = year, ymax = prop_mig+1.96*se, ymin = y_lower), fill = "red", alpha = 0.2)+
  geom_line(aes(color = "estimate"), size = 1.1) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.5)+
  ylab("proportion") + 
  #ggtitle("Mexican migrants by age, California 2001--2018") +
  geom_point(data = fb_prop_age_new_mex %>% filter(state=="Connecticut") %>% 
               mutate(age_group_cat = paste(age_group, age_group+4, sep = "-")),
             aes(year, (estimated_prop), col = 'FB'), size = 1.5) +
  scale_color_manual(name = "", values = c("ACS" = "red", "estimate" = "black", "FB" = "blue")) +
  theme_bw(base_size = 18)
ggsave(here("fig", paste0(origin_country, "_CT_age_ts.pdf")), width = 10, height = 6)

