##### Facebook migration project
##### 2. Run the Facebook bias model
##### Use the first wave of Facebook data and 2016 ACS data to get bias estimates


# 0. Packages -------------------------------------------------------------

library(tidyverse)
library(here)
library(boot)
library(rjags)
library(R2jags)
library(geofacet)


# 1. Read in ACS ----------------------------------------------------------
origin_country <- "India"
acs_prop_age <- readRDS(here("data", "acs_for_fb.RDS"))


# 2. Read in FB data wave 1 and join to ACS-----------------------------------------------

d <- read_csv(here("data","master_acs_facebook_expat_wave1.csv"))

# filter to just males, calculate age group, calculate proportions
fb_prop_age <- d %>% 
  filter(sex==1) %>% 
  mutate(exp_prop = expat_population_wave1/facebook_population_wave1) %>% 
  mutate(age_group = as.numeric(unlist(lapply(strsplit(ages_ranges, "_"), '[[', 2)))) %>% 
  select(age_group, state, origin, expat_population_wave1, exp_prop) 


# join together with acs 
prop_age <- fb_prop_age %>% 
  left_join(acs_prop_age)

# remove missing ages
prop_age <- prop_age[!is.na(prop_age$prop_mig),]

# add an age group category
prop_age <- prop_age %>% mutate(age_group_cat = paste(age_group, age_group+4, sep = "-"))

# want to just look at one country of origin
prop_age <- prop_age %>% filter(origin == origin_country)

# 3. Get data in format for JAGS ------------------------------------------

# make covariate matrix with first wave
x.i <- with(prop_age, model.matrix(log(prop_mig) ~ factor(age_group)+ factor(state)))
x.i <- x.i[,-1]
colnames(x.i) <- gsub("factor\\(.*\\)", "", colnames(x.i))
log_fb_prop.i <- log(prop_age$exp_prop)

# make a list of data for jags input

jags.data <- list(log_acs_prop = log(prop_age$prop_mig), 
                  se = prop_age$se_log,
                  n = nrow(x.i),
                  x.i = x.i,
                  K = ncol(x.i),
                  log_fb_prop.i = log_fb_prop.i)

# the parameters that we are interested in
parnames <- c("b0", "b_fb","b", "tau", "mu")


# 4. Run MCMC -------------------------------------------------------------

mod <- jags(data = jags.data, 
            parameters.to.save=parnames,
            model.file = here("code/models","model_bias.txt"))

# check convergence
max(mod$BUGSoutput$summary[,"Rhat"])

# pull out the samples
mcmc.array <- mod$BUGSoutput$sims.array


# 5. Plot some stuff ------------------------------------------------------

# have a look at the predicted values to see. 

prop_age$est <- inv.logit(mod$BUGSoutput$summary[grepl("mu", rownames(mod$BUGSoutput$summary) ), "mean"])
prop_age$lwr <- inv.logit(mod$BUGSoutput$summary[grepl("mu", rownames(mod$BUGSoutput$summary) ), "2.5%"])
prop_age$upr <- inv.logit(mod$BUGSoutput$summary[grepl("mu", rownames(mod$BUGSoutput$summary) ), "97.5%"])

prop_age %>% 
  ggplot(aes(age_group, est, group = state)) + geom_line(aes(color = "estimate"))+
  geom_point(aes(age_group, exp_prop, col = "FB"))+
  geom_point(aes(age_group, prop_mig, color = "ACS"))+
  geom_ribbon(aes(ymin = lwr, ymax = upr),  fill = 2, alpha = 0.2) + 
  scale_color_manual(name = "", values = c("ACS" = "black", "FB" = "blue", "estimate" = "red")) +
  facet_geo(~state, scales = "free_y") + 
  ylab("Proportion") + xlab("Age Group") + 
  #ggtitle(paste0("Bias-adjusted estimates for 2016 for ", origin_country)) + 
  theme_bw(base_size = 14)
ggsave(here("fig",paste0("bias_model_2016_all_states_",origin_country,".pdf")), width = 17, height = 8)


# 6. Save estimates -------------------------------------------------------

write.csv(as.data.frame(mod$BUGSoutput$summary), here("output",paste0("bias_model_all_",origin_country,".csv")))








