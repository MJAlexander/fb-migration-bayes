##### Facebook migration project
##### 3. Run the combined time series model


# 0. Packages and functions -------------------------------------------------------------

library(tidyverse)
library(here)
library(boot)
library(rjags)
library(narcan)
library(R2jags)
library(geofacet)
source(here("code","plottrace.R"))


# 1. Read in ACS data -----------------------------------------------------

# specify country of origin
origin_country <- "India"

acs_prop_age <- readRDS(here("data", "acs_for_model.RDS"))
acs_prop_age <- acs_prop_age %>% filter(origin == origin_country) # only country of interest
total_pops <- readRDS(here("data", "acs_total_pop.RDS"))


# 2. SVD ------------------------------------------------------------------

# want to get two principal components of migration age schedules from ACS data 

years <- 2001:2016
age_groups <- unique(acs_prop_age$age_group)

px <- as.matrix(acs_prop_age %>% 
                  select(age_group,state, year, prop_mig) %>%
                  spread(age_group, prop_mig) %>% 
                  filter(year!=2016) %>%  # remove data already used for bias model
                  select(-state)
)[,-1]

# remove all rows with NAs
px <- px[-(which(apply(is.na(px), 1, sum)>0)),]
log_px <- log(px)

# do SVD on the log matrix
svd_px <- svd(log_px)


# looks like probably only need first two

# nice ggplot

pcs <- tibble(Z1 = -1*svd_px$v[, 1], Z2 = svd_px$v[, 2], age = age_groups) %>% 
  mutate(age_group = paste(age, age+4, sep = "-"))

pcs %>% gather(pc, value, -age, -age_group) %>% 
  ggplot(aes(age, value)) + geom_line() + geom_point() + 
  facet_wrap(~pc, scales = "free_y") + xlab("age group") +
  theme_bw(base_size = 18)
ggsave(here("fig",paste0(origin_country, "_usa_pcs.pdf")), width = 8, height = 5)



# 3. Read in and process Facebook -----------------------------------------

# read in results from bias model
bias_df <- read_csv(here("output", paste0("bias_model_all_",origin_country,".csv")))

b <- (bias_df[grepl("b\\[", bias_df$X1),"mean"])[[1]]
b0 <- (bias_df[grepl("b0", bias_df$X1),"mean"])[[1]]
b_fb <- (bias_df[grepl("b_fb", bias_df$X1),"mean"])[[1]]
tau <- (bias_df[grepl("tau", bias_df$X1),"mean"])[[1]]

# read in facebook data 

d1 <- read_csv(here("data","master_acs_facebook_expat_wave1.csv"))
d2 <- read_csv(here("data","master_acs_facebook_expat_wave2.csv"))
d3 <- read_csv(here("data","master_acs_facebook_expat_wave3.csv"))
d4 <- read_csv(here("data","master_acs_facebook_expat_wave4.csv"))
d5 <- read_csv(here("data","master_acs_facebook_expat_wave5.csv"))


# bind all these together

df <- d3 %>% 
  rename(expat_population = expat_population_wave3, facebook_population = facebook_population_wave3) %>% 
  mutate(wave = 3) %>% 
  bind_rows(d4 %>% 
              rename(expat_population = expat_population_wave4, facebook_population = facebook_population_wave4) %>% 
              mutate(wave = 4) 
  ) %>% 
  bind_rows(d5 %>% 
              rename(expat_population = expat_population_wave5, facebook_population = facebook_population_wave5) %>% 
              mutate(wave = 5)) %>% 
  bind_rows(d1 %>% 
              rename(expat_population = expat_population_wave1, facebook_population = facebook_population_wave1) %>% 
              mutate(wave = 1)) %>% 
  bind_rows(d2 %>% 
              rename(expat_population = expat_population_wave2, facebook_population = facebook_population_wave2) %>% 
              mutate(wave = 2))


# keep males, get age groups, calculate proportions
fb_prop_age_new <- df %>% 
  filter(sex==1) %>% 
  mutate(exp_prop = expat_population/facebook_population) %>% 
  mutate(se = sqrt(exp_prop*(1-exp_prop)/facebook_population),
         se_log = se/exp_prop) %>% 
  mutate(age_group = as.numeric(unlist(lapply(strsplit(ages_ranges, "_"), '[[', 2)))) %>% 
  select(age_group, state, origin, expat_population, facebook_population, exp_prop, se, se_log, wave) 

# restrict to country or interest
fb_prop_age_new_mex <- (fb_prop_age_new %>% filter(origin==origin_country))

## REMOVE Vermont for Mexico, for some reason not in ACS
#fb_prop_age_new_mex <- fb_prop_age_new_mex %>% filter(state!= "Vermont")

# get rid of underscores in state names

fb_prop_age_new_mex <- fb_prop_age_new_mex %>% 
  mutate(state = gsub("_", " ", state))

# take the log
log_fb_prop.j <- log(fb_prop_age_new_mex$exp_prop)

# get design matrix to calculate bias adjusted proportions
x.j <- with(fb_prop_age_new_mex, model.matrix( ~ factor(age_group)+ factor(state)))
x.j <- x.j[,-1]
colnames(x.j) <- gsub("factor\\(.*\\)", "", colnames(x.j))

# calculate bias adjusted proportions
log_est_prop.j <- b0 + x.j %*% as.matrix(b) + log_fb_prop.j*b_fb

# put this as a new column in fb_mex
fb_prop_age_new_mex$estimated_prop <- exp(log_est_prop.j)

# add a year
fb_prop_age_new_mex$year <- ifelse(fb_prop_age_new_mex$wave<5, 2017, 2018)

saveRDS(fb_prop_age_new_mex, file = "data/fb_for_model.RDS")

# 4. Get data in JAGS format ----------------------------------------------

# principal components
#Y.x <- svd_px$v[, 1:2] 

# note, depending on the migrant origin, may need to switch pcs 
# e.g. india the baseline age pattern is represented by the second PC
Y.x <- svd_px$v[, c(2,1)] 

## make sure that all states in acs are also in FB. if not, remove for now 

states <- unique(fb_prop_age_new_mex$state)
ages <- unique(fb_prop_age_new_mex$age_group)
years <- 2001:2018

states_not_in_fb <- unique(acs_prop_age$state)[!(unique(acs_prop_age$state) %in% states)]

acs_prop_age <- acs_prop_age %>% filter(!(state %in% states_not_in_fb ))

# get ACS data in vector form
p.i <- rep(NA, nrow(acs_prop_age))
se.i <- rep(NA, nrow(acs_prop_age))

get_age.i <- rep(NA, nrow(acs_prop_age))
get_year.i <- rep(NA, nrow(acs_prop_age))
get_state.i <- rep(NA, nrow(acs_prop_age))

for(i in 1:nrow(acs_prop_age)){
  
  p.i[i] <- acs_prop_age$prop_mig[i]
  se.i[i] <- acs_prop_age$se_log[i]
  get_age.i[i] <- which(ages== acs_prop_age$age_group[i])
  get_year.i[i] <- which(years== acs_prop_age$year[i])
  get_state.i[i] <- which(states== acs_prop_age$state[i])
  
}


# tack on the Facebook observations

# calculate the maximum SE
max_ses <- acs_prop_age %>% 
  group_by(state) %>% 
  summarise(max_se = max(se_log))

# join this to the FB data 
fb_prop_age_new_mex <- fb_prop_age_new_mex %>% 
  left_join(max_ses)


for(i in 1:nrow(fb_prop_age_new_mex)){
  p.i <- c(p.i, (fb_prop_age_new_mex$estimated_prop)[i])
  se.i <- c(se.i, fb_prop_age_new_mex$max_se[i])
  get_age.i <- c(get_age.i,which(ages== fb_prop_age_new_mex$age_group[i]))
  get_state.i <- c(get_state.i,which(states== fb_prop_age_new_mex$state[i]))
  get_year.i <- c(get_year.i,which(years== fb_prop_age_new_mex$year[i]))
}

# put in a list for JAGS
jags.data <- list(log_p.i = log(p.i), 
                  se.i  = se.i,
                  get_age.i = get_age.i,
                  get_state.i = get_state.i,
                  get_year.i = get_year.i,
                  Y.x = as.matrix(Y.x), 
                  #ax = ax,
                  tau = tau,
                  X = length(age_groups), T = length(years), S = length(states), N = nrow(acs_prop_age),
                  nextra = nrow(fb_prop_age_new_mex))

# parameters to save

parnames <- c("beta.ts", 
              "alpha.s",
              "sigma.beta", 
              "sigma.ns",
              "eps.xts", "rho", 
              "chi", "sigma.ar",
              "delta",
              "mu.beta", "log_mu.xts")


# 5. Run MCMC -------------------------------------------------------------

# initial conditions: set AR error term to be zero 

inits_list <- list()
inits_list[["eps.xts"]] <- array(0, c(length(age_groups), length(years), length(states)))

all_inits <- list()
for(i in 1:1){
  all_inits[[i]] <- inits_list
}


# --- run model --- # 
mod <- jags.parallel(data = jags.data, 
                     parameters.to.save=parnames,
                     inits = all_inits,
                     n.chains = 4,
                     #n.iter = 20000,
                     model.file = here("code/models","model_usa_natural_2.txt"))

max(mod$BUGSoutput$summary[,"Rhat"])
mod$BUGSoutput$summary[mod$BUGSoutput$summary[,"Rhat"]>1.08,]
mcmc.array <- mod$BUGSoutput$sims.array

PlotTrace("mu.beta[17,2]", mcmc.array)
PlotTrace("sigma.ar[24]", mcmc.array)

save(mcmc.array, file = here("output", paste0(origin_country, "_mcmc_array.Rda")))



# 6. Save estimates and CIs -----------------------------------------------

p_all <- c()

for(k in 1:length(states)){
  p.xtp <- array(NA, c(length(age_groups), length(years), 3))
  
  
  for(i in 1:(length(years))){
    
    for(j in 1:length(age_groups)){
      p.ai <-  (c(mcmc.array[,,paste0("log_mu.xts[", j, ",",i, ",",k,"]")]))
      p.xtp[j,i, 1] <- median(p.ai)
      p.xtp[j,i, 2] <- quantile(p.ai, 0.025)
      p.xtp[j,i, 3] <- quantile(p.ai, 0.975)
    }
  }
  
  rownames(p.xtp) <- age_groups
  colnames(p.xtp) <- years
  dimnames(p.xtp)[[3]] <- c("median", "lower", "upper")
  
  p_df <- as.data.frame.table(exp(p.xtp), responseName = "number") 
  colnames(p_df) <- c("age_group", "year", "quantile", "number")
  p_df <- p_df %>% spread(quantile, number)
  p_df$year <- as.numeric(as.character(p_df$year))
  
  p_df$age_group <- as.numeric(as.character(p_df$age_group))
  p_df$age_group_cat <- paste(p_df$age_group, p_df$age_group+4, sep = "-")
  p_df <- p_df %>% left_join(acs_prop_age %>%  filter(state==states[k]))
  p_df$state <- states[k]
  
  p_all <- rbind(p_all, p_df)
}


p_all <- p_all %>% filter(!is.na(state))

write_csv(p_all, path = here("output", paste0(origin_country, "_results_ci.csv")))


# 7. Run model without FB (for validation) --------------------------------

mod <- jags.parallel(data = jags.data, 
                     parameters.to.save=parnames,
                     n.iter = 30000,
                     model.file = "code/models/model_usa_natural_no_fb_2.txt")

max(mod$BUGSoutput$summary[,"Rhat"])
mod$BUGSoutput$summary[mod$BUGSoutput$summary[,"Rhat"]>1.1,]
mcmc.array.nf <- mod$BUGSoutput$sims.array

# 8. Save validation estimates and CIs ------------------------------------

p_all <- c()

for(k in 1:length(states)){
  p.xtp <- array(NA, c(length(age_groups), length(years), 3))
  
  
  for(i in 1:(length(years))){
    
    for(j in 1:length(age_groups)){
      p.ai <-  (c(mcmc.array.nf[,,paste0("log_mu.xts[", j, ",",i, ",",k,"]")]))
      p.xtp[j,i, 1] <- median(p.ai)
      p.xtp[j,i, 2] <- quantile(p.ai, 0.025)
      p.xtp[j,i, 3] <- quantile(p.ai, 0.975)
    }
  }
  
  rownames(p.xtp) <- age_groups
  colnames(p.xtp) <- years
  dimnames(p.xtp)[[3]] <- c("median", "lower", "upper")
  
  p_df <- as.data.frame.table(exp(p.xtp), responseName = "number") 
  colnames(p_df) <- c("age_group", "year", "quantile", "number")
  p_df <- p_df %>% spread(quantile, number)
  p_df$year <- as.numeric(as.character(p_df$year))
  
  p_df$age_group <- as.numeric(as.character(p_df$age_group))
  p_df$age_group_cat <- paste(p_df$age_group, p_df$age_group+4, sep = "-")
  p_df <- p_df %>% left_join(acs_prop_age %>%  filter(state==states[k]))
  p_df$state <- states[k]
  
  p_all <- rbind(p_all, p_df)
}

write_csv(p_all, path = here("output", paste0(origin_country, "_results_nf_ci.csv")))

