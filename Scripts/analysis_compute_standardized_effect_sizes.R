# load packages                     --------------------------------------------------------

library(tidyverse)   # ggplot, dplyr, and friends
library(brms)        # Bayesian modeling through Stan
library(parameters)  # Nicer output of model results
library(tidybayes)   # Manipulate brms objects in a tidy way

# Setup                             --------------------------------------------------------

today <- format(Sys.Date(),format="%d_%m_%Y")
source("Scripts/functions.R")


# Load Data                         ---------------------------------------------------------------

est <- read_csv2("Data/df_analysis.csv") 

# Metric Knowledge  (Hypothesis 1a) ---------------------------------------------------------------

res_kcal_OME <-  brm(OME_corr     ~ match_domain + (1|ID) + (match_domain|ID_item),
                     family       = gaussian(),
                     data         = est %>% filter(item_type     == "transfer",
                                                   est_criterion == "Kcal"),  
                     warmup       = bayes$warmup, 
                     iter         = bayes$iter, 
                     chains       = bayes$chains, 
                     seed         = bayes$bayes_seed,
                     cores        = bayes$cores,
                     file         = "Results/Models/fit_H1a_Kcal_M1_normal.rds",
                     backend      = "rstan",
                     sample_prior = "yes",
                     save_pars    = save_pars(all=TRUE))


d_from_brms(res_kcal_OME,0.06) # 0.253149



res_CO2_OME <-  brm(OME_corr     ~ match_domain + (1|ID) + (match_domain|ID_item),
                     family       = gaussian(),
                     data         = est %>% filter(item_type     == "transfer",
                                                   est_criterion == "CO2"),  
                     warmup       = bayes$warmup, 
                     iter         = bayes$iter, 
                     chains       = bayes$chains, 
                     seed         = bayes$bayes_seed,
                     cores        = bayes$cores,
                     file         = "Results/Models/fit_H1a_CO2_M1_normal.rds",
                     backend      = "rstan",
                     sample_prior = "yes",
                     save_pars    = save_pars(all=TRUE))


d_from_brms(res_CO2_OME,0.51) # 0.7335665


# Metric Knowledge  (Hypothesis 2a) ---------------------------------------------------------------


res_kcal_OME <-  brm(OME_corr     ~ item_type + (item_type|ID) + (1|ID_item),
                     family       = gaussian(),
                     data         = est %>% filter(trained_criterion == "Kcal",
                                                   est_criterion     == "Kcal"),  
                     warmup       = bayes$warmup, 
                     iter         = bayes$iter, 
                     chains       = bayes$chains, 
                     seed         = bayes$bayes_seed,
                     cores        = bayes$cores,
                     file         = "Results/Models/fit_H2a_Kcal_M1_normal.rds",
                     backend      = "rstan",
                     sample_prior = "yes",
                     save_pars    = save_pars(all=TRUE))

d_from_brms(res_kcal_OME,0.05) # 0.2275606


# Mapping Knowledge (Hypothesis 2b) ---------------------------------------------------------------

res_kcal_rank <-  brm(rank_z       ~ item_type + (item_type|ID),
                      family       = gaussian(),
                      data         = est %>%
                                      filter(est_criterion      == "Kcal",
                                             trained_criterion  == "Kcal") %>% 
                                      select(ID,item_type,rank_z) %>% 
                                      distinct(),  
                      warmup       = bayes$warmup, 
                      iter         = bayes$iter, 
                      chains       = bayes$chains, 
                      seed         = bayes$bayes_seed,
                      cores        = bayes$cores,
                      file         = "Results/Models/fit_H2b_kcal_M1_normal.rds",
                      backend      = "rstan",
                      sample_prior = "yes",
                      save_pars    = save_pars(all=TRUE))


d_from_brms(res_kcal_rank,.13) # 0.301334


# Metric Knowledge  (Hypothesis 3a) ---------------------------------------------------------------

res_OME_m2 <-  brm(OME_corr     ~ match_domain * est_criterion + (1|ID) + (est_criterion*match_domain|ID_item),
                   family       = gaussian(),
                   data         = est %>% filter(item_type     == "transfer"),  
                   warmup       = bayes$warmup, 
                   iter         = bayes$iter, 
                   chains       = bayes$chains, 
                   seed         = bayes$bayes_seed,
                   cores        = bayes$cores,
                   file         = "Results/Models/fit_H3a_M2_normal.rds",
                   backend      = "rstan",
                   sample_prior = "yes",
                   save_pars    = save_pars(all=TRUE))

d_from_brms(res_OME_m2,0.67) # 1.172509
d_from_brms(res_OME_m2,0.54) # 0.9450072


# Mapping Knowledge (Hypothesis 3b) ---------------------------------------------------------------


res_rank_m2  <- brm(rank_z       ~ match_domain*est_criterion + (1|ID),
                    family       = gaussian(),
                    data         = est %>%
                                    filter(item_type == "transfer") %>% 
                                    select(ID,est_criterion,match_domain,rank_z) %>% 
                                    distinct(),  
                    warmup       = bayes$warmup, 
                    iter         = bayes$iter, 
                    chains       = bayes$chains, 
                    seed         = bayes$bayes_seed,
                    cores        = bayes$cores,
                    file         = "Results/Models/fit_H3b_M2_normal.rds",
                    backend      = "rstan",
                    sample_prior = "yes",
                    save_pars    = save_pars(all=TRUE))

d_from_brms(res_rank_m2,.11) # 0.3806749
