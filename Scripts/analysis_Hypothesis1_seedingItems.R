# load packages         --------------------------------------------------------

library(tidyverse)   # ggplot, dplyr, and friends
library(brms)        # Bayesian modeling through Stan
library(psych)       # For describe()
library(correlation) # For correlations with nicer output and z_fisher()
library(patchwork)   # For combining plots
library(parameters)  # Nicer output of model results
library(tidybayes)   # Manipulate brms objects in a tidy way
library(scales)      # For formatting labels in ggplot
library(extrafont)   # to change ggplot font
library(kableExtra)  # for Latextables
library(papaja)      # better printing 
library(bayestestR)  # for describe posterior


# Setup                 --------------------------------------------------------

today <- format(Sys.Date(),format="%d_%m_%Y")
source("Scripts/functions.R")



bayes <- list(chains     = 4,
              iter       = 10000,
              warmup     = 1000,
              bayes_seed = 1701,
              cores      = 4,
              prior_OME = c(
                prior(normal(-0.75, 0.50), class = "Intercept"),
                prior(student_t(3,-0.20, 0.15), class = "b", dpar ="", coef = "match_domain"),
                prior(inv_gamma(5,1),      class = "sd"),
                prior(inv_gamma(5,1),      class = "sigma"),
                prior(lkj(2),              class = "cor")),
              prior_rank = c(
                prior(normal(0, 0.50),    class = "Intercept"),
                prior(normal(0.05, 0.50), class = "b", dpar ="", coef = "match_domain"),
                prior(inv_gamma(5,1),     class = "sd"),
                prior(inv_gamma(5,1),     class = "sigma"))
)



effect_resp <- function(mod){
  
  new_df <- data.frame(match_domain  = c(-0.5,0.5))
  
  # compute model implied marginal means
  mm <- mod %>% epred_draws(newdata = new_df, re_formula = NA)
  
  df <- data.frame("match_no"  = mm[with(mm, match_domain  == -0.5),]$.epred,
                   "match_yes" = mm[with(mm, match_domain  == 0.5),]$.epred) %>% 
          mutate("diff" = match_no-match_yes)
  
  
  return(df)
  
  
}



# Load Data             ---------------------------------------------------------------

est <- read_csv2("Data/df_analysis.csv") %>% 
        filter(item_type         == "seeding",
               trained_criterion == est_criterion) %>% 
        mutate(match_domain  = match_domain-0.5)

# H1a - Kcal            -----------------------------------------------------------

res_kcal_OME <-  brm(OME_corr     ~ match_domain + (1|ID) + (match_domain|ID_item),
                     family       = lognormal(),
                     prior        = bayes$prior_OME,
                     data         = est %>% filter(est_criterion == "Kcal"),  
                     warmup       = bayes$warmup, 
                     iter         = bayes$iter, 
                     chains       = bayes$chains, 
                     seed         = bayes$bayes_seed,
                     cores        = bayes$cores,
                     file         = "Results/Models/fit_H1a_Kcal_M1_seedingItemsOnly.rds",
                     backend      = "rstan",
                     sample_prior = "yes",
                     save_pars    = save_pars(all=TRUE))



res_kcal_OME0 <-  brm(OME_corr ~ 1 + (1|ID) + (match_domain|ID_item), 
                      family       = lognormal(),
                      prior        = bayes$prior_OME[-2,],
                      data         = est %>% filter(est_criterion == "Kcal"),  
                      warmup       = bayes$warmup, 
                      iter         = bayes$iter, 
                      chains       = bayes$chains, 
                      seed         = bayes$bayes_seed,
                      cores        = bayes$cores,
                      file         = "Results/Models/fit_H1a_Kcal_M0_seedingItemsOnly.rds",
                      backend      = "rstan",
                      sample_prior = "yes",
                      save_pars    = save_pars(all=TRUE))





# Summary
model_parameters(res_kcal_OME ,effects="all")
model_parameters(res_kcal_OME0,effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_kcal_OME, res_kcal_OME0) # 1.01722


# Means and global average marginal effect on response scale
effect_resp(res_kcal_OME) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")



# H1a - CO2             -----------------------------------------------------------


res_CO2_OME <-  brm(OME_corr     ~ match_domain + (1|ID) + (match_domain|ID_item),
                    family       = lognormal(),
                    prior        = bayes$prior_OME,
                    data         = est %>% filter(est_criterion == "CO2"),  
                    warmup       = bayes$warmup, 
                    iter         = bayes$iter, 
                    chains       = bayes$chains, 
                    seed         = bayes$bayes_seed,
                    cores        = bayes$cores,
                    file         = "Results/Models/fit_H1a_CO2_M1_seedingItemsOnly.rds",
                    backend      = "rstan",
                    sample_prior = "yes",
                    save_pars    = save_pars(all=TRUE))


res_CO2_OME0 <-  brm(OME_corr ~ 1 + (1|ID) + (match_domain|ID_item), 
                     family       = lognormal(),
                     prior        = bayes$prior_OME[-2,],
                     data         = est %>% filter(est_criterion == "CO2"),  
                     warmup       = bayes$warmup, 
                     iter         = bayes$iter, 
                     chains       = bayes$chains, 
                     seed         = bayes$bayes_seed,
                     cores        = bayes$cores,
                     file         = "Results/Models/fit_H1a_CO2_M0_seedingItemsOnly.rds",
                     backend      = "rstan",
                     sample_prior = "yes",
                     save_pars    = save_pars(all=TRUE))



# Summary
model_parameters(res_CO2_OME , effects="all")
model_parameters(res_CO2_OME0, effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_CO2_OME, res_CO2_OME0) # 0.96319


# Means and global average marginal effect on response scale
effect_resp(res_CO2_OME) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")




# H1b - Kcal            -----------------------------------------------------------

# Fit models
res_kcal_rank <-  brm(rank_z       ~ match_domain + (1|ID), 
                      family       = gaussian(),
                      prior        = bayes$prior_rank,
                      data         = est %>% 
                                      filter(est_criterion == "Kcal") %>% 
                                      select(ID,match_domain,rank_z) %>% 
                                      distinct(),  
                      warmup       = bayes$warmup, 
                      iter         = bayes$iter, 
                      chains       = bayes$chains, 
                      seed         = bayes$bayes_seed,
                      cores        = bayes$cores,
                      file         = "Results/Models/fit_H1b_kcal_M1_seedingItemsOnly.rds",
                      backend      = "rstan",
                      sample_prior = "yes",
                      save_pars    = save_pars(all=TRUE))




res_kcal_rank0 <-   brm(rank_z         ~ 1 + (1|ID), 
                        family       = gaussian(),
                        prior        = bayes$prior_rank[-2,],
                        data         = est %>% 
                                        filter(est_criterion == "Kcal") %>% 
                                        select(ID,match_domain,rank_z) %>% 
                                        distinct(),  
                        warmup       = bayes$warmup, 
                        iter         = bayes$iter, 
                        chains       = bayes$chains, 
                        seed         = bayes$bayes_seed,
                        cores        = bayes$cores,
                        file         = "Results/Models/fit_H1b_kcal_M0_seedingItemsOnly.rds",
                        backend      = "rstan",
                        sample_prior = "yes",
                        save_pars    = save_pars(all=TRUE))



# Summary
model_parameters(res_kcal_rank ,effects="all")
model_parameters(res_kcal_rank0,effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_kcal_rank, res_kcal_rank0) # 1.00404


# Means and global average marginal effect on response scale
effect_resp(res_kcal_rank) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")




# H1b - CO2             -----------------------------------------------------------


# Fit models
res_CO2_rank  <-  brm(rank_z       ~ match_domain + (1|ID), 
                      family       = gaussian(),
                      prior        = bayes$prior_rank,
                      data         = est %>% 
                                      filter(est_criterion == "CO2") %>% 
                                      select(ID,match_domain,rank_z) %>% 
                                      distinct(),  
                      warmup       = bayes$warmup, 
                      iter         = bayes$iter, 
                      chains       = bayes$chains, 
                      seed         = bayes$bayes_seed,
                      cores        = bayes$cores,
                      file         = "Results/Models/fit_H1b_CO2_M1_seedingItemsOnly.rds",
                      backend      = "rstan",
                      sample_prior = "yes",
                      save_pars    = save_pars(all=TRUE))





res_CO2_rank0  <-   brm(rank_z       ~ 1 + (1|ID), 
                        family       = gaussian(),
                        prior        = bayes$prior_rank[-2,],
                        data         = est %>% 
                          filter(est_criterion == "CO2") %>% 
                          select(ID,match_domain,rank_z) %>% 
                          distinct(),  
                        warmup       = bayes$warmup, 
                        iter         = bayes$iter, 
                        chains       = bayes$chains, 
                        seed         = bayes$bayes_seed,
                        cores        = bayes$cores,
                        file         = "Results/Models/fit_H1b_CO2_M0_seedingItemsOnly.rds",
                        backend      = "rstan",
                        sample_prior = "yes",
                        save_pars    = save_pars(all=TRUE))


# Summary
model_parameters(res_CO2_rank ,effects="all")
model_parameters(res_CO2_rank0,effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_CO2_rank, res_CO2_rank0) # 0.98160


# Means and global average marginal effect on response scale
effect_resp(res_CO2_rank) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi") 



