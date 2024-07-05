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
                prior(normal(0, 1), class = "Intercept"),
                prior(student_t(3,0, 1), class = "b", dpar ="", coef = "match_domain"),
                prior(inv_gamma(5,1),      class = "sd"),
                prior(inv_gamma(5,1),      class = "sigma"),
                prior(lkj(2),              class = "cor")))




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
        filter(item_type     == "transfer") %>% 
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
                     file         = "Results/Models/fit_H1a_Kcal_M1_sensitivity.rds",
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
                      file         = "Results/Models/fit_H1a_Kcal_M0_sensitivity.rds",
                      backend      = "rstan",
                      sample_prior = "yes",
                      save_pars    = save_pars(all=TRUE))





# Summary
model_parameters(res_kcal_OME ,effects="all")
model_parameters(res_kcal_OME0,effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_kcal_OME, res_kcal_OME0) # 0.36580


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
                    file         = "Results/Models/fit_H1a_CO2_M1_sensitivity.rds",
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
                     file         = "Results/Models/fit_H1a_CO2_M0_sensitivity.rds",
                     backend      = "rstan",
                     sample_prior = "yes",
                     save_pars    = save_pars(all=TRUE))



# Summary
model_parameters(res_CO2_OME , effects="all")
model_parameters(res_CO2_OME0, effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_CO2_OME, res_CO2_OME0) # 7658.61501


# Means and global average marginal effect on response scale
effect_resp(res_CO2_OME) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")




