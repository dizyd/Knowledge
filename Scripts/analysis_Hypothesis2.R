# load packages         --------------------------------------------------------

library(tidyverse)  # ggplot, dplyr, and friends
library(brms)       # Bayesian modeling through Stan
library(psych)      # For describe()
library(correlation)# For correlations with nicer output
library(patchwork)  # For combining plots
library(parameters) # Nicer output of model results
library(tidybayes)  # Manipulate brms objects in a tidy way
library(scales)     # For formatting labels in ggplot
library(extrafont)  # to change ggplot font
library(kableExtra) # for Latextables
library(papaja)     # better printing 
library(bayestestR) # for describe_posterior() 
library(emmeans)    # for contrast()


# Setup                 --------------------------------------------------------

today <- format(Sys.Date(),format="%d_%m_%Y")
source("Scripts/functions.R")


bayes <- list(chains     = 4,
              iter       = 10000,
              warmup     = 1000,
              bayes_seed = 1701,
              cores      = 4,
              prior_OME = c(
                prior(normal(-1.00, 0.50), class = "Intercept"),
                prior(student_t(3,0.10, 0.20), class = "b"),
                prior(inv_gamma(5,1),      class = "sd"),
                prior(inv_gamma(5,1),      class = "sigma"),
                prior(lkj(2),              class = "cor")),
              prior_rank = c(
                prior(normal(0, 0.50),     class = "Intercept"),
                prior(normal(-.05, .50),    class = "b"),
                prior(inv_gamma(5,1),      class = "sd"),
                prior(inv_gamma(5,1),      class = "sigma"))
)



effect_resp <- function(mod,transform=FALSE){
  
  new_df <- data.frame(item_type  = c(-0.5,0.5))
  
  # compute model implied marginal means
  mm <- mod %>% epred_draws(newdata = new_df, re_formula = NA)
  
  if(transform){
    
    mm$.epred  <- z_fisher(z=mm$.epred)
    
  }
  
  df <- data.frame("it_seeding"  =  mm[with(mm,item_type == -0.5),]$.epred,
                   "it_transfer" =  mm[with(mm,item_type ==  0.5),]$.epred) %>% 
           mutate("diff" = it_seeding-it_transfer)
  
  
  return(df)
  
  
}

# Load Data             ---------------------------------------------------------------

est <- read_csv2("Data/df_analysis.csv")  %>% 
          mutate(item_type  = ifelse(item_type == "seeding",-0.5,0.5))

# H2a - Kcal            -----------------------------------------------------------

res_kcal_OME <-  brm(OME_corr     ~ item_type + (item_type|ID) + (1|ID_item),
                     family       = lognormal(),
                     data         = est %>% filter(trained_criterion == "Kcal",
                                                   est_criterion     == "Kcal"),  
                     prior        = bayes$prior_OME,
                     warmup       = bayes$warmup, 
                     iter         = bayes$iter, 
                     chains       = bayes$chains, 
                     seed         = bayes$bayes_seed,
                     cores        = bayes$cores,
                     file         = "Results/Models/fit_H2a_Kcal_M1.rds",
                     backend      = "rstan",
                     sample_prior = "yes",
                     save_pars    = save_pars(all=TRUE))


res_kcal_OME0 <-  brm(OME_corr     ~ 1 + (item_type|ID) + (1|ID_item), 
                      family       = lognormal(),
                      data         = est %>% filter(trained_criterion == "Kcal",
                                                    est_criterion     == "Kcal"),  
                      prior        = bayes$prior_OME[-2,],
                      warmup       = bayes$warmup, 
                      iter         = bayes$iter, 
                      chains       = bayes$chains, 
                      seed         = bayes$bayes_seed,
                      cores        = bayes$cores,
                      file         = "Results/Models/fit_H2a_Kcal_M0.rds",
                      backend      = "rstan",
                      sample_prior = "yes",
                      save_pars    = save_pars(all=TRUE))


# Summary
model_parameters(res_kcal_OME ,effects="all")
model_parameters(res_kcal_OME0,effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_kcal_OME, res_kcal_OME0) # 1.51149


# Means and global average marginal effect on response scale
effect_resp(res_kcal_OME) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")


# H2a - CO2             -----------------------------------------------------------


res_CO2_OME <-  brm(OME_corr     ~ item_type + (item_type|ID) + (1|ID_item),
                    family       = lognormal(),
                    data         = est %>% filter(est_criterion     == "CO2",
                                                  trained_criterion == "CO2"),  
                    prior        = bayes$prior_OME,
                    warmup       = bayes$warmup, 
                    iter         = bayes$iter, 
                    chains       = bayes$chains, 
                    seed         = bayes$bayes_seed,
                    cores        = bayes$cores,
                    file         = "Results/Models/fit_H2a_CO2_M1.rds",
                    backend      = "rstan",
                    sample_prior = "yes",
                    save_pars    = save_pars(all=TRUE))


res_CO2_OME0 <-  brm(OME_corr     ~ 1 + (item_type|ID) + (1|ID_item), 
                     family       = lognormal(),
                     data         = est %>% filter(est_criterion     == "CO2",
                                                   trained_criterion == "CO2"),  
                     prior        = bayes$prior_OME[-2,],
                     warmup       = bayes$warmup, 
                     iter         = bayes$iter, 
                     chains       = bayes$chains, 
                     seed         = bayes$bayes_seed,
                     cores        = bayes$cores,
                     file         = "Results/Models/fit_H2a_CO2_M0.rds",
                     backend      = "rstan",
                     sample_prior = "yes",
                     save_pars    = save_pars(all=TRUE))


# Summary
model_parameters(res_CO2_OME ,effects="all")
model_parameters(res_CO2_OME0,effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_CO2_OME, res_CO2_OME0) # 0.47615


# Means and global average marginal effect on response scale
effect_resp(res_CO2_OME) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")

# H2b - Kcal            -----------------------------------------------------------

# Fit models
res_kcal_rank <-  brm(rank_z       ~ item_type + (item_type|ID),
                      family       = gaussian(),
                      data         = est %>% filter(est_criterion     == "Kcal",
                                                    trained_criterion == "Kcal") %>% 
                                              select(ID,item_type,rank_z) %>% 
                                              distinct(),  
                      prior        = bayes$prior_rank,
                      warmup       = bayes$warmup, 
                      iter         = bayes$iter, 
                      chains       = bayes$chains, 
                      seed         = bayes$bayes_seed,
                      cores        = bayes$cores,
                      file         = "Results/Models/fit_H2b_kcal_M1.rds",
                      backend      = "rstan",
                      sample_prior = "yes",
                      save_pars    = save_pars(all=TRUE))




res_kcal_rank0 <-   brm(rank_z       ~ 1 + (item_type|ID),
                        family       = gaussian(),
                        data         = est %>% filter(est_criterion     == "Kcal",
                                                      trained_criterion == "Kcal") %>% 
                                                select(ID,item_type,rank_z) %>% 
                                                distinct(),  
                        prior        = bayes$prior_rank[-2,],
                        warmup       = bayes$warmup, 
                        iter         = bayes$iter, 
                        chains       = bayes$chains, 
                        seed         = bayes$bayes_seed,
                        cores        = bayes$cores,
                        file         = "Results/Models/fit_H2b_kcal_M0.rds",
                        backend      = "rstan",
                        sample_prior = "yes",
                        save_pars    = save_pars(all=TRUE))



# Summary
model_parameters(res_kcal_rank ,effects="all")
model_parameters(res_kcal_rank0,effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_kcal_rank, res_kcal_rank0) # 1.39670


# Means and global average marginal effect on response scale
effect_resp(res_kcal_rank, transform = TRUE) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")



# H2b - CO2             -----------------------------------------------------------


# Fit models
res_CO2_rank  <-  brm(rank_z       ~ item_type + (item_type|ID),
                      family       = gaussian(),
                      data         = est %>% filter(est_criterion     == "CO2",
                                                    trained_criterion == "CO2") %>% 
                                              select(ID,item_type,rank_z) %>% 
                                              distinct(),  
                      prior        = bayes$prior_rank,
                      warmup       = bayes$warmup, 
                      iter         = bayes$iter, 
                      chains       = bayes$chains, 
                      seed         = bayes$bayes_seed,
                      cores        = bayes$cores,
                      file         = "Results/Models/fit_H2b_CO2_M1.rds",
                      backend      = "rstan",
                      sample_prior = "yes",
                      save_pars    = save_pars(all=TRUE))





res_CO2_rank0  <-   brm(rank_z       ~ 1 + (item_type|ID),
                        family       = gaussian(),
                        data         = est %>% filter(est_criterion     == "CO2",
                                                      trained_criterion == "CO2") %>% 
                                                select(ID,item_type,rank_z) %>% 
                                                distinct(),  
                        prior        = bayes$prior_rank[-2,],
                        warmup       = bayes$warmup, 
                        iter         = bayes$iter, 
                        chains       = bayes$chains, 
                        seed         = bayes$bayes_seed,
                        cores        = bayes$cores,
                        file         = "Results/Models/fit_H2b_CO2_M0.rds",
                        backend      = "rstan",
                        sample_prior = "yes",
                        save_pars    = save_pars(all=TRUE))



# Summary
model_parameters(res_CO2_rank ,effects="all")
model_parameters(res_CO2_rank0,effects="all")


# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_CO2_rank, res_CO2_rank0) # 0.11688


# Means and global average marginal effect on response scale
effect_resp(res_CO2_rank, transform = TRUE) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")




