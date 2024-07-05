# load packages         --------------------------------------------------------

library(tidyverse)   # ggplot, dplyr, and friends
library(brms)        # Bayesian modeling through Stan
library(psych)       # For describe()
library(correlation) # For correlations with nicer output
library(patchwork)   # For combining plots
library(parameters)  # Nicer output of model results
library(tidybayes)   # Manipulate brms objects in a tidy way
library(scales)      # For formatting labels in ggplot
library(extrafont)   # to change ggplot font
library(kableExtra)  # for Latextables
library(papaja)      # better printing 
library(bayestestR)  # for describe_posterior() 
library(emmeans)     # for contrast()
library(BayesFactor) # for ttestBF() and anovaBF()

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
                prior(normal(-0.20, 1),    class = "b", dpar ="", coef = "est_criterionKcal"),
                prior(student_t(3,0.20, 0.30),  class = "b", dpar ="", coef = "match_domain:est_criterionKcal"),
                prior(inv_gamma(5,1),      class = "sd"),
                prior(inv_gamma(5,1),      class = "sigma"),
                prior(lkj(2),              class = "cor")),
              prior_rank = c(
                prior(normal(0, 0.50),     class = "Intercept"),
                prior(normal(.05, 0.50),    class = "b", dpar ="", coef = "match_domain"),
                prior(normal(0, 0.50),     class = "b", dpar ="", coef = "est_criterionKcal"),
                prior(normal(0, 0.50),     class = "b", dpar ="", coef = "match_domain:est_criterionKcal"),
                prior(inv_gamma(5,1),      class = "sd"),
                prior(inv_gamma(5,1),      class = "sigma"))
)



effect_resp      <- function(mod,transform=FALSE){

  new_df <- data.frame(match_domain  = c(-0.5,0.5,-0.5,0.5),
                       est_criterion = c("CO2","Kcal","Kcal","CO2"))
  
  # compute model implied marginal means
  mm <- mod %>% epred_draws(newdata = new_df, re_formula = NA)

  if(transform){
    
    mm$.epred  <- z_fisher(z=mm$.epred)
    
  }
  
  
  df <- data.frame("match_no_CO2"   = mm[with(mm, match_domain  == -0.5 & est_criterion == "CO2"),]$.epred,
                   "match_yes_CO2"  = mm[with(mm, match_domain  ==  0.5 & est_criterion == "CO2"),]$.epred,
                   "match_no_Kcal"  = mm[with(mm, match_domain  == -0.5 & est_criterion == "Kcal"),]$.epred,
                   "match_yes_Kcal" = mm[with(mm, match_domain  ==  0.5 & est_criterion == "Kcal"),]$.epred)  %>% 
        rowwise() %>%   
        mutate("CO2"         = mean(c(match_no_CO2,match_yes_CO2)),
               "Kcal"        = mean(c(match_no_Kcal,match_yes_Kcal)),
               "diff_est"    = CO2-Kcal,
               "diff_CO2"    = match_yes_CO2  - match_no_CO2,
               "diff_Kcal"   = match_yes_Kcal - match_no_Kcal,
               "interaction" = diff_Kcal       - diff_CO2)
                   
    return(df)
  
 
  
}

# Load Data             ---------------------------------------------------------------

est <- read_csv2("Data/df_analysis.csv")

est <- est %>% 
        filter(item_type == "transfer") %>% 
        mutate(match_domain = match_domain-0.5) # Recode match domain as -0.5 -  0.5 for easier interpretability of est_criterion



# Manipulaton Check:    ---------------------

# Differences in general criterion knowledge

# Descriptives 
temp <- est %>% 
          select(ID,trained_criterion,est_criterion,Kcal_knowledge,CO2_knowledge) %>% 
          distinct() 


temp %>% 
  group_by(trained_criterion,est_criterion) %>% 
  summarize(m_kcal  = mean(Kcal_knowledge),
            sd_kcal = sd(Kcal_knowledge),
            m_CO2   = mean(CO2_knowledge),
            sd_CO2  = sd(CO2_knowledge))
  
temp %>% 
  summarize(m_kcal  = mean(Kcal_knowledge),
            sd_kcal = sd(Kcal_knowledge),
            m_CO2   = mean(CO2_knowledge),
            sd_CO2  = sd(CO2_knowledge))


result <- ttestBF(x = temp$Kcal_knowledge, y = temp$CO2_knowledge)
describe_posterior(result)



# Reactivity (SUPPLEMENT)

temp <- temp %>% 
          mutate(trained_f   = factor(trained_criterion),
                 estimated_f = factor(est_criterion)) %>% 
          as.data.frame()

temp %>% 
  group_by(est_criterion) %>% 
  summarize(m_kcal  = mean(Kcal_knowledge),
            sd_kcal = sd(Kcal_knowledge),
            m_CO2   = mean(CO2_knowledge),
            sd_CO2  = sd(CO2_knowledge))


bf_Kcal = anovaBF(Kcal_knowledge ~ trained_f * estimated_f, data=temp)
bf_Kcal


bf_CO2 = anovaBF(CO2_knowledge ~ trained_f * estimated_f, data=temp)
bf_CO2


temp %>% 
  pivot_longer(cols      = c(Kcal_knowledge,CO2_knowledge),
               names_to  = "criterion",
               values_to = "values") %>% 
  mutate(criterion   = ifelse(criterion   == "CO2_knowledge", "bold(Knowledge:~CO[2])","bold(Knowledge:~kcal)"),
         estimated_f = ifelse(estimated_f == "CO2","CO[2]","kcal")) %>% 
  ggplot(aes(x = estimated_f, y = values,  group = criterion)) +
    geom_jitter(width=0.1, height = 0.1)+ 
    stat_summary(fun.data = mean_se, geom = "errorbar",aes(color=criterion),width=0.1) +
    stat_summary(fun="mean",geom="line",lwd=0.8,aes(color=criterion)) +
    stat_summary(fun="mean",geom="point",size=4,aes(fill=criterion),shape=21) +
    scale_color_manual(values=c(clrs[4],clrs[3])) +
    scale_fill_manual(values=c(clrs[4],clrs[3])) +
    facet_wrap(. ~ criterion,labeller = label_parsed) +
    theme_nice() +
    scale_x_discrete(labels = parse_format()) +
    scale_y_continuous(breaks = 1:7) +
    labs(x = "Estimated Criterion",
         y = "Knowledge Rating") +
    theme(legend.position = "none") 



# H3a - Metric          -----------------------------------------------------------

res_OME_m2 <-  brm(OME_corr     ~ match_domain * est_criterion + (1|ID) + (est_criterion*match_domain|ID_item),
                   family       = lognormal(),
                   prior        = bayes$prior_OME,
                   data         = est,  
                   warmup       = bayes$warmup, 
                   iter         = bayes$iter, 
                   chains       = bayes$chains, 
                   seed         = bayes$bayes_seed,
                   cores        = bayes$cores,
                   file         = "Results/Models/fit_H3a_M2.rds",
                   backend      = "rstan",
                   sample_prior = "yes",
                   save_pars    = save_pars(all=TRUE))


res_OME_m1 <-  brm(OME_corr    ~ match_domain + est_criterion + (1|ID) + (est_criterion*match_domain|ID_item),
                  family       = lognormal(),
                  prior        = bayes$prior_OME[c(-4),],
                  data         = est,  
                  warmup       = bayes$warmup, 
                  iter         = bayes$iter, 
                  chains       = bayes$chains, 
                  seed         = bayes$bayes_seed,
                  cores        = bayes$cores,
                  file         = "Results/Models/fit_H3a_M1.rds",
                  backend      = "rstan",
                  sample_prior = "yes",
                  save_pars    = save_pars(all=TRUE))


res_OME_m0 <-  brm(OME_corr     ~ match_domain + (1|ID) + (est_criterion*match_domain|ID_item),
                   family       = lognormal(),
                   prior        = bayes$prior_OME[c(-3,-4),],
                   data         = est,  
                   warmup       = bayes$warmup, 
                   iter         = bayes$iter, 
                   chains       = bayes$chains, 
                   seed         = bayes$bayes_seed,
                   cores        = bayes$cores,
                   file         = "Results/Models/fit_H3a_M0.rds",
                   backend      = "rstan",
                   sample_prior = "yes",
                   save_pars    = save_pars(all=TRUE))



# Summary
model_parameters(res_OME_m2, effects="fixed")
model_parameters(res_OME_m1, effects="fixed")
model_parameters(res_OME_m0, effects="fixed")

# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_OME_m1, res_OME_m0) # 308185302721397504.00000
bayes_factor(res_OME_m2, res_OME_m1) # 7.65056


# Means and global average marginal effect on response scale

effect_resp(res_OME_m2) %>% 
  describe_posterior(centrality = "all", 
                     ci_method  = "hdi")




# H3b - Mapping         -----------------------------------------------------------


# Fit models
res_rank_m2  <- brm(rank_z       ~ match_domain*est_criterion + (1|ID),
                    family       = gaussian(),
                    prior        = bayes$prior_rank,
                    data         = est %>% select(ID,est_criterion,match_domain,rank_z) %>% distinct(),  
                    warmup       = bayes$warmup, 
                    iter         = bayes$iter, 
                    chains       = bayes$chains, 
                    seed         = bayes$bayes_seed,
                    cores        = bayes$cores,
                    file         = "Results/Models/fit_H3b_M2.rds",
                    backend      = "rstan",
                    sample_prior = "yes",
                    save_pars    = save_pars(all=TRUE))


res_rank_m1  <- brm(rank_z       ~ match_domain+est_criterion + (1|ID),
                    family       = gaussian(),
                    prior        = bayes$prior_rank[c(-4),],
                    data         = est %>% select(ID,est_criterion,match_domain,rank_z) %>% distinct(),  
                    warmup       = bayes$warmup, 
                    iter         = bayes$iter, 
                    chains       = bayes$chains, 
                    seed         = bayes$bayes_seed,
                    cores        = bayes$cores,
                    file         = "Results/Models/fit_H3b_M1.rds",
                    backend      = "rstan",
                    sample_prior = "yes",
                    save_pars    = save_pars(all=TRUE))



res_rank_m0  <- brm(rank_z       ~ match_domain + (1|ID),
                    family       = gaussian(),
                    prior        = bayes$prior_rank[c(-3,-4),],
                    data         = est %>% select(ID,est_criterion,match_domain,rank_z) %>% distinct(),  
                    warmup       = bayes$warmup, 
                    iter         = bayes$iter, 
                    chains       = bayes$chains, 
                    seed         = bayes$bayes_seed,
                    cores        = bayes$cores,
                    file         = "Results/Models/fit_H3b_M0.rds",
                    backend      = "rstan",
                    sample_prior = "yes",
                    save_pars    = save_pars(all=TRUE))



# Summary
model_parameters(res_rank_m2 ,effects="all")
model_parameters(res_rank_m1,effects="all")
model_parameters(res_rank_m0,effects="all")

# Calculate Bayes Factor
set.seed(34876)

bayes_factor(res_rank_m1, res_rank_m0) # 183.57138
bayes_factor(res_rank_m2, res_rank_m1) # 0.28498




# Marginal means and effects on response scale
describe_posterior(res_rank_m1)

effect_resp(res_rank_m1, transform = TRUE) %>% 
    describe_posterior(centrality = "all", 
                       ci_method  = "hdi") 


