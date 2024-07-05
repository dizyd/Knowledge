# load packages -----------------------------------------------------------


library(tidyverse)  # ggplot, dplyr, and friends
library(brms)       # Bayesian modeling through Stan
library(tidybayes)  # Manipulate brms objects in a tidy way
library(ggdist)
library(distributional)
library(parameters) # Nicer output of model results
library(marginaleffects)


# Setup         -------------------------------------------------------------------


clrs <- c("#54AA8F","#00335B",
          "#22A884FF","#414487FF",
          "#496aa2","#e46c0a")



theme_nice <- function(){
  theme_minimal(base_family = "Jost") +  
    theme(plot.title       = element_text(hjust = 0.5,size = 20),
          panel.grid.minor = element_blank(),
          text             = element_text(size  = 20),
          panel.border     = element_rect(colour = "black", linewidth = 0.5, fill = NA),
          axis.title.x     = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y     = element_text(margin = unit(c(3, 3, 0, 0), "mm"), angle = 90),
          legend.title     = element_text(face = "bold",size=16),
          strip.text       = element_text(face = "bold"),
          legend.position  = "bottom"
    )}



# Generate samples & plot -------------------------------------------------


temp      <- data.frame(DV = 1, match_domain = c(rep(1,5000),rep(0,5000)))

fit_prior <- brm(DV  ~ 1 + match_domain,
                 data   = temp,
                 family = lognormal(),
                 prior  = c(prior(normal(-0.75, 0.50), class = "Intercept"),
                            prior(student_t(3,-0.20, 0.15), class = "b", dpar ="", coef = "match_domain"),
                            prior(inv_gamma(5,1),      class = "sigma")),
                 sample_prior = "only",
                 iter         = 10000)


temp      <- fit_prior %>% 
                epred_draws(newdata    = data.frame(match_domain  = c(0,1)),
                            re_formula = NA) 

temp_int  <- data.frame(temp[which(temp$match_domain==0),".epred"])

temp_diff <- data.frame(temp[which(temp$match_domain==0),".epred"],
                        temp[which(temp$match_domain==1),".epred"]) %>% 
              mutate(diff=.epred.1-.epred) 



# Plot transformed parameters

data.frame("class"   = factor(c(rep("intercept",nrow(temp_int)),rep("b",nrow(temp_diff))),
                              levels = c("intercept","b")), 
           "samples" = c(temp_int$.epred,temp_diff$diff)) %>% 
  filter(samples < 3 & samples > -1) %>%  # for plottin, removes n=7 samples
  ggplot(aes(x = samples, fill = class)) +
    stat_slabinterval(point_interval = "median_hdi",normalize = "panels", .width=.95) +
    scale_fill_manual(values=c("grey35","grey35")) +
    facet_wrap(.~class,labeller = label_parsed,scales="free") +
    labs(y     = "Density",
         x     = "OME") +
    theme_nice() +
    theme(legend.position = "none")


# Plot predictions



# conditional_preds <- predictions(
#   fit_prior, 
#   newdata    = data.frame(match_domain  = c(0,1)),
#   re_formula = NA, 
#   by = "match_domain") %>% posteriordraws()
# 
# conditional_preds %>% 
#   ggplot(aes(x = draw, fill = factor(match_domain  ))) +
#   stat_halfeye() +
#   scale_fill_manual(values = c(clrs[3], clrs[4])) +
#   theme_nice()





# Save Plot ---------------------------------------------------------------




ggsave("Results/Plots/appendix_A2.pdf",
       width=10,height=5,bg="white",dpi=300,device = cairo_pdf)








