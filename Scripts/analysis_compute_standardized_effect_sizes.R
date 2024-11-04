# Load packages                     --------------------------------------------------------

library(tidyverse)   # ggplot, dplyr, and friends

# Load Data                         ---------------------------------------------------------------

est <- read_csv2("Data/df_analysis.csv") 

# Metric Knowledge  (Hypothesis 1a) ---------------------------------------------------------------


# Compute SD from data 
temp_sd <- est %>% 
            filter(item_type     == "transfer",
                   est_criterion == "Kcal") %>% 
            pull(OME_corr) %>% 
            sd()

# Effects as reported in manuscript
round(0.06/temp_sd,2) # 0.26




# Compute SD from data 
temp_sd <- est %>% 
            filter(item_type     == "transfer",
                   est_criterion == "CO2") %>% 
            pull(OME_corr) %>% 
            sd()

# Effects as reported in manuscript
round(0.51/temp_sd,2) # 0.68



# Metric Knowledge  (Hypothesis 2a) ---------------------------------------------------------------

# Compute SD from data 
temp_sd <- est %>% 
            filter(trained_criterion == "Kcal",
                   est_criterion     == "Kcal") %>% 
            pull(OME_corr) %>% 
            sd()

# Effects as reported in manuscript
round(0.05/temp_sd,2) # 0.23


# Mapping Knowledge (Hypothesis 2b) ---------------------------------------------------------------


# Compute SD from data 
temp_sd <- est %>% 
            filter(trained_criterion == "Kcal",
                   est_criterion     == "Kcal") %>% 
            select(ID,item_type,rank_z) %>% 
            distinct() %>% 
            pull(rank_z) %>% 
            sd()

# Effects as reported in manuscript
round(0.13/temp_sd,2) # 0.34 

# Metric Knowledge  (Hypothesis 3a) ---------------------------------------------------------------

# Compute SD from data 
temp_sd <- est %>% 
            filter(item_type     == "transfer") %>% 
            pull(OME_corr) %>% 
            sd()

# Effects as reported in manuscript
round(0.67/temp_sd,2) # 1.06
round(0.54/temp_sd,2) # 0.86




# Mapping Knowledge (Hypothesis 3b) ---------------------------------------------------------------


temp_sd <- est %>% 
            filter(item_type == "transfer") %>% 
            select(ID,est_criterion,match_domain,rank_z) %>%
            distinct() %>% 
            pull(rank_z) %>% 
            sd()

# Effects as reported in manuscript
round(0.11/temp_sd,2) # 1.06
