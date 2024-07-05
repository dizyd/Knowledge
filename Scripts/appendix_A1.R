# load packages
library(tidyverse)  # ggplot, dplyr, and friends
library(kableExtra) # for Latextables
library(papaja)     # better printing 



items <- read_csv2("Data/Items_design.CSV")


# Table A1

temp <- items %>% 
          #filter(category %in% c("fruit & vegetables","meat & fish","dairy")) %>% 
          select(name,category,CO2=`g CO2 pro kg`,kcal=`Kcal pro 100g`,seeding_CO2,seeding_Kcal) %>% 
          mutate(CO2  = printnum(CO2,digits=0),
                 CO2  = ifelse(seeding_CO2 == 1,  paste0("\\textbf{",CO2,"}"),CO2),
                 kcal = ifelse(seeding_Kcal == 1, paste0("\\textbf{",kcal,"}"),kcal)) %>%
          select(-seeding_CO2,-seeding_Kcal,-category)

bind_cols(temp[1:12,],temp[13:24,],temp[25:36,], temp[37:48,], temp[49:60,]) %>% 
  kable(format    = "latex",digits=2,booktabs=TRUE,align="c",
        col.names = rep(c("Item","g CO_2/kg","kcal/100g"),5),
        caption   = "$CO_2$ footprint, calories and cooresponding category for each of the 60 food items.",
        escape    = FALSE) %>%
  add_header_above(header = c("fruit & vegetables" = 3, "meat & fish" = 3,
                              "dairy products" = 3, "grain (based) products" = 3,
                              "sweets" = 3)) %>% 
  kableExtra::kable_styling(latex_options = c("scale_down")) %>%
  footnote(general           = "\\\\footnotesize\\{Seeding items for the corresponding category and criterion are highlighted in bold. 
           Units are g $CO_2$/kg and kcal/100g.\\}",
           footnote_as_chunk = TRUE,
           threeparttable    = TRUE,
           escape            = FALSE,
           general_title     = "\\\\footnotesize\\{Note.\\}",
           title_format      = c("italic")) %>% 
  landscape(margin = 0)
  
  
  
  
