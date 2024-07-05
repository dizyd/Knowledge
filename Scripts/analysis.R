# load packages                               --------------------------------------------------------

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



# Setup                                       --------------------------------------------------------

today <- format(Sys.Date(),format="%d_%m_%Y")
source("Scripts/functions.R")

# Read in data                                ------------------------------------------------------------

insta0 <- read_csv2("Data/data_insta_seeding.csv")

# Check data
str(insta0)

# Initial N = 161
cat("N: ", insta0$ID %>% unique() %>% length())


temp <- insta0 %>% select(ID,trained_criterion,est_criterion) %>% distinct()
table(temp$trained_criterion)
table(temp$trained_criterion,temp$est_criterion)

# Filtering Persons                           -------------------------------------------------------

# Completed survey

f_complete <- insta0 %>% 
                select(ID,age) %>% distinct() %>% 
                filter(is.na(age)) %>% .$ID

# Based on preregistration

# Liked posts
f_ID_n_posts  <- insta0 %>% 
                    select(ID,n_posts_liked) %>% distinct() %>% 
                    filter(n_posts_liked < 1) %>% .$ID

# 2AFC performance
f_ID_2AFC     <- insta0 %>%
                  group_by(ID) %>%
                  summarize(p_corr = mean(img_clicked == "left",na.rm = T),
                            .groups = "drop") %>%
                  filter(p_corr <= .5) %>% .$ID

# Quality
f_ID_qual     <- insta0 %>% 
                  select(ID,quality) %>% distinct() %>% 
                  filter(quality == "no") %>% .$ID

# Aid
f_ID_cheat    <- insta0 %>% 
                  select(ID,cheated) %>% distinct() %>% 
                  filter(cheated == "yes") %>% .$ID

# Technical problems
f_ID_tech     <- insta0 %>% 
                  select(ID,technical_problems) %>% distinct() %>% 
                  filter(technical_problems == "yes") %>% .$ID

# Based on comments:
f_ID_tech     <- c("i73e5s070k","8zdmkbocs5") # "h3xdtjlrbz" "ppbbk37ej4" seem ok to keep, i73e5s070k is already in n_posts


# Estimates
f_ID_est      <- insta0 %>% 
                  filter(phase == "estimation") %>% 
                  select(ID,judgment) %>% group_by(ID) %>%
                  summarise(unique_ests = unique(judgment) %>% length(), 
                            .groups = "drop") %>% 
                  filter(unique_ests <= 2) %>% .$ID

# Time
f_ID_rts    <- insta0 %>% 
                  filter(phase == "estimation") %>% 
                  select(ID,rt_ms) %>% group_by(ID) %>%
                  summarise(rt_ms_below_1s = mean(rt_ms<1000), 
                            .groups = "drop") %>% 
                  filter(rt_ms_below_1s >= 0.1) %>% .$ID

cat("N: ", insta0$ID %>% unique() %>% length())

filter_IDs <- unique(c(f_complete,
                       f_ID_n_posts,f_ID_qual,f_ID_2AFC,
                       f_ID_cheat,f_ID_tech,
                       f_ID_est,
                       f_ID_rts))


insta <- insta0 %>% filter(!ID %in% filter_IDs)

cat("N: ", insta$ID %>% unique() %>% length()) # N:  144


temp <- insta %>% select(ID,trained_criterion,est_criterion) %>% distinct()
table(temp$trained_criterion)
table(temp$trained_criterion,temp$est_criterion)





# Additional Checks                           -------------------------------------------------------


# Dayes posts liked
insta %>% 
  select(ID,n_dates_liked) %>% distinct() -> temp_dates_liked

# Total time for estimation phase
insta %>% 
  select(ID,phase,est_phase_time_min) %>% distinct() -> temp_time

# Comments
insta %>% 
  select(ID,other_comments) %>% na.omit() %>% distinct() -> temp_comments

# Manual changes based on comment
insta[which(insta$ID == "e4mcov7hay" & insta$ID_item == 49),"judgment"] <- 500

# n trained criterion
insta %>% 
  select(ID,trained_criterion) %>% distinct() %>% 
  .$trained_criterion %>% table()



# Demographics                                ------------------------------------------------------------

demo <- insta %>%
          select(ID, age, gender, education_level, SNS_1, SNS_2, SNS_3) %>%
          distinct() %>% rowwise() %>% 
          mutate(SNS = mean(c(SNS_1,SNS_2,SNS_3)))
          
describe(demo$age) %>% select(n,mean,median,sd,min,max)
ez_tables(demo$gender)
ez_tables(demo$education_level)
describe(demo$SNS) %>% select(n,mean,median,sd,min,max)


psych::alpha(demo[,c("SNS_1","SNS_2","SNS_3")])


# Prepare data for analysis                   -----------------------------------------------

est   <- insta %>%
            mutate(match_domain      = ifelse(trained_criterion == est_criterion,1,0)) %>% 
            filter(phase == "estimation") %>% 
            rowwise() %>% 
            mutate(criterion  = ifelse(est_criterion == "Kcal",crit_kcal,crit_CO2),
                   sign_OME   = log10(judgment / criterion),
                   OME        = abs(log10(judgment / criterion)),
                   OME_corr   = ifelse(OME==0,0.0001,OME),
                   item_type  = case_when(
                     trained_criterion == "Kcal" & seeding_Kcal == 1 ~ "seeding",
                     trained_criterion == "CO2"  & seeding_CO2  == 1 ~ "seeding",
                     TRUE                                            ~ "transfer"
                   )) %>% 
            group_by(ID,item_type) %>% 
            mutate(rank      = cor(criterion,judgment,method="spearman"),
                   rank_z    = z_fisher(r = rank)) %>% 
            select(ID,trained_criterion,est_criterion,match_domain,trial,ID_item:rt_ms,criterion,item_type,OME,OME_corr,sign_OME,rank,rank_z,age:SNS_3) %>% 
            ungroup()



# Filtering Trials                            --------------------------------------------------------


nrow(est) # 8640

est <- est %>% filter(judgment != 0)

nrow(est) # 8638

# Save tidy_df 
write_csv2(est,"Data/df_analysis.csv")


# Descriptives  - Table 1                     -------------------------------------------------------


(T1 <- est %>% 
  group_by(est_criterion,trained_criterion) %>% 
  summarise(across(OME_corr, list(mean = mean,
                                  sd   = sd),
                   .names = "{.col}.{fn}"),
            across(sign_OME, list(mean = mean,
                                  sd   = sd),
                   .names = "{.col}.{fn}"),
            across(rank,  list(mean   = mean,
                               sd     = sd),
                   .names = "{.col}.{fn}"),.groups = "drop") %>% 
  mutate_if(is.numeric,printnum,digits=2) %>% 
  mutate(OME       = paste0(OME_corr.mean," (",OME_corr.sd,")"),
         sign_OME  = paste0(sign_OME.mean," (",sign_OME.sd,")"),
         rank      = paste0(rank.mean," (",rank.sd,")")) %>% 
  select(est_criterion,trained_criterion,OME,sign_OME,rank))


# Latex Table
T1 %>% 
  kable(format    = "latex",digits=2,booktabs=TRUE,align="c",
        col.names = c("Estimated","Trained","OME","sign_OME","$\\rho$"),
        caption   = "Caption",
        escape    = FALSE) %>% 
  footnote(general           = "\\\\footnotesize\\{This is the note\\}",
           footnote_as_chunk = TRUE,
           threeparttable    = TRUE,
           escape            = FALSE,
           general_title     = "\\\\footnotesize\\{Note.\\}",
           title_format      = c("italic")) %>% 
  collapse_rows(columns = c(1,2), latex_hline = "major", valign = "middle")



# Descriptives  - Table 2                     -------------------------------------------------------


est %>% 
  filter(trained_criterion == est_criterion) %>% 
  group_by(est_criterion,item_type) %>% 
  summarise(mean = exp(mean(log(OME_corr))),
            sd   = exp(sd(log(OME_corr)))


(T2 <- est %>% 
          filter(trained_criterion == est_criterion) %>% 
          group_by(est_criterion,item_type) %>% 
          summarise(across(OME_corr, list(mean = mean,
                                          sd   = sd),
                           .names = "{.col}.{fn}"),
                    across(rank,  list(mean   = mean,
                                       sd     = sd),
                           .names = "{.col}.{fn}"),.groups = "drop") %>% 
          mutate_if(is.numeric,printnum,digits=2) %>% 
          mutate(OME       = paste0(OME_corr.mean," (",OME_corr.sd,")"),
                 rank      = paste0(rank.mean," (",rank.sd,")")) %>% 
          select(est_criterion,item_type,OME,rank))


# Latex Table
T2 %>% 
    kable(format    = "latex",digits=2,booktabs=TRUE,align="c",
          col.names = c("Estimated","Item Type","OME","$\\rho$"),
          caption   = "Caption",
          escape    = FALSE) %>% 
    footnote(general           = "\\\\footnotesize\\{This is the note\\}",
             footnote_as_chunk = TRUE,
             threeparttable    = TRUE,
             escape            = FALSE,
             general_title     = "\\\\footnotesize\\{Note.\\}",
             title_format      = c("italic")) %>% 
    collapse_rows(columns = c(1,2), latex_hline = "major", valign = "middle")








# Empirical Plot                              --------------------------------------------------------------------




est %>%
  filter(item_type     == "transfer") %>% 
  group_by(ID,est_criterion,trained_criterion) %>% 
  summarise(OME=mean(OME), .groups = "drop") %>% 
  mutate(est_criterion = ifelse(est_criterion == "CO2",
                                "bold(Estimated:~CO[2])",
                                "bold(Estimated:~kcal)"),
         trained_criterion = ifelse(trained_criterion == "CO2",
                                    "CO[2]",
                                    "kcal")) %>% 
  rename(`Estimated Criterion` = est_criterion,
         `Trained Criterion`   = trained_criterion) %>% 
  ggplot(.,aes(x = `Trained Criterion`, y = OME, group = `Estimated Criterion`  )) +
  geom_jitter(aes(color = `Trained Criterion`),size=3,width = 0.1,alpha=0.5, height = 0) + 
  stat_summary(fun.data = mean_se, geom = "errorbar",color="black",width=0.1) +
  stat_summary(fun="mean",geom="line",lwd=0.8,color="black") +
  stat_summary(fun="mean",geom="point",size=4,aes(fill = `Trained Criterion`),color="black",shape=21) +
  scale_fill_manual(values=c(clrs[4],clrs[3])) +
  scale_color_manual(values=c(clrs[4],clrs[3])) +
  facet_wrap(.~`Estimated Criterion`,labeller = label_parsed) +
  labs(fill  = "Trained Criterion", 
       y     = "OME",
       x     = "Trained Criterion") +
  scale_x_discrete(labels = parse_format()) +
  theme_nice() +
  theme(legend.position = "none") -> p_OME



est %>%
  filter(item_type     == "transfer") %>% 
  group_by(ID,est_criterion,trained_criterion) %>% 
  summarise(rank=mean(rank), .groups = "drop") %>% 
  mutate(est_criterion = ifelse(est_criterion == "CO2",
                                "bold(Estimated:~CO[2])",
                                "bold(Estimated:~kcal)"),
         trained_criterion = ifelse(trained_criterion == "CO2",
                                    "CO[2]",
                                    "kcal")) %>% 
  rename(`Estimated Criterion` = est_criterion,
         `Trained Criterion`   = trained_criterion) %>% 
  ggplot(.,aes(x = `Trained Criterion`, y = rank, group = `Estimated Criterion`  )) +
  geom_jitter(aes(color = `Trained Criterion`),size=3,width = 0.1,alpha=0.5, height = 0) + 
  stat_summary(fun.data = mean_se, geom = "errorbar",color="black",width=0.1) +
  stat_summary(fun="mean",geom="line",lwd=0.8,color="black") +
  stat_summary(fun="mean",geom="point",size=4,aes(fill = `Trained Criterion`),color="black",shape=21) +
  scale_fill_manual(values=c(clrs[4],clrs[3])) +
  scale_color_manual(values=c(clrs[4],clrs[3])) +
  facet_wrap(.~`Estimated Criterion`,labeller = label_parsed) +
  labs(fill  = "Trained Criterion", 
       y     = "Rank Correlation",
       x     = "Trained Criterion") +
  scale_x_discrete(labels = parse_format()) +
  theme_nice() +
  theme(legend.position = "none") -> p_rank




est %>% 
  mutate(est_criterion = ifelse(est_criterion == "CO2",
                                "bold(Estimated:~CO[2])",
                                "bold(Estimated:~kcal)"),
         trained_criterion = ifelse(trained_criterion == "CO2",
                                    "CO[2]",
                                    "kcal")) %>% 
  rename(`Estimated Criterion` = est_criterion,
         `Trained Criterion` = trained_criterion) %>% 
  ggplot(.,aes(x = sign_OME, fill = `Trained Criterion`)) +
  geom_histogram(bins=30,position = "identity",alpha=0.75,color="black") +
  scale_fill_manual(values=c("#414487FF","#22A884FF"),
                    labels = parse_format()) +
  geom_vline(xintercept = 0,linewidth=1,lty="dashed")+
  facet_wrap(.~`Estimated Criterion`,labeller = label_parsed) +
  labs(fill = "Trained Criterion", 
       y    = "Frequency",
       x    = "signed OME") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,900)) +
  theme_nice() +
  theme(legend.position = c(.15,.75)) -> p_signed




(p_signed/ p_OME  / p_rank) +
  plot_annotation(tag_levels="A") +
  plot_layout(heights = c(5, 5, 5))

ggsave("Results/Plots/main_res_obs.pdf",
       width=8,height=13,bg="white",dpi=300,device = cairo_pdf)






