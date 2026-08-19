# \#Knowledge: Improving food-related knowledge via seeding implemented as a social media intervention

## About this repository
This repository contains the data, materials, and the code to reproduce all analyses and figures in the project *\#Knowledge: Improving food-related knowledge via seeding implemented as a social media intervention*. The preregistration and the `brms` model files from the analysis can be found on the respective OSF: https://osf.io/vztjq/. The structure of this repository is as follows:

<br>

- **Scripts/** holds code relevant for the paper. Specifically:
    - `Scripts/functions.R` contains code of useful R helper functions, e.g., for analysis, plotting etc.
    - `Scripts/analysis.R` contains code for the initial data preparation steps, including filtering of participants and trials, calculating descriptive statistics (Table 1 and 2 in the manscuript), and making the main results figure (Figure 3).
    - `Scripts/analysis_Hypothesis1.R` contains the code to run the analysis for Hypothesis 1.
    - `Scripts/analysis_Hypothesis2.R` contains the code to run the analysis for Hypothesis 2.
    - `Scripts/analysis_Hypothesis3.R` contains the code to run the analysis for Hypothesis 3.
    - `Scripts/analysis_Hypothesis1_sensitivity.R` contains the code to run the analysis for Hypothesis 1 with more skeptical priors.
    - `Scripts/analysis_Hypothesis2_sensitivity.R` contains the code to run the analysis for Hypothesis 2 with more skeptical priors.
    - `Scripts/analysis_Hypothesis3_sensitivity.R` contains the code to run the analysis for Hypothesis 3 with more skeptical priors.
    - `Scripts/aanalysis_Hypothesis1_seedingItems.R` contains the code to run the analysis for Hypothesis 1 but only for the seeding items as reported in the supplementary materials (see [here](https://dizyd.github.io/Knowledge/supplement.html#seeding-effects-on-direct-learning)).
    - `Scripts/appendix_A1.R` contains  the code to produce the Table A1.
    - `Scripts/appendix_A2.R` contains  the code to produce the Figure B1.
    - `Scripts/analysis_compute_standardized_effect_sizes.R` contains the code to compute the standardizeded effect size reported in the main text (for more information see also the supplementary materials [here](https://dizyd.github.io/Knowledge/supplement.html#calculation-of-standardized-effect-sizes)).

<br>

- **Data/** contains the data files. Specifically:
    - `Data/data_insta_seeding.csv` which contains the full data.
    - `Data/df_analysis.csv` which contains the filtered and cleaned data used for all analysis.

<br>
    
- **Results/** contains the script `download_brms_files_from_OSF.R` which downloads all `brms` model files from the OSF (Caution: This might take a few minutes).

<br>

- **Materials/** contains all *seeding* fact and *trivia* fact images used during the 15 day seeding phase.

<br>

- **Plots/** includes all figures in the manuscript. 

<br>


This work, including all figures, is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.  All code is licensed under the MIT License.

## Contributing Authors
David Izydorczyk, Barbara Kreis, Michael Kilb & Arndt Bröder

## Abstract

Research has shown that a simple procedure called seeding can improve estimation accuracy for real-world quantities. However, this effect has only been demonstrated in controlled experimental studies. This study investigates whether a seeding procedure implemented via Instagram can serve as a large-scale intervention to boost knowledge. Participants were randomly assigned to follow one of two Instagram accounts posting information about either the calorie content or the CO~2~ footprint of 15 selected food items over 15 days. Afterward, participants (*N* = 144) estimated the calorie content or CO2 footprint for 60 foods. Results
showed that seeding improved participants’ metric knowledge (e.g., mean, range), especially for the CO~2~ footprint, where initial knowledge was lower. However, effects on mapping knowledge (i.e., relative ordering) were limited. These findings suggest that seeding through social media could be a viable large-scale educational tool, complementing other systemic interventions, particularly in areas where initial knowledge is limited. Keywords: real-world estimation, numerical estimation, calorie content, carbon footprints, seeding

## Publication
Izydorczyk, D., Kreis, B.K, Kilb, M., & Bröder, A. (in press). \#Knowledge: Improving food-related knowledge via seeding implemented as a social media intervention. *Collabra*

## Funding
This research was funded by Grant IZ 96/1-1 provided to David Izydorczyk from the German Research Foundation (DFG) and supported by the University of Mannheim’s Graduate School of Economic and Social Sciences.


