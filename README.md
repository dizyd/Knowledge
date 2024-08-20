# \#Knowledge: Improving food-related knowledge via seeding implemented as a social media intervention


This repository contains code to reproduce all analyses and figures in the project *\#Knowledge: Improving food-related knowledge via seeding implemented as a social media intervention*. **The data corresponding to this project can be found on the respective OSF: https://osf.io/vztjq/**. The structure of this repository is as follows:

<br>

- **Scripts/** holds code relevant for the paper. Specifically:
    - `Scripts/functions.R` has code for useful R helper functions for analysis and plotting.
    - `Scripts/analysis.R` contains code for the initial data preparation step, including filtering participants and trials, calculating descriptive statistics (Table 1 and 2 in manscuript), and making the main results figure.
    - `Scripts/analysis_Hypothesis1.R` has code run the analysis for Hypothesis 1.
    - `Scripts/analysis_Hypothesis2.R` has code run the analysis for Hypothesis 2.
    - `Scripts/analysis_Hypothesis3.R` has code run the analysis for Hypothesis 3.
    - `Scripts/appendix_A1.R` has code to produce the Table A1.
    - `Scripts/appendix_A2.R` has code to produce the Figure B1.

<br>

- **Data/** contains the script `download_data_from_OSF.R` which downloads the data relevant for the paper from the OSF. Specifically:
    - `Data/data_insta_seeding.csv` which contains the full data.
    - `Data/df_analysis.csv` which contains the filtered and cleaned data used for all analysis.

<br>
    
- **Results/** contains the script `download_brms_files_from_OSF.R` which downloads all 36 `brms` model files from the OSF.

<br>

- **Materials/** contains all *seeding* fact and *trivia* fact images used during the 15 day seeding phase.

<br>

- **Plots/** includes all figures in the manuscript. 

<br>


This work, including all figures, is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.  All code is licensed under the MIT License.

## Contributing Authors
David Izydorczyk, Barbara Kreis, Michael Kilb & Arndt Bröder

## Abstract
Will be added soon.

## Publication
(work in progress)

## Funding
This research was funded by Grant IZ 96/1-1 provided to David Izydorczyk from the German Research Foundation (DFG) and supported by the University of Mannheim’s Graduate School of Economic and Social Sciences.


