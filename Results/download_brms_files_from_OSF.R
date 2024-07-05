# Downloads the brms model files from the OSF into the corresponding folder
# requires install.packages("osfr")

library(osfr)


# Download all results files from the Result Files component (might take a while)
osf_download(osf_ls_files(osf_retrieve_node("y2rn8")),path = "Results")

# Download a specific file: 
# e.g. fit_H1a_CO2_M1.rds
osf_download(osf_retrieve_file("5r2q3"),path = "Results")
