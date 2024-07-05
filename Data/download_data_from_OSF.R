# Downloads the data file from the OSF into the corresponding folder
# requires install.packages("osfr")

library(osfr)



# Download all .csv files from the Data component 
osf_download(osf_ls_files(osf_retrieve_node("y2rn8")),path = "Results")

# Download a specific file
osf_download(osf_retrieve_file("gva9p"),path = "Data") 
osf_download(osf_retrieve_file("7htym"),path = "Data") # df_analysis.csv (cleaned and filtered df)
