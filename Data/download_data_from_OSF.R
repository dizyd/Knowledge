# Downloads the data file from the OSF into the corresponding folder
# requires install.packages("osfr)

osfr::osf_download(osf_retrieve_file("9tn4e"),path = "Data")
 