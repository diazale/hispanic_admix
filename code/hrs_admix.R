# Run some linear regression on admixture levels on the HRS

data_dir <- "/Volumes/Stockage/alex/hrs/aux"
data_file <- "mexam.csv"
  
hrs_data_mex <- read.csv(paste(data_dir, data_file, sep = "/"))

# Format data
hrs_data_mex$IndID <- as.character(hrs_data_mex$IndID)
hrs_data_mex$FamID <- as.character(hrs_data_mex$FamID)
hrs_data_mex$BirthRegionNum <- as.factor(hrs_data_mex$BirthRegionNum)

# We're interested in the three admixture values
# ADMIX1 = AFR
# ADMIX2 = EUR
# ADMIX3 = Native and/or Asian

lm_admix <- lm(ADMIX3 ~ BirthYear, data = hrs_data_mex)
summary(lm_admix)

