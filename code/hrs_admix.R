library(ggplot2)

# Run some linear regression on admixture levels on the HRS

#Required results:
# 1. Summary of model Native American ancestry by birth year
# 2. Scatterplot of Native American ancestry as a function of birth year in the Mexican Americans with the above trend line from the model
# 3. Range of Native American ancestry in the Mexican Americans in HRS. (my note: average by birth year?)

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
# ADMIX3 = Native American / Asian

lm_admix <- lm(ADMIX3 ~ BirthYear, data = hrs_data_mex)
summary(lm_admix)

ggplot(data = hrs_data_mex, aes(y = ADMIX3, x = BirthYear)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Birth Year") + ylab("Estimated admixture level") +
  ggtitle("Estimated admixture level vs birth year (Self-identified Mexican-Americans, HRS)")
