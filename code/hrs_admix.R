library(ggplot2)
library(magrittr)
library(tidyverse)

# Run some linear regression on admixture levels on the HRS

#Required results:
# 1. Summary of model Native American ancestry by birth year
# 2. Scatterplot of Native American ancestry as a function of birth year in the Mexican Americans with the above trend line from the model
# 3. Range of Native American ancestry in the Mexican Americans in HRS. (my note: average by birth year?)

data_dir <- "/Volumes/Stockage/alex/hrs/aux"
img_dir <- "/Users/alex/Documents/projects/hrs/hispanic_admixture/images"

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

##### Native American ancestry as a function of birth year #####

lm_admix <- lm(ADMIX3 ~ BirthYear, data = hrs_data_mex)
summary(lm_admix)

ggplot(data = hrs_data_mex, aes(y = ADMIX3, x = BirthYear)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Birth Year") + ylab("Estimated admixture level") +
  ggtitle("Estimated Native American admixture level vs birth year (Self-identified Mexican-Americans, HRS)") +
  ggsave(paste(img_dir, "regression_admix_vs_birthyear.jpeg", sep = "/"))

##### Range of Native American ancestry #####
# Make a density plot of the admixture levels
# Throw in birth year too (maybe bin by five-year levels and do box plots?)

# Basic density -- look at the entire population
ggplot(data = hrs_data_mex, aes(x = ADMIX3)) +
  geom_density() +
  xlab("Estimated Native American admixture") +
  ggtitle("Density plot of estimated admixture levels (self-identified Mexican-Americans, HRS)") +
  ggsave(paste(img_dir, "density_mexam_admix_native.jpeg", sep = "/"))

# Use the age-range bins
ggplot(data = hrs_data_mex, aes(x = ADMIX3, group = AgeRange, fill = AgeRange)) +
  geom_density(alpha = 0.1)

# Hm. Not very clear. use something else.
# Get the mean admixture level for each age group
hrs_data_mex %>%
  select(AgeRange, ADMIX3) %>%
  group_by(AgeRange) %>%
  mutate(mean_admix = mean(ADMIX3)) -> mean_admix

ggplot(mean_admix, aes(ADMIX3)) + 
  geom_density(alpha = 0.1) +
  facet_wrap(~AgeRange, ncol = 1) +
  geom_vline(aes(xintercept = mean_admix)) +
  xlab("Estimated admixture level") +
  ggtitle("Densities of estimated Native American admixture by age range (self-identified Mexican-Americans, HRS)") +
  ggsave(paste(img_dir, "age_range_density.jpeg", sep = "/"))
