library(tidyverse)
library(boot)
library(ggridges)

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

##### Ancestry as a function of birth year #####
# Native American ancestry
lm_admix <- lm(ADMIX3 ~ BirthYear + BirthRegionName, data = hrs_data_mex)
summary(lm_admix)

# European ancestry
lm_admix_eur <- lm(ADMIX2 ~ BirthYear, data = hrs_data_mex)
summary(lm_admix_eur)

# African ancestry
lm_admix_afr <- lm(ADMIX1 ~ BirthYear, data = hrs_data_mex)
summary(lm_admix_afr)

# Drop the 4 most recent births (only 4 born 1970+)
hrs_data_mex_temp <- subset(hrs_data_mex, BirthYear < 1970 && as.character(Race_HRS) == "White")
lm_admix_trimmed <- lm(ADMIX3 ~ BirthYear, data = hrs_data_mex_temp)
summary(lm_admix_trimmed)

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

##### Do 1000 bootstrap replicates of Mex-Am Native ancestry by decade and make a ridgeline plot #####
# Can you do 1000 bootstrap sampling iterations for Native American ancestry for each decade of the
# HRS Mexican Americans and then plot the distribution of means in a ridgeline plot?

# Define a decade variable
min(hrs_data_mex$BirthYear)
max(hrs_data_mex$BirthYear)
hrs_data_mex$BirthDecade[hrs_data_mex$BirthYear < 1920] <- "1910-19"
hrs_data_mex$BirthDecade[1920 <= hrs_data_mex$BirthYear & hrs_data_mex$BirthYear < 1930] <- "1920-29"
hrs_data_mex$BirthDecade[1930 <= hrs_data_mex$BirthYear & hrs_data_mex$BirthYear < 1940] <- "1930-39"
hrs_data_mex$BirthDecade[1940 <= hrs_data_mex$BirthYear & hrs_data_mex$BirthYear < 1950] <- "1940-49"
hrs_data_mex$BirthDecade[1950 <= hrs_data_mex$BirthYear & hrs_data_mex$BirthYear < 1960] <- "1950-59"
hrs_data_mex$BirthDecade[1960 <= hrs_data_mex$BirthYear & hrs_data_mex$BirthYear < 1970] <- "1960-69"
hrs_data_mex$BirthDecade[1970 <= hrs_data_mex$BirthYear & hrs_data_mex$BirthYear < 1980] <- "1970-79"
hrs_data_mex$BirthDecade[1980 <= hrs_data_mex$BirthYear] <- "1980+"

# Convert to factors and re-order them
hrs_data_mex$BirthDecade <- as.factor(hrs_data_mex$BirthDecade)
print(levels(hrs_data_mex$BirthDecade))


# Look at how many people are in each decade
hrs_data_mex %>% group_by(BirthDecade) %>% summarise(no_rows = length(BirthDecade))
# Hm. 3 in 1970-80, and 1 in 1980+

# Define a mean function for the bootstrap
boot_mean <- function(orig, resample) {
  mean(orig[resample])
}

mean_bs <- list()
for (bd in unique(hrs_data_mex$BirthDecade)){
  print(bd)
  hrs_sub <- subset(hrs_data_mex, BirthDecade==bd)
  mean_bs[[bd]] <- boot(hrs_sub$ADMIX3, boot_mean, R = 1000)
}

rm(df_mean_bs)
rm(temp_dat)
rm(temp_dec)
# convert to long format
for (bd in names(mean_bs)){
  temp_dat <- mean_bs[[bd]]$t

  temp_df <- data.frame(temp_dat)
  temp_df$BirthDecade <- as.factor(bd)
  names(temp_df) <- c("ADMIX3","BirthDecade")
  
  if(exists("df_mean_bs") == F){
    df_mean_bs <- temp_df
  } else{
    df_mean_bs <-rbind(df_mean_bs, temp_df)
  }
}

# Re-order the decades from oldest to most recent
levels(df_mean_bs$BirthDecade)
df_mean_bs$BirthDecade <- factor(df_mean_bs$BirthDecade, levels(df_mean_bs$BirthDecade)[c(6:1,7,8)])

# Set up the ridgeline plot
ggplot(df_mean_bs, aes(x = ADMIX3, y = BirthDecade, group = BirthDecade)) +
  geom_density_ridges()

# Work within the self-identified white population
hrs_data_mex_white_pre70s <- subset(hrs_data_mex, BirthYear < 1970 &
                                      (as.character(Race_HRS) == "White"))
lm_admix_mex_white_pre70s <- lm(ADMIX3 ~ BirthYear, data = hrs_data_mex_white_pre70s)
summary(lm_admix_mex_white_pre70s)

hrs_data_mex_white <- subset(hrs_data_mex, as.character(Race_HRS) == "White")
lm_admix_mex_white <- lm(ADMIX3 ~ BirthYear, data = hrs_data_mex_white)
summary(lm_admix_mex_white)

# Drop the 4 youngest individuals
hrs_data_mex_pre70s <- subset(hrs_data_mex, BirthYear < 1970)
lm_admix_mex_pre70s <- lm(ADMIX3 ~ BirthYear, data = hrs_data_mex_pre70s)
summary(lm_admix_mex_pre70s)

##### Playing around with different data samples #####
hrs_data_mex_1920_1959 <- subset(hrs_data_mex, BirthYear < 1960 & BirthYear >= 1920)
lm_1920_1959 <- lm(ADMIX3 ~ BirthYear, hrs_data_mex_1920_1959)
summary(lm_1920_1959)

##### Bootstrap regresson coefficients #####
# Run a regression with 1000 bootstrap samples
# Give the distributions of the p-values and slopes

# Repeat 1000 times:
# 1. Draw a bootstrap sample
# 2. Run a regression model
# 3. Extract the values

# Define the linear model function for the bootstrap function
# It returns two numeric values: The slope and p-value of BirthYear
lm_func <- function(dset, i){
  d2 <- dset[i,]
  lm_admix_bs <- lm(ADMIX3 ~ BirthYear, data = d2)
  
  # Extract the coefficients of interest
  return(c(summary(lm_admix_bs)$coefficients[2,1],
           summary(lm_admix_bs)$coefficients[2,4]))
}

# Set the seed and run the bootstrap regression
# Run these steps together
x <- .Random.seed
ts <- Sys.time() # Create timestamp for managing seeds
ts_str <- as.character(as.numeric(ts))
boot_reg <- boot(data = hrs_data_mex,
                  statistic = lm_func,
                  R = 1000)

# Save the seed for reproducibility
code_dir <- "/Users/alex/Documents/projects/hrs/hispanic_admixture/code"
fname <- paste(code_dir, paste("bootstrap_regression_seed", ts_str, ".RData", sep = ""), sep = "/") 
save(x,
     file = fname)

# Convert results to a data frame
boot_reg_df <- data.frame(boot_reg$t)
names(boot_reg_df) <- c("slopes","pvals")

hist(slopes)
hist(pvals, breaks = 20)

mean_slope = round(mean(boot_reg_df$slopes),5)

# Plot a histogram of slopes
ggplot(boot_reg_df) +
  geom_histogram(aes(x = slopes), binwidth = 0.0001, colour = "black", fill = "white") +
  geom_vline(xintercept = mean_slope, linetype = "dashed", colour = "red") +
  geom_text(aes(x = mean_slope, y = 120,
                label = paste("Mean = ", mean_slope, sep = "")), data = data.frame(), hjust = -0.1) + 
  xlab("Estimated slope of admixture") + ylab("Frequency") +
  ggtitle("Bootstrap regression slopes (R = 1000)") +
  ggsave(paste(img_dir, "bootstrap_regression_slopes.jpeg", sep = "/"))

# Plot a histogram of p-values
ggplot(boot_reg_df) + 
  geom_histogram(aes(x = pvals), breaks = seq(0, 1, 0.05), colour = "black", fill = "white") +
  geom_vline(xintercept = 0.05, linetype = "dashed", colour = "red") +
  geom_text(aes(x = 0.05, y = 600, label = "p = 0.05"), data = data.frame(), hjust = -0.1) +
  xlab("p-value") + ylab("Frequency") +
  ggtitle("Bootstrap regression p-values of slopes (R = 1000)") +
  ggsave(paste(img_dir, "bootstrap_regression_pvals.jpeg", sep = "/"))

pval_pct <- length(which(boot_reg_df$pvals < 0.05))/length(boot_reg_df$pvals)

# Plot an empirical CDF
ggplot(boot_reg_df, aes(x = pvals)) +
  stat_ecdf(geom = "step", pad = FALSE) +
  geom_vline(xintercept = 0.05, linetype = "dashed", colour = "red") +
  geom_point(aes(x = 0.05, y = pval_pct), data = data.frame()) +
  geom_text(aes(x = 0.05, y = pval_pct), label = as.character(pval_pct), data = data.frame(), hjust = -0.15) +
  xlab("p-value") + ylab("Cumulative probability") +
  ggtitle("ECDF of bootstrap regression p-values (R = 1000)") +
  ggsave(paste(img_dir, "bootstrap_regressoin_pvals_ecdf.jpeg", sep = "/"))

# Regenerate results using the seed
# load()
# attr(boot_reg, "seed") <- x
#.Random.seed <- attr(boot_reg, "seed")
#boot_reg2 <- boot(data = hrs_data_mex,
#                  statistic = lm_func,
#                  R = 1000)
