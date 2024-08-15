# Load libraries

library(readr) # For importing csv
library(tidyverse) # For various - %%
library(modelsummary) # For data summary
library(effectsize) # For working out semi partial effect sizes for multiple linear regressions
library(interactions) # For interaction visualisation plots


# Import data

developmentalData <- read_csv("developmentalData.csv")

# Show first six rows of data

head(developmentalData)

# Create new table with variables needed for study

draft_rp_dev_data <- developmentalData %>%
  select(key, sex, age, eyes, group, IQ, relPow_delta_rCP, relPow_theta_rCP, relPow_alpha_rCP, relPow_beta_rCP, relPow_delta_lCP, relPow_theta_lCP, relPow_alpha_lCP, relPow_beta_lCP, relPow_delta_rOP, relPow_theta_rOP, relPow_alpha_rOP, relPow_beta_rOP, relPow_delta_lOP, relPow_theta_lOP, relPow_alpha_lOP, relPow_beta_lOP, relPow_delta_O, relPow_theta_O, relPow_alpha_O, relPow_beta_O)

### Dataset discovery

# Summary of variables in dataset

summary(draft_rp_dev_data)

# Remove any rows with NA values - any row with NA values to be removed

rp_dev_data <- na.omit(draft_rp_dev_data)

# See format of all variables

str(rp_dev_data)

# Count of female vs male

table(rp_dev_data$sex)

# Count of eyes state: open vs closed

table(rp_dev_data$eyes)

# count of number of subjects in each age group

table(rp_dev_data$age)

# plot of all ages 

hist(rp_dev_data$age)

# Probability density plots

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_alpha_lCP, na.rm = T), main = "Relative Power lCP Alpha")
plot(density(rp_dev_data$relPow_beta_lCP, na.rm = T), main = "Relative Power lCP Beta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_delta_lCP, na.rm = T), main = "Relative Power lCP Delta")
plot(density(rp_dev_data$relPow_theta_lCP, na.rm = T), main = "Relative Power lCP Theta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_alpha_rCP, na.rm = T), main = "Relative Power rCP Alpha")
plot(density(rp_dev_data$relPow_beta_rCP, na.rm = T), main = "Relative Power rCP Beta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_delta_rCP, na.rm = T), main = "Relative Power rCP Delta")
plot(density(rp_dev_data$relPow_theta_rCP, na.rm = T), main = "Relative Power rCP Theta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_alpha_O, na.rm = T), main = "Relative Power O Alpha")
plot(density(rp_dev_data$relPow_beta_O, na.rm = T), main = "Relative Power O Beta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_delta_O, na.rm = T), main = "Relative Power O Delta")
plot(density(rp_dev_data$relPow_theta_O, na.rm = T), main = "Relative Power O Theta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_alpha_lOP, na.rm = T), main = "Relative Power lOP Alpha")
plot(density(rp_dev_data$relPow_beta_lOP, na.rm = T), main = "Relative Power lOP Beta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_delta_lOP, na.rm = T), main = "Relative Power lOP Delta")
plot(density(rp_dev_data$relPow_theta_lOP, na.rm = T), main = "Relative Power lOP Theta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_alpha_rOP, na.rm = T), main = "Relative Power rOP Alpha")
plot(density(rp_dev_data$relPow_beta_rOP, na.rm = T), main = "Relative Power rOP Beta")

par(mfrow = c(1, 2))
plot(density(rp_dev_data$relPow_delta_rOP, na.rm = T), main = "Relative Power rOP Delta")
plot(density(rp_dev_data$relPow_theta_rOP, na.rm = T), main = "Relative Power rOP Theta")

# Find means, standard deviations and correlation coefficients for continuous variables

desc_stats_numv_table <- rp_dev_data[, !colnames(rp_dev_data) %in% c("sex", "eyes")]

desc_table <- datasummary(All(desc_stats_numv_table) ~ Mean + SD, data = rp_dev_data)

print(desc_table)

datasummary_correlation(rp_dev_data)

corr_tab <- datasummary_correlation(rp_dev_data, output = "data.frame")[, 2:3]

all_stats <- datasummary(All(rp_dev_data) ~ Mean + SD, data = rp_dev_data, add_columns = corr_tab)

print(all_stats)

## Linear regressions - sex and age as separate predictors

# Create a function to run linear model for each DV

sep_lm_function <- function(x) {
  x <- as.character(substitute(x))
  
  
  formula <- as.formula(paste(x, "~ sex + age + eyes + IQ"))
  
  linear_model <- lm(formula, data = rp_dev_data)
  
  summary(linear_model)
  
}

# Create a function to work out the semi partial effect sizes for each DV

sep_lm_function_eff_sizes <- function(x) {
  x <- as.character(substitute(x))
  
  
  formula <- as.formula(paste(x, "~ sex + age + eyes + IQ"))
  
  linear_model <- lm(formula, data = rp_dev_data)
  
  r2_semipartial(linear_model, alternative = "two.sided")
  
}

# Linear model for Relative power - left Centro Parietal - Alpha

sep_lm1 <- sep_lm_function(relPow_alpha_lCP)
print(sep_lm1)

# Semi partial effect sizes for Relative power - left Centro Parietal - Alpha

sep_lm_function_eff_sizes(relPow_alpha_lCP)

# Linear model for Relative power - left Centro Parietal - Beta

sep_lm2 <- sep_lm_function(relPow_beta_lCP)
print(sep_lm2)

# Semi partial effect sizes for Relative power - left Centro Parietal - Beta

sep_lm_function_eff_sizes(relPow_beta_lCP)

# Linear model for Relative power - left Centro Parietal - Delta

sep_lm3 <- sep_lm_function(relPow_delta_lCP)
print(sep_lm3)

# Semi partial effect sizes for Relative power - left Centro Parietal - Delta

sep_lm_function_eff_sizes(relPow_delta_lCP)

# Linear model for Relative power - left Centro Parietal - Theta

sep_lm4 <- sep_lm_function(relPow_theta_lCP)
print(sep_lm4)

# Semi partial effect sizes for Relative power - left Centro Parietal - Theta

sep_lm_function_eff_sizes(relPow_theta_lCP)

# Linear model for Relative power - right Centro Parietal - Alpha

sep_lm5 <- sep_lm_function(relPow_alpha_rCP)
print(sep_lm5)

# Semi partial effect sizes for Relative power - right Centro Parietal - Alpha

sep_lm_function_eff_sizes(relPow_alpha_rCP)

# Linear model for Relative power - right Centro Parietal - Beta

sep_lm6 <- sep_lm_function(relPow_beta_rCP)
print(sep_lm6)

# Semi partial effect sizes for Relative power - right Centro Parietal - Beta

sep_lm_function_eff_sizes(relPow_beta_rCP)

# Linear model for Relative power - right Centro Parietal - Delta

sep_lm7 <- sep_lm_function(relPow_delta_rCP)
print(sep_lm7)

# Semi partial effect sizes for Relative power - right Centro Parietal - Delta

sep_lm_function_eff_sizes(relPow_delta_rCP)

# Linear model for Relative power - right Centro Parietal - Theta

sep_lm8 <- sep_lm_function(relPow_theta_rCP)
print(sep_lm8)

# Semi partial effect sizes for Relative power - right Centro Parietal - Theta

sep_lm_function_eff_sizes(relPow_theta_rCP)

# Linear model for Relative power - Occipital - Alpha

sep_lm9 <- sep_lm_function(relPow_alpha_O)
print(sep_lm9)

# Semi partial effect sizes for Relative power - Occipital - Alpha

sep_lm_function_eff_sizes(relPow_alpha_O)

# Linear model for Relative power - Occipital - Beta

sep_lm10 <- sep_lm_function(relPow_beta_O)
print(sep_lm10)

# Semi partial effect sizes for Relative power - Occipital - Beta

sep_lm_function_eff_sizes(relPow_beta_O)

# Linear model for Relative power - Occipital - Delta

sep_lm11 <- sep_lm_function(relPow_delta_O)
print(sep_lm11)

# Semi partial effect sizes for Relative power - Occipital - Delta

sep_lm_function_eff_sizes(relPow_delta_O)

# Linear model for Relative power - Occipital - Theta

sep_lm12 <- sep_lm_function(relPow_theta_O)
print(sep_lm12)

# Semi partial effect sizes for Relative power - Occipital - Theta

sep_lm_function_eff_sizes(relPow_theta_O)

# Linear model for Relative power - left Occipito Parietal - Alpha

sep_lm13 <- sep_lm_function(relPow_alpha_lOP)
print(sep_lm13)

# Semi partial effect sizes for Relative power - left Occipito Parietal - Alpha

sep_lm_function_eff_sizes(relPow_alpha_lOP)

# Linear model for Relative power - left Occipito Parietal - Beta

sep_lm14 <- sep_lm_function(relPow_beta_lOP)
print(sep_lm14)

# Semi partial effect sizes for Relative power - left Occipito Parietal - Beta

sep_lm_function_eff_sizes(relPow_beta_lOP)

# Linear model for Relative power - left Occipito Parietal - Delta

sep_lm15 <- sep_lm_function(relPow_delta_lOP)
print(sep_lm15)

# Semi partial effect sizes for Relative power - left Occipito Parietal - Delta

sep_lm_function_eff_sizes(relPow_delta_lOP)

# Linear model for Relative power - left Occipito Parietal - Theta

sep_lm16 <- sep_lm_function(relPow_theta_lOP)
print(sep_lm16)

# Semi partial effect sizes for Relative power - left Occipito Parietal - Theta

sep_lm_function_eff_sizes(relPow_theta_lOP)

# Linear model for Relative power - right Occipito Parietal - Alpha

sep_lm17 <- sep_lm_function(relPow_alpha_rOP)
print(sep_lm17)

# Semi partial effect sizes for Relative power - right Occipito Parietal - Alpha

sep_lm_function_eff_sizes(relPow_alpha_rOP)

# Linear model for Relative power - right Occipito Parietal - Beta

sep_lm18 <- sep_lm_function(relPow_beta_rOP)
print(sep_lm18)

# Semi partial effect sizes for Relative power - right Occipito Parietal - Beta

sep_lm_function_eff_sizes(relPow_beta_rOP)

# Linear model for Relative power - right Occipito Parietal - Delta

sep_lm19 <- sep_lm_function(relPow_delta_rOP)
print(sep_lm19)

# Semi partial effect sizes for Relative power - right Occipito Parietal - Delta

sep_lm_function_eff_sizes(relPow_delta_rOP)

# Linear model for Relative power - right Occipito Parietal - Theta

sep_lm20 <- sep_lm_function(relPow_theta_rOP)
print(sep_lm20)

# Semi partial effect sizes for Relative power - right Occipito Parietal - Theta

sep_lm_function_eff_sizes(relPow_theta_rOP)


## Linear regressions - sex and age as interactive predictors

# Create a function to run linear model for each DV

int_lm_function <- function(x) {
  x <- as.character(substitute(x))
  
  
  formula <- as.formula(paste(x, "~ sex * age + eyes + IQ"))
  
  linear_model <- lm(formula, data = rp_dev_data)
  
  summary(linear_model)
  
}

# Create a function to work out the semi partial effect sizes for each DV

int_lm_function_eff_sizes <- function(x) {
  x <- as.character(substitute(x))
  
  
  formula <- as.formula(paste(x, "~ sex + age + eyes + IQ"))
  
  linear_model <- lm(formula, data = rp_dev_data)
  
  r2_semipartial(linear_model, alternative = "two.sided")
  
}

# Relative power - left Centro Parietal - Alpha

int_lm1 <- int_lm_function(relPow_alpha_lCP)
print(int_lm1)

# Semi partial effect sizes for Relative power - left Centro Parietal - Alpha

int_lm_function_eff_sizes(relPow_alpha_lCP)

# Relative power - left Centro Parietal - Beta

int_lm2 <- int_lm_function(relPow_beta_lCP)
print(int_lm2)

# Semi partial effect sizes for Relative power - left Centro Parietal - Beta

int_lm_function_eff_sizes(relPow_beta_lCP)

# Relative power - left Centro Parietal - Delta

int_lm3 <- int_lm_function(relPow_delta_lCP)
print(int_lm3)

# Semi partial effect sizes for Relative power - left Centro Parietal - Delta

int_lm_function_eff_sizes(relPow_delta_lCP)

# Relative power - left Centro Parietal - Theta

int_lm4 <- int_lm_function(relPow_theta_lCP)
print(int_lm4)

# Semi partial effect sizes for Relative power - left Centro Parietal - Theta

int_lm_function_eff_sizes(relPow_theta_lCP)

# Relative power - right Centro Parietal - Alpha

int_lm5 <- int_lm_function(relPow_alpha_rCP)
print(int_lm5)

# Semi partial effect sizes for Relative power - right Centro Parietal - Alpha

int_lm_function_eff_sizes(relPow_alpha_rCP)

# Relative power - right Centro Parietal - Beta

int_lm6 <- int_lm_function(relPow_beta_rCP)
print(int_lm6)

# Semi partial effect sizes for Relative power - right Centro Parietal - Beta

int_lm_function_eff_sizes(relPow_beta_rCP)

# Relative power - right Centro Parietal - Delta

int_lm7 <- int_lm_function(relPow_delta_rCP)
print(int_lm7)

# Semi partial effect sizes for Relative power - right Centro Parietal - Delta

int_lm_function_eff_sizes(relPow_delta_rCP)

# Relative power - right Centro Parietal - Theta

int_lm8 <- int_lm_function(relPow_theta_rCP)
print(int_lm8)

# Semi partial effect sizes for Relative power - right Centro Parietal - Theta

int_lm_function_eff_sizes(relPow_theta_rCP)

# Relative power - Occipital - Alpha

int_lm9 <- int_lm_function(relPow_alpha_O)
print(int_lm9)

# Semi partial effect sizes for Relative power - Occipital - Alpha

int_lm_function_eff_sizes(relPow_alpha_O)

# Relative power - Occipital - Beta

int_lm10 <- int_lm_function(relPow_beta_O)
print(int_lm10)

# Semi partial effect sizes for Relative power - Occipital - Beta

int_lm_function_eff_sizes(relPow_beta_O)

# Relative power - Occipital - Delta

int_lm11 <- int_lm_function(relPow_delta_O)
print(int_lm11)

# Semi partial effect sizes for Relative power - Occipital - Delta

int_lm_function_eff_sizes(relPow_delta_O)

# Relative power - Occipital - Theta

int_lm12 <- int_lm_function(relPow_theta_O)
print(int_lm12)

# Semi partial effect sizes for Relative power - Occipital - Theta

int_lm_function_eff_sizes(relPow_theta_O)

# Relative power - left Occipito Parietal - Alpha

int_lm13 <- int_lm_function(relPow_alpha_lOP)
print(int_lm13)

# Semi partial effect sizes for Relative power - left Occipito Parietal - Alpha

int_lm_function_eff_sizes(relPow_alpha_lOP)

# Relative power - left Occipito Parietal - Beta

int_lm14 <- int_lm_function(relPow_beta_lOP)
print(int_lm14)

# Semi partial effect sizes for Relative power - left Occipito Parietal - Beta

int_lm_function_eff_sizes(relPow_beta_lOP)

# Relative power - left Occipito Parietal - Delta

int_lm15 <- int_lm_function(relPow_delta_lOP)
print(int_lm15)

# Semi partial effect sizes for Relative power - left Occipito Parietal - Delta

int_lm_function_eff_sizes(relPow_delta_lOP)

# Relative power - left Occipito Parietal - Theta

int_lm16 <- int_lm_function(relPow_theta_lOP)
print(int_lm16)

# Semi partial effect sizes for Relative power - left Occipito Parietal - Theta

int_lm_function_eff_sizes(relPow_theta_lOP)

# Relative power - right Occipito Parietal - Alpha

int_lm17 <- int_lm_function(relPow_alpha_rCP)
print(int_lm17)

# Semi partial effect sizes for Relative power - right Occipito Parietal - Alpha

int_lm_function_eff_sizes(relPow_alpha_rOP)

# Relative power - right Occipito Parietal - Beta

int_lm18 <- int_lm_function(relPow_beta_rOP)
print(int_lm18)

# Semi partial effect sizes for Relative power - right Occipito Parietal - Beta

int_lm_function_eff_sizes(relPow_beta_rOP)

# Relative power - right Occipito Parietal - Delta

int_lm19 <- int_lm_function(relPow_delta_rOP)
print(int_lm19)

# Semi partial effect sizes for Relative power - right Occipito Parietal - Delta

int_lm_function_eff_sizes(relPow_delta_rOP)

# Relative power - right Occipito Parietal - Theta

int_lm20 <- int_lm_function(relPow_theta_rOP)
print(int_lm20)

# Semi partial effect sizes for Relative power - right Occipito Parietal - Theta

int_lm_function_eff_sizes(relPow_theta_rOP)

# ----------------------------

## Plots to visualise the interaction between sex and age on the relative power of beta, across the left Centro Parietal, right Centro Parietal, Occipital, left Occipito Parietal and right Occipito Parietal regions

# Plot to visualise interaction for left Centro Parietal region

int_lm2_forplot <- lm(relPow_beta_lCP ~ sex * age + eyes + IQ, data = rp_dev_data)
interact_plot(int_lm2_forplot, pred = age, modx = sex, plot.points = T)

# Plot to visualise interaction for right Centro Parietal region

int_lm6_forplot <- lm(relPow_beta_rCP ~ sex * age + eyes + IQ, data = rp_dev_data)
interact_plot(int_lm6_forplot, pred = age, modx = sex, plot.points = T)

# Plot to visualise interaction for Occipital region

int_lm10_forplot <- lm(relPow_beta_O ~ sex * age + eyes + IQ, data = rp_dev_data)
interact_plot(int_lm10_forplot, pred = age, modx = sex, plot.points = T)

# Plot to visualise interaction for left Occipito Parietal region

int_lm14_forplot <- lm(relPow_beta_lOP ~ sex * age + eyes + IQ, data = rp_dev_data)
interact_plot(int_lm14_forplot, pred = age, modx = sex, plot.points = T)

# Plot to visualise interaction for right Occipito Parietal region

int_lm18_forplot <- lm(relPow_beta_rOP ~ sex * age + eyes + IQ, data = rp_dev_data)
interact_plot(int_lm18_forplot, pred = age, modx = sex, plot.points = T)

