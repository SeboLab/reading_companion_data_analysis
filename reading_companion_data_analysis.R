# clear the workspace
rm(list=ls()) 
cat("\014")  

# load packages
library(dplyr) 
library(tidyverse) 
library(ggpubr) 
library(rstatix)

# specify the directory that the csv file is in and the csv file name 
rc_data_rm <- read.csv("/Users/user/Documents/R/reading_companion.csv", header=T) 

# specify the factors for the data
rc_data_rm$condition_0H_1R <- factor(
  rc_data_rm$condition_0H_1R,
  levels = 0:1,
  labels = c("human", "robot")
) 
rc_data_rm$first_condition_0H_1R <- factor(
  rc_data_rm$first_condition_0H_1R,
  levels = 0:1,
  labels = c("human first", "robot first")
)
rc_data_rm$session_number <- factor(
  rc_data_rm$session_number,
  levels = 1:2,
  labels = c("first", "second")
)


####################### Comprehension ##########################################

# Outliers
rc_data_rm %>%
  group_by(condition_0H_1R) %>%
  identify_outliers(total_comp)

# Mean and std
rc_data_rm %>%
  group_by(condition_0H_1R) %>%
  get_summary_stats(total_comp, type = "mean_sd")

# Boxplot
bxp <- ggboxplot(
  rc_data_rm, x = "condition_0H_1R", y = "total_comp",
  color = "condition_0H_1R", palette = "jco"
)
bxp

# ANOVA
total_comp_aov <- aov(total_comp ~ condition_0H_1R * session_number + age +
                           Error(pid / (condition_0H_1R * session_number)),
                         data=rc_data_rm)
summary(total_comp_aov)


####################### Jitter ##########################################
# Outliers
jitter_outliers <- rc_data_rm %>%
  group_by(condition_0H_1R) %>%
  identify_outliers("jitter_delta")

# Mean and std
rc_data_rm %>%
  group_by(condition_0H_1R) %>%
  get_summary_stats(jitter_delta, type = "mean_sd")

# Boxplot
bxp <- ggboxplot(
  rc_data_rm, x = "condition_0H_1R", y = "jitter_delta",
  color = "condition_0H_1R", palette = "jco"
)
bxp

# ANOVA
jitter_aov <- aov(jitter_delta ~ condition_0H_1R * session_number + age +
                    Error(pid / (condition_0H_1R * session_number)),
                  data=rc_data_rm)
summary(jitter_aov)

####################### HRV LF/HF ##########################################

# Outliers
rc_data_rm %>%
  group_by(condition_0H_1R) %>%
  identify_outliers(hrv_delta)

# Mean and std
rc_data_rm %>%
  group_by(condition_0H_1R) %>%
  get_summary_stats(hrv_delta, type = "mean_sd")

# Boxplot
bxp <- ggboxplot(
  rc_data_rm, x = "condition_0H_1R", y = "hrv_delta",
  color = "condition_0H_1R", palette = "jco"
)
bxp

#ANOVA
HRV_aov <- aov(hrv_delta ~ condition_0H_1R * session_number + age +
                      Error(pid / (condition_0H_1R * session_number)),
                    data=rc_data_rm)
summary(HRV_aov)

####################### Thermal ##########################################

# Only include in this analysis rows where the include column is 1 
# Exluded data is due to occlusion, preventing thermal calculation
rc_data_rm_thermal <- rc_data_rm[which(rc_data_rm$exclude==0), ]

# Outliers
outliers <- rc_data_rm_thermal %>%
  group_by(condition_0H_1R) %>%
  identify_outliers(thermal_delta)

# Mean and std
rc_data_rm_thermal %>%
  group_by(condition_0H_1R) %>%
  get_summary_stats(thermal_delta, type = "mean_sd")

# Boxplot
bxp <- ggboxplot(
  rc_data_rm_thermal, x = "condition_0H_1R", y = "thermal_delta",
  color = "condition_0H_1R", palette = "jco"
)
bxp

# ANOVA
thermal_aov <- aov(thermal_delta ~ condition_0H_1R * session_number + age +
                           Error(pid / (condition_0H_1R * session_number)),
                         data=rc_data_rm_thermal)
summary(thermal_aov)
