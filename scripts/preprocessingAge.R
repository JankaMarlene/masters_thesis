library(tidyverse)
library(dplyr)

# Import data
alldata <- read.delim("C:/Users/jankj/OneDrive/Desktop/masters_thesis/data/participants.tsv", na.strings = "n/a", header = TRUE)

# Subset of alldata that contains only the important variables
subset <- alldata %>%
  select(participant_id,nr, age, sex, group, graduation, years_of_education, neurological_diseases_1, facit_f_FS, hads_a_total_score, hads_d_total_score, psqi_total_score, moca, pvt_reaction_time, nback_miss_1, nback_false_alarm_1 ,nback_miss_2 ,nback_false_alarm_2 ,tmt_a_time, tmt_b_time)

# Convert 'moca' variable to a binary variable based on a cutoff score of 25 
# 0 represents scores less than 26 (may) indicating cognitive impairment 
# 1 represents scores more than 25 (may) indicating no cognitive impairment 
subset <- subset %>%
  mutate(moca = ifelse(moca > 25, 1, 0))

# Subset with all relevant cognitive data
cog_subset <- subset %>%
  select(participant_id,nr, age, group, pvt_reaction_time, nback_miss_1, nback_false_alarm_1, nback_miss_2, nback_false_alarm_2, tmt_a_time, tmt_b_time)

# Summarize cog_subset
cog_subset %>%
  summary()

# Filter to get all "withPCS" rows
# Summary of "withPCS" group
cog_subset %>%
  filter(group == "withPCS") %>%
  summary(group == "withPCS")

# Filter to get all "withoutPCS" rows
# Summary of "withoutPCS" group
cog_subset %>%
  filter(group == "withoutPCS") %>%
  summary(group == "withoutPCS")

# Checking for missing values
any(is.na(subset))
# Missing values = TRUE
# Removing rows with missing values
# cog_subset_clean <- cog_subset[complete.cases(cog_subset), ]
subset<- subset %>%
  drop_na(pvt_reaction_time, nback_miss_1, nback_miss_2, tmt_a_time, tmt_b_time)

# Summarize cleaned dataframe
subset %>%
  summary()

# Variables for which outliers are to be identified and winsorized
variables <- c("pvt_reaction_time","nback_miss_1","nback_miss_2","tmt_a_time","tmt_b_time")

clean_data <- subset
# Function to winsorize a variable
winsorize_variable <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- IQR(x)
  
  # Winsorization
  x <- ifelse(x > q3 + 1.5 * iqr, q3 + 1.5 * iqr, x)
  x <- ifelse(x < q1 - 1.5 * iqr, q1 - 1.5 * iqr, x)
  
  x
}

# Loop over all variables
for (variable in variables) {
  # Detect outliers
  box_plot <- boxplot(subset[[variable]])$out
  mtext(paste("Outliers for", variable, ":", paste(box_plot, collapse = ",")))
  
  # Identify rows containing outliers
  out_ind <- which(subset[[variable]] %in% c(box_plot))
  cat("Indices of outliers for", variable, ":", out_ind, "\n")
  cat("Rows with outliers for", variable, ":\n")
  print(subset[out_ind,])
  
  # Winsorize the variable
  clean_data <- clean_data %>%
    mutate({{variable}} := winsorize_variable(!!sym(variable)))
}

# Check the clean dataset
print(clean_data)

# Loop over all variables
# for (variable in variables) {
  # Detect outliers
  # box_plot <- boxplot(cog_subset_clean[[variable]])$out
  # mtext(paste("Outliers for", variable, ":", paste(box_plot,
                                                   # collapse = ",")))
  # Identify rows containing outliers
  # out_ind <- which(cog_subset_clean[[variable]] %in% c(box_plot))
  # cat("Indices of outliers for", variable, ":", out_ind, "\n")
  # cat("Rows with outliers for", variable, ":\n")
  # print(cog_subset_clean[out_ind,])
  # Remove outliers
  # clean_data <- clean_data[!clean_data[[variable]] %in% box_plot, ]
# }

# Test correlation
clean_data |>
  select(pvt_reaction_time, nback_miss_1, nback_false_alarm_1, nback_miss_2, nback_false_alarm_2, tmt_a_time, tmt_b_time) |>
  cor(use = "pairwise.complete.obs") |>
  round(2)
# Scatter plot of tmt_a_time vs. tmt_b_time faceted by age
ggplot(clean_data, aes(x = tmt_a_time, y = tmt_b_time, color = age)) +
  geom_point()
# Scatter plot of nback_miss_1 vs. nback_miss_2 faceted by age
ggplot(clean_data, aes(x = nback_miss_1, y = nback_miss_2, color = age)) +
  geom_point()


# Function to create age groups
create_age_groups <- function(age) {
  if (age >= 18 & age <= 24) {
    return("18-24 Years")
  } else if (age >= 25 & age <= 34) {
    return("25-34 Years")
  } else if (age >= 35 & age <= 44) {
    return("35-44 Years")
  } else if (age >= 45 & age <= 54) {
    return("45-54 Years")
  } else if (age >= 55 & age <= 59) {
    return("55-59 Years")
  } else if (age >= 60 & age <= 64) {
    return("60-64 Years")
  } else if (age >= 65 & age <= 69) {
    return("65-69 Years")
  } else {
    return("70-80 Years")
  }
}

# Function to create age groups
#create_age_groups <- function(age) {
#  if (age >= 18 & age <= 49) {
#    return("18-49 Years")
#  } else if (age >= 50 & age <= 80) {
#    return("50-80 Years")
#  }
#}
# Create age groups
clean_data <- clean_data %>%
  mutate(age_group = sapply(age, create_age_groups))

# Calculate mean and standard deviation for each age group and variable
age_group_summary <- clean_data %>%
  group_by(age_group) %>%
  summarize(across(c("pvt_reaction_time", "nback_miss_1", "nback_miss_2", "tmt_a_time", "tmt_b_time"), 
                   list(mean = mean, sd = sd)))

# Function to calculate z-scores
calculate_z_scores <- function(x, mean, sd) {
  (x - mean) / sd
}

# Function to calculate z-scores for each individual based on age
calculate_z_scores_individual <- function(x, age, age_group_summary) {
  # Find the corresponding age group for each individual
  age_group <- sapply(age, create_age_groups)
  
  # Join the age group summary data to the individual data based on age group
  individual_data <- data.frame(x, age, age_group) %>%
    left_join(age_group_summary, by = "age_group")
  
  # Calculate z-scores for each variable using individual mean and standard deviation
  z_scores <- individual_data %>%
    mutate(
      z_pvt_reaction_time = calculate_z_scores(pvt_reaction_time, pvt_reaction_time_mean, pvt_reaction_time_sd),
      z_nback_miss_1 = calculate_z_scores(nback_miss_1, nback_miss_1_mean, nback_miss_1_sd),
      z_nback_miss_2 = calculate_z_scores(nback_miss_2, nback_miss_2_mean, nback_miss_2_sd),
      z_tmt_a_time = calculate_z_scores(tmt_a_time, tmt_a_time_mean, tmt_a_time_sd),
      z_tmt_b_time = calculate_z_scores(tmt_b_time, tmt_b_time_mean, tmt_b_time_sd)
    ) %>%
    select(starts_with("z_"))
  
  # Combine the z-scores with the original data
  x <- cbind(x, z_scores)
  
  return(x)
}

# Calculate z-scores for each individual based on age
clean_data <- calculate_z_scores_individual(clean_data, clean_data$age, age_group_summary)

save(clean_data, file = "clean_data.RData")





