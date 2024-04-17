library(tidyverse)
library(dplyr)

# Import data
alldata <- read.delim("C:/Users/jankj/OneDrive/Desktop/masters_thesis/data/participants.tsv", na.strings = "n/a", header = TRUE)

# Subset of alldata that contains only the important variables
subset <- alldata %>%
  select(participant_id,nr, age, sex, group, graduation, years_of_education, neurological_diseases_1, facit_f_total_score, hads_a_total_score, hads_d_total_score, psqi_total_score, moca, pvt_reaction_time, nback_miss_1, nback_false_alarm_1 ,nback_miss_2 ,nback_false_alarm_2 ,tmt_a_time, tmt_b_time)

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
any(is.na(cog_subset))
# Missing values = TRUE
# Removing rows with missing values
# cog_subset_clean <- cog_subset[complete.cases(cog_subset), ]
cog_subset_clean <- cog_subset %>%
  drop_na()

# Summarize cleaned dataframe
cog_subset_clean %>%
  summary()

# Variables for which outliers are to be identified and winsorized
variables <- c("pvt_reaction_time","nback_miss_1","nback_miss_2","tmt_a_time","tmt_b_time")

clean_data <- cog_subset_clean
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
  box_plot <- boxplot(cog_subset_clean[[variable]])$out
  mtext(paste("Outliers for", variable, ":", paste(box_plot, collapse = ",")))
  
  # Identify rows containing outliers
  out_ind <- which(cog_subset_clean[[variable]] %in% c(box_plot))
  cat("Indices of outliers for", variable, ":", out_ind, "\n")
  cat("Rows with outliers for", variable, ":\n")
  print(cog_subset_clean[out_ind,])
  
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
  # Winsorize outliers
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

# Standardization
clean_data[, c("pvt_reaction_time","nback_miss_1","nback_miss_2","tmt_a_time","tmt_b_time")] = scale(clean_data[, c("pvt_reaction_time","nback_miss_1","nback_miss_2","tmt_a_time","tmt_b_time")])

save(clean_data, file = "clean_data.RData")

