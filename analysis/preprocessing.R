# Installing tidyverse package
# install.packages("tidyverse")
library(tidyverse)

alldata <- read.delim("C:/Users/jankj/OneDrive/Desktop/masters_thesis/data/participants.tsv", na.strings = "n/a", header = TRUE)

# List off important column

important_columns <- c("participant_id","age","sex","group","graduation","years_of_education","neurological_diseases_1","facit_f_total_score","hads_a_total_score","hads_d_total_score","psqi_total_score","moca","pvt_reaction_time","nback_miss_1","nback_false_alarm_1","nback_miss_2","nback_false_alarm_2","tmt_a_time","tmt_b_time")

# Subset of alldata that contains only the important columns

subset <- alldata[,important_columns]

# Subset with all relevant cognitive data

cognitive_columns <- c("participant_id","moca","pvt_reaction_time","nback_miss_1","nback_false_alarm_1","nback_miss_2","nback_false_alarm_2","tmt_a_time","tmt_b_time")
cog_subset <- alldata[,cognitive_columns]

# Convert 'moca' variable to a binary variable based on a cutoff score of 25 
# 0 represents scores less than 26 (may) indicating cognitive impairment 
# 1 represents scores more than 25 (may) indicating no cognitive impairment 
# Define cutoff score for moca 
# Overwrite "moca" variable with binary scaled values

cutoff_score <- 25
cog_subset$moca <- ifelse(cog_subset$moca > cutoff_score, 1, 0)

# Checking for missing values
any(is.na(cog_subset))
# Missing values = TRUE
# Removing rows with missing values
cog_subset_clean <- cog_subset[complete.cases(cog_subset), ]
# Imputation of missing values
cog_subset_imputed <- na.mean(cog_subset)
# Compute correlation matrix
## cor(cog_subset)
