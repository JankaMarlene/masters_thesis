# Installing tidyverse package
# install.packages("tidyverse")
# Import the tidyverse
library(tidyverse)

# Import data
alldata <- read.delim("C:/Users/jankj/OneDrive/Desktop/masters_thesis/data/participants.tsv", na.strings = "n/a", header = TRUE)

# Subset of alldata that contains only the important variables
subset <- alldata %>%
  select(participant_id, age, sex, group, graduation, years_of_education, neurological_diseases_1, facit_f_total_score, hads_a_total_score, hads_d_total_score, psqi_total_score, moca, pvt_reaction_time, nback_miss_1, nback_false_alarm_1 ,nback_miss_2 ,nback_false_alarm_2 ,tmt_a_time, tmt_b_time)

# important_columns <- c("participant_id","age","sex","group","graduation","years_of_education","neurological_diseases_1","facit_f_total_score","hads_a_total_score","hads_d_total_score","psqi_total_score","moca","pvt_reaction_time","nback_miss_1","nback_false_alarm_1","nback_miss_2","nback_false_alarm_2","tmt_a_time","tmt_b_time")
# subset <- alldata[,important_columns]

# Subset with all relevant cognitive data
cog_subset <- alldata %>%
  select(participant_id, age, group, moca, pvt_reaction_time, nback_miss_1, nback_false_alarm_1, nback_miss_2, nback_false_alarm_2, tmt_a_time, tmt_b_time)

# Convert 'moca' variable to a binary variable based on a cutoff score of 25 
# 0 represents scores less than 26 (may) indicating cognitive impairment 
# 1 represents scores more than 25 (may) indicating no cognitive impairment 
cog_subset <- cog_subset %>%
  mutate(moca = ifelse(moca > 25, 1, 0))

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
  
# Imputation of missing values
# Funktioniert nicht, da z.B. beim n-back Werte nicht zwischen 0 und 1 annehmen kann. Auch mit round hat er Probleme
# cog_subset_impute <- cog_subset %>%
#  mutate_all(~ replace_na(., mean(.,na.rm = TRUE)))

# Detect outliers
selected_variables <- cog_subset_clean[, c("moca","pvt_reaction_time","nback_miss_1","nback_false_alarm_1","nback_miss_2","nback_false_alarm_2","tmt_a_time","tmt_b_time")]
boxplot(selected_variables, main = "Boxplot of cog_subset_clean")

# Compute correlation matrix
## cor(cog_subset)

# Scatter plot of tmt_a_time vs. tmt_b_time
ggplot(cog_subset, aes(x = tmt_a_time, y = tmt_b_time)) +
  geom_point()
# Scatter plot of tmt_a_time vs. tmt_b_time faceted by age
ggplot(cog_subset, aes(x = tmt_a_time, y = tmt_b_time, color = age)) +
  geom_point()
# Scatter plot of nback_miss_1 vs. nback_miss_2 faceted by age
ggplot(cog_subset, aes(x = nback_miss_1, y = nback_miss_2, color = age)) +
  geom_point()

# Clustering

# Test correlation
library(dplyr)
cog_subset_clean |>
  select(moca, pvt_reaction_time, nback_miss_1, nback_false_alarm_1, nback_miss_2, nback_false_alarm_2, tmt_a_time, tmt_b_time) |>
  cor(use = "pairwise.complete.obs") |>
  round(2)

# Standardization
cog_subset_clean[, c("moca","pvt_reaction_time","nback_miss_1","nback_false_alarm_1","nback_miss_2","nback_false_alarm_2","tmt_a_time","tmt_b_time")] = scale(cog_subset_clean[, c("moca","pvt_reaction_time","nback_miss_1","nback_false_alarm_1","nback_miss_2","nback_false_alarm_2","tmt_a_time","tmt_b_time")])

# Get columns of interest
cog_subset_clean_cog <- cog_subset_clean[, c("moca","pvt_reaction_time","nback_miss_1","nback_false_alarm_1","nback_miss_2","nback_false_alarm_2","tmt_a_time","tmt_b_time")]

# Hierarchical clustering
cog_df <- cog_subset_clean[, c("group","moca","pvt_reaction_time","nback_miss_1","nback_false_alarm_1","nback_miss_2","nback_false_alarm_2","tmt_a_time","tmt_b_time")]
str(cog_df)
summary(cog_df)
cog_label <- cog_df$group
cog_df$group <- NULL
str(cog_df)


# Outliers
# boxplot(cog_subset_clean_cog, main = "Boxplot of cog_subset_clean_cog")

set.seed(123)
km.out <- kmeans(cog_subset_clean_cog, centers = 2, nstart = 20)
km.out

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(cog_subset_clean_cog, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a screen plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',3),'#FF0000', rep('#000000', 6))
  )

# Select number of clusters
k <- 4
set.seed(123)
# Build model with k clusters: km.out
km.out <- kmeans(cog_subset_clean_cog, centers = k, nstart = 20)

# Only possible with two variables, but I have more
# cog_subset_clean_cog$cluster_id <- factor(km.out$cluster)
# ggplot(cog_subset_clean_cog, aes(number_of_reviews, price, color = cluster_id)) +
  # geom_point(alpha = 0.25) +
  # xlab("Number of reviews") +
  # ylab("Price")