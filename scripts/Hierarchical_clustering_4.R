# Hierarchical clustering
library(tidyverse)
library(dplyr)
library(dendextend)
library(ggplot2)
library(gridExtra)
library(purrr)
library(car)
load("clean_data.RData")

# Extract relevant columns from clean_data
cog_df <- clean_data[, c("group","z_pvt_reaction_time_w","z_tmt_a_time_w","z_tmt_b_time_w")]
# Check structure and summarize contents of cog_df
str(cog_df)
summary(cog_df)

# Store group labels in a separate variable and exclude label (group column) from the dataset to do clustering
# Later true labels will be used to check how good clustering turned out
cog_label <- cog_df$group
cog_df$group <- NULL
str(cog_df)

# Build distance matrix
# Since all values are continuous numerical values I use euclidean distance method
dist_mat <- dist(cog_df, method = 'euclidean')

# Now decide which linkage method to use
# Try different kinds of linkage methods after decide which performed better
# Build dendrogram by plotting hierarchical cluster object with hclust
# Specify linkage method via 'method' argument
hclust_ward <- hclust(dist_mat, method = 'ward')
plot(hclust_ward)

# Create the desired number of clusters
# Since I want two groups 'withPCS' and 'withoutPCS' number of clusters = 2
cut_ward <- cutree(hclust_ward, k = 4)
# To visualize clusters on dendrogram use abline function to draw the cut line
plot(hclust_ward)
rect.hclust(hclust_ward, k = 4, border = 2:30)
abline(h = 10, col = 'red')
# Visualize tree with different colored branches
ward_dend_obj <- as.dendrogram(hclust_ward)
ward_col_dend <- color_branches(ward_dend_obj, h = 10)
plot(ward_col_dend)

# Append cluster results obtained back in the original dataframe 
# Use mutate
# Count how many observations were assigned to each cluster with the count function
cog_df_cl <- mutate(cog_df, cluster = cut_ward)
count(cog_df_cl,cluster)

# Cross-checking clustering results using table function
table(cog_df_cl$cluster,cog_label)

# Add the cluster information from cog_df_cl to clean_data
clean_data$cluster <- cog_df_cl$cluster

# Check the updated structure of clean_data
str(clean_data)

# Checking sex in cluster
table(cog_df_cl$cluster,clean_data$sex)

#--------
cluster_info <- clean_data %>%
  select(participant_id, cluster)

# Load the participants.tsv file
participants <- read.delim("C:/Users/jankj/OneDrive/Desktop/masters_thesis/data/merged_data.tsv", na.strings = "n/a", header = TRUE)

# Check the structure of participants to identify a common identifier
str(participants)

# Merge the clean_data with participants based on a common identifier
merged_data <- participants %>%
  left_join(cluster_info, by = "participant_id")

merged_data <- merged_data %>%
  rename(cluster_4 = cluster)

str(merged_data)

# Convert cluster_4 to character if necessary
merged_data <- merged_data %>%
  mutate(cluster_4 = as.character(cluster_4))

# Use dplyr::recode explicitly
merged_data <- merged_data %>%
  mutate(cluster_4 = dplyr::recode(cluster_4, `1` = 'c1', `2` = 'c2', `3` = 'c3', `4` = 'c4'))

# Check the structure of the merged data to ensure everything is correct
str(merged_data)

# Optionally, save the merged data to a new file
write_tsv(merged_data, "C:/Users/jankj/OneDrive/Desktop/masters_thesis/data/merged_data_all.tsv")

#--------
# Age

# Calculate means and standard deviations for age by cluster
age_stats <- clean_data %>%
  group_by(cog_df_cl$cluster) %>%
  summarise(
    mean = round(mean(age, na.rm = TRUE), 2),
    sd = round(sd(age, na.rm = TRUE), 2)
  )

# Plotting age distribution between clusters with mean as text
ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = age)) +
  # Custom boxplot without median line
  stat_boxplot(geom = "errorbar", width = 0.25) +  # Add whiskers
  geom_boxplot(aes(group = as.factor(cog_df_cl$cluster)), color = "black", fill = "gray80") +  
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.75, size = 1, color = "black") +
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), vjust = -0.5, color = "black") +
  labs(x = "Cluster", y = "Age", title = "Age Distribution between Clusters") +
  theme_bw()


# Perform ANOVA for age between clusters
anova_age <- aov(age ~ as.factor(cog_df_cl$cluster), data = clean_data)

# Display ANOVA results
#summary(anova_age)
#age_stats

# Calculate mean and sd for each group within each cluster
mean_sd_stats <- clean_data %>%
  group_by(cog_df_cl$cluster, group) %>%
  summarise(
    mean = round(mean(age, na.rm = TRUE), 2),
    sd = round(sd(age, na.rm = TRUE), 2)
  )

# Display mean and sd for each group within each cluster
print(mean_sd_stats)

# Plotting age distribution within clusters based on withPCS and withoutPCS labels with mean as text
ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = age, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) + # Adjust position of boxplots
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), # Adjust position of points
               shape = 18, size = 4, color = "red") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), 
               position = position_dodge(width = 0.75), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "Age", title = "Age Distribution within Clusters based on withPCS and withoutPCS Labels")

# Perform ANOVA for age within "withPCS" group between clusters
anova_withPCS <- aov(age ~ as.factor(cluster), data = subset(clean_data, group == "withPCS"))

# Perform ANOVA for age within "withoutPCS" group between clusters
anova_withoutPCS <- aov(age ~ as.factor(cluster), data = subset(clean_data, group == "withoutPCS"))

# Display ANOVA results for "withPCS" group
summary(anova_withPCS)

# Display ANOVA results for "withoutPCS" group
summary(anova_withoutPCS)

# Perform ANOVA for age between "withPCS" and "withoutPCS" groups within each cluster

# Cluster 1
cluster_1_data <- subset(clean_data, cluster == 1)
t_test_cluster_1 <- t.test(age ~ group, data = cluster_1_data)
t_test_cluster_1

# Cluster 2
cluster_2_data <- subset(clean_data, cluster == 2)
t_test_cluster_2 <- t.test(age ~ group, data = cluster_2_data)
t_test_cluster_2

# Cluster 3
cluster_3_data <- subset(clean_data, cluster == 3)
t_test_cluster_3 <- t.test(age ~ group, data = cluster_3_data)
t_test_cluster_3

# Cluster 4
cluster_4_data <- subset(clean_data, cluster == 4)
t_test_cluster_4 <- t.test(age ~ group, data = cluster_4_data)
t_test_cluster_4

#--------
# Education

# Ensure the cluster variable is part of the clean_data dataframe
clean_data$cluster <- cog_df_cl$cluster

# Calculate means of years_of_education by cluster
cluster_means <- tapply(clean_data$years_of_education, clean_data$cluster, FUN = mean)
cluster_means

# Calculate standard deviations of years_of_education by cluster
cluster_sds <- tapply(clean_data$years_of_education, clean_data$cluster, FUN = sd)
cluster_sds

# Combine means and standard deviations into a data frame
cluster_stats <- data.frame(
  cluster = names(cluster_means),
  mean = round(cluster_means, 2),
  sd = round(cluster_sds, 2)
)

# Print the cluster statistics
cluster_stats

# Perform ANOVA for years of education between clusters
anova_education <- aov(years_of_education ~ as.factor(cluster), data = clean_data)

# Display ANOVA results
summary(anova_education)

# Calculate mean and sd for each group within each cluster
mean_sd_stats <- clean_data %>%
  group_by(cluster, group) %>%
  summarise(
    mean = round(mean(years_of_education, na.rm = TRUE), 2),
    sd = round(sd(years_of_education, na.rm = TRUE), 2)
  )

# Display mean and sd for each group within each cluster
print(mean_sd_stats)

# Perform ANOVA for years of education within the "withPCS" group between clusters
anova_withPCS_education <- aov(years_of_education ~ as.factor(cluster), data = subset(clean_data, group == "withPCS"))

# Display ANOVA results for the "withPCS" group
summary(anova_withPCS_education)

# Perform ANOVA for years of education within the "withoutPCS" group between clusters
anova_withoutPCS_education <- aov(years_of_education ~ as.factor(cluster), data = subset(clean_data, group == "withoutPCS"))

# Display ANOVA results for the "withoutPCS" group
summary(anova_withoutPCS_education)

# Plotting years of education distribution within clusters based on withPCS and withoutPCS labels with mean as text
ggplot(clean_data, aes(x = as.factor(cluster), y = years_of_education, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) + # Adjust position of boxplots
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), # Adjust position of points
               shape = 18, size = 4, color = "red") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), 
               position = position_dodge(width = 0.75), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "Years of Education", title = "Years of Education Distribution within Clusters based on withPCS and withoutPCS Labels")

# Perform t-test for years of education between "withPCS" and "withoutPCS" groups within each cluster

# Cluster 1
cluster_1_data <- subset(clean_data, cluster == 1)
t_test_cluster_1 <- t.test(years_of_education ~ group, data = cluster_1_data)
t_test_cluster_1

# Cluster 2
cluster_2_data <- subset(clean_data, cluster == 2)
t_test_cluster_2 <- t.test(years_of_education ~ group, data = cluster_2_data)
t_test_cluster_2

# Cluster 3
cluster_3_data <- subset(clean_data, cluster == 3)
t_test_cluster_3 <- t.test(years_of_education ~ group, data = cluster_3_data)
t_test_cluster_3

# Cluster 4
cluster_4_data <- subset(clean_data, cluster == 4)
t_test_cluster_4 <- t.test(years_of_education ~ group, data = cluster_4_data)
t_test_cluster_4

#--------
# Cognitive variables actual values

# Vector of cognitive variables
variables <- c("pvt_reaction_time", "nback_miss_1", "nback_miss_2", "tmt_a_time", "tmt_b_time", "tmt_diff")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable to create boxplots
for (variable in variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable))) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

# Initialize lists to store results
anova_results <- list()
descriptive_stats_list <- list()
normality_results <- list()
homogeneity_results <- list()
effect_sizes <- list()

# Perform ANOVA, calculate descriptive statistics, check assumptions, and calculate effect sizes
for (variable in variables) {
  # Perform ANOVA
  anova_result <- aov(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  anova_results[[variable]] <- summary(anova_result)
  
  # Calculate descriptive statistics
  descriptive_stats <- clean_data %>%
    group_by(cluster = as.factor(cluster)) %>%
    summarise(
      mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
      sd = round(sd(!!sym(variable), na.rm = TRUE), 2)
    ) %>%
    mutate(
      mean = format(mean, nsmall = 2),
      sd = format(sd, nsmall = 2)
    )
  descriptive_stats_list[[variable]] <- descriptive_stats
  
  # Check normality of residuals
  shapiro_test <- shapiro.test(residuals(lm(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)))
  normality_results[[variable]] <- shapiro_test
  
  # Check homogeneity of variances
  levene_test <- car::leveneTest(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  homogeneity_results[[variable]] <- levene_test
  
  # Calculate effect size (Eta Squared)
  eta_squared <- summary(anova_result)[[1]][["Sum Sq"]][1] / sum(summary(anova_result)[[1]][["Sum Sq"]])
  effect_sizes[[variable]] <- eta_squared
}

# Display results
anova_results
descriptive_stats_list
normality_results
homogeneity_results
effect_sizes

# Function to calculate mean and sd for a single variable
calculate_stats <- function(variable_name) {
  clean_data %>%
    group_by(cluster = as.factor(cog_df_cl$cluster), group) %>%
    summarise(
      mean = round(mean(!!sym(variable_name), na.rm = TRUE), 4),
      sd = round(sd(!!sym(variable_name), na.rm = TRUE), 4)
    ) %>%
    mutate(variable = variable_name)  # Add variable name as a column
}

# Map function over each variable to calculate mean and sd
stats_list <- map(variables, calculate_stats)

# Extract and name each table by variable
named_stats_tables <- map(setNames(stats_list, variables), bind_rows)

# Display each table
named_stats_tables

# Initialize lists to store ANOVA results for withPCS and withoutPCS groups
anova_results_withPCS <- list()
anova_results_withoutPCS <- list()

# Loop over each variable to perform ANOVA within the "withPCS" and "withoutPCS" groups
for (variable in variables) {
  # Perform ANOVA for the current variable within "withPCS" group
  withPCS_data <- subset(clean_data, group == "withPCS")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "withoutPCS")
  anova_withoutPCS <- aov(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster), data = withoutPCS_data)
  
  # Store the ANOVA results
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_results_withoutPCS[[variable]] <- summary(anova_withoutPCS)
}

# Display ANOVA results for "withPCS" and "withoutPCS" groups
anova_results_withPCS
anova_results_withoutPCS

# Plot cognitive variables within clusters based on withPCS and withoutPCS labels
plot_list_group <- list()

for (variable in variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster and Group"))
  
  plot_list_group[[variable]] <- plot
}

grid.arrange(grobs = plot_list_group, ncol = 2)

# Initialize list to store t-test results
t_test_results <- list()

# Function to perform t-test for a single variable within each cluster
perform_t_test <- function(variable_name) {
  t_test_list <- list()
  
  for (i in 1:3) {
    cluster_data <- subset(clean_data, cluster == i)
    t_test <- t.test(cluster_data[[variable_name]] ~ cluster_data$group)
    t_test_list[[paste("Cluster", i)]] <- t_test
  }
  
  t_test_list
}

# Perform t-tests for each variable within each cluster
for (variable in variables) {
  t_test_results[[variable]] <- perform_t_test(variable)
}

# Display t-test results
t_test_results

##
# Initialize lists to store ANOVA results and models
anova_models <- list()
anova_models_withPCS <- list()
anova_models_withoutPCS <- list()

# Loop over each variable to perform ANOVA and store the models
for (variable in variables) {
  # Perform ANOVA
  anova_result <- aov(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  anova_results[[variable]] <- summary(anova_result)
  anova_models[[variable]] <- anova_result
  
  # Perform ANOVA for the current variable within "withPCS" group
  withPCS_data <- subset(clean_data, group == "withPCS")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_models_withPCS[[variable]] <- anova_withPCS
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "withoutPCS")
  anova_withoutPCS <- aov(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster), data = withoutPCS_data)
  anova_results_withoutPCS[[variable]] <- summary(anova_withoutPCS)
  anova_models_withoutPCS[[variable]] <- anova_withoutPCS
}

# Display ANOVA results
anova_results
anova_results_withPCS
anova_results_withoutPCS


# Initialize lists to store Tukey HSD results
tukey_results <- list()
tukey_results_withPCS <- list()
tukey_results_withoutPCS <- list()

# Loop over each variable to perform Tukey HSD test if ANOVA is significant
for (variable in variables) {
  # Check if the overall ANOVA for the variable is significant
  if (anova_results[[variable]][[1]]["as.factor(cluster)", "Pr(>F)"] < 0.05) {
    tukey_results[[variable]] <- TukeyHSD(anova_models[[variable]])
  }
  
  # Check if the ANOVA for the variable within "withPCS" group is significant
  if (anova_results_withPCS[[variable]][[1]]["as.factor(withPCS_data$cluster)", "Pr(>F)"] < 0.05) {
    tukey_results_withPCS[[variable]] <- TukeyHSD(anova_models_withPCS[[variable]])
  }
  
  # Check if the ANOVA for the variable within "withoutPCS" group is significant
  if (anova_results_withoutPCS[[variable]][[1]]["as.factor(withoutPCS_data$cluster)", "Pr(>F)"] < 0.05) {
    tukey_results_withoutPCS[[variable]] <- TukeyHSD(anova_models_withoutPCS[[variable]])
  }
}

# Display Tukey HSD results for significant variables
tukey_results
tukey_results_withPCS
tukey_results_withoutPCS



# Initialize a list to store the results
results <- list()

# Loop through each variable and perform the Mann-Whitney U test
for (variable in variables) {
  # Extract the data for Cluster 4
  withPCS_cluster4 <- withPCS_data[withPCS_data$cluster == 4, variable]
  withoutPCS_cluster4 <- withoutPCS_data[withoutPCS_data$cluster == 4, variable]
  
  # Perform Mann-Whitney U test
  result <- wilcox.test(withPCS_cluster4, withoutPCS_cluster4)
  
  # Store the result in the list
  results[[variable]] <- result
}

# Print the results
for (variable in variables) {
  cat("Results for", variable, ":\n")
  print(results[[variable]])
  cat("\n")
}


#--------
# Cognitive variables winsorized Data

# Vector of variables for which to create boxplots
variables <- c("pvt_reaction_time_w", "nback_miss_1_w", "nback_miss_2_w", "tmt_a_time_w", "tmt_b_time_w","tmt_diff_w")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable to create boxplots
for (variable in variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable))) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

# Initialize lists to store results
anova_results <- list()
descriptive_stats_list <- list()
normality_results <- list()
homogeneity_results <- list()
effect_sizes <- list()

# Perform ANOVA, calculate descriptive statistics, check assumptions, and calculate effect sizes
for (variable in variables) {
  # Perform ANOVA
  anova_result <- aov(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  anova_results[[variable]] <- summary(anova_result)
  
  # Calculate descriptive statistics
  descriptive_stats <- clean_data %>%
    group_by(cluster = as.factor(cluster)) %>%
    summarise(
      mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
      sd = round(sd(!!sym(variable), na.rm = TRUE), 2)
    ) %>%
    mutate(
      mean = format(mean, nsmall = 2),
      sd = format(sd, nsmall = 2)
    )
  descriptive_stats_list[[variable]] <- descriptive_stats
  
  # Check normality of residuals
  #shapiro_test <- shapiro.test(residuals(lm(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)))
  #normality_results[[variable]] <- shapiro_test
  
  # Check homogeneity of variances
  #levene_test <- car::leveneTest(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  #homogeneity_results[[variable]] <- levene_test
  
  # Calculate effect size (Eta Squared)
  #eta_squared <- summary(anova_result)[[1]][["Sum Sq"]][1] / sum(summary(anova_result)[[1]][["Sum Sq"]])
  #effect_sizes[[variable]] <- eta_squared
}

# Display results
anova_results
descriptive_stats_list
normality_results
homogeneity_results
effect_sizes

# Function to calculate mean and sd for a single variable
calculate_stats <- function(variable_name) {
  clean_data %>%
    group_by(cluster = as.factor(cog_df_cl$cluster), group) %>%
    summarise(
      mean = round(mean(!!sym(variable_name), na.rm = TRUE), 4),
      sd = round(sd(!!sym(variable_name), na.rm = TRUE), 4)
    ) %>%
    mutate(variable = variable_name)  # Add variable name as a column
}

# Map function over each variable to calculate mean and sd
stats_list <- map(variables, calculate_stats)

# Extract and name each table by variable
named_stats_tables <- map(setNames(stats_list, variables), bind_rows)

# Display each table
named_stats_tables

# Initialize lists to store ANOVA results for withPCS and withoutPCS groups
anova_results_withPCS <- list()
anova_results_withoutPCS <- list()

# Loop over each variable to perform ANOVA within the "withPCS" and "withoutPCS" groups
for (variable in variables) {
  # Perform ANOVA for the current variable within "withPCS" group
  withPCS_data <- subset(clean_data, group == "withPCS")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "withoutPCS")
  anova_withoutPCS <- aov(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster), data = withoutPCS_data)
  
  # Store the ANOVA results
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_results_withoutPCS[[variable]] <- summary(anova_withoutPCS)
}

# Display ANOVA results for "withPCS" and "withoutPCS" groups
anova_results_withPCS
anova_results_withoutPCS

# Plot cognitive variables within clusters based on withPCS and withoutPCS labels
plot_list_group <- list()

for (variable in variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster and Group"))
  
  plot_list_group[[variable]] <- plot
}

grid.arrange(grobs = plot_list_group, ncol = 2)

# Initialize list to store t-test results
t_test_results <- list()

# Function to perform t-test for a single variable within each cluster
perform_t_test <- function(variable_name) {
  t_test_list <- list()
  
  for (i in 1:3) {
    cluster_data <- subset(clean_data, cluster == i)
    t_test <- t.test(cluster_data[[variable_name]] ~ cluster_data$group)
    t_test_list[[paste("Cluster", i)]] <- t_test
  }
  
  t_test_list
}

# Perform t-tests for each variable within each cluster
for (variable in variables) {
  t_test_results[[variable]] <- perform_t_test(variable)
}

# Display t-test results
t_test_results

##
# Initialize lists to store ANOVA results and models
anova_models <- list()
anova_models_withPCS <- list()
anova_models_withoutPCS <- list()

# Loop over each variable to perform ANOVA and store the models
for (variable in variables) {
  # Perform ANOVA
  anova_result <- aov(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  anova_results[[variable]] <- summary(anova_result)
  anova_models[[variable]] <- anova_result
  
  # Perform ANOVA for the current variable within "withPCS" group
  withPCS_data <- subset(clean_data, group == "withPCS")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_models_withPCS[[variable]] <- anova_withPCS
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "withoutPCS")
  anova_withoutPCS <- aov(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster), data = withoutPCS_data)
  anova_results_withoutPCS[[variable]] <- summary(anova_withoutPCS)
  anova_models_withoutPCS[[variable]] <- anova_withoutPCS
}

# Display ANOVA results
anova_results
anova_results_withPCS
anova_results_withoutPCS


# Initialize lists to store Tukey HSD results
tukey_results <- list()
tukey_results_withPCS <- list()
tukey_results_withoutPCS <- list()

# Loop over each variable to perform Tukey HSD test if ANOVA is significant
for (variable in variables) {
  # Check if the overall ANOVA for the variable is significant
  if (anova_results[[variable]][[1]]["as.factor(cluster)", "Pr(>F)"] < 0.05) {
    tukey_results[[variable]] <- TukeyHSD(anova_models[[variable]])
  }
  
  # Check if the ANOVA for the variable within "withPCS" group is significant
  if (anova_results_withPCS[[variable]][[1]]["as.factor(withPCS_data$cluster)", "Pr(>F)"] < 0.05) {
    tukey_results_withPCS[[variable]] <- TukeyHSD(anova_models_withPCS[[variable]])
  }
  
  # Check if the ANOVA for the variable within "withoutPCS" group is significant
  if (anova_results_withoutPCS[[variable]][[1]]["as.factor(withoutPCS_data$cluster)", "Pr(>F)"] < 0.05) {
    tukey_results_withoutPCS[[variable]] <- TukeyHSD(anova_models_withoutPCS[[variable]])
  }
}

# Display Tukey HSD results for significant variables
tukey_results
tukey_results_withPCS
tukey_results_withoutPCS

#--------
# Cognitive variables standardized data

# Vector of variables for which to create boxplots
variables <- c("z_pvt_reaction_time_w", "s_nback_miss_1_w", "s_nback_miss_2_w", "z_tmt_a_time_w", "z_tmt_b_time_w","z_tmt_diff_w")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable to create boxplots
for (variable in variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable))) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

# Initialize lists to store results
anova_results <- list()
descriptive_stats_list <- list()
normality_results <- list()
homogeneity_results <- list()
effect_sizes <- list()

# Perform ANOVA, calculate descriptive statistics, check assumptions, and calculate effect sizes
for (variable in variables) {
  # Perform ANOVA
  anova_result <- aov(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  anova_results[[variable]] <- summary(anova_result)
  
  # Calculate descriptive statistics
  descriptive_stats <- clean_data %>%
    group_by(cluster = as.factor(cluster)) %>%
    summarise(
      mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
      sd = round(sd(!!sym(variable), na.rm = TRUE), 2)
    ) %>%
    mutate(
      mean = format(mean, nsmall = 2),
      sd = format(sd, nsmall = 2)
    )
  descriptive_stats_list[[variable]] <- descriptive_stats
  
  # Check normality of residuals
  shapiro_test <- shapiro.test(residuals(lm(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)))
  normality_results[[variable]] <- shapiro_test
  
  # Check homogeneity of variances
  levene_test <- car::leveneTest(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  homogeneity_results[[variable]] <- levene_test
  
  # Calculate effect size (Eta Squared)
  eta_squared <- summary(anova_result)[[1]][["Sum Sq"]][1] / sum(summary(anova_result)[[1]][["Sum Sq"]])
  effect_sizes[[variable]] <- eta_squared
}

# Display results
anova_results
descriptive_stats_list
normality_results
homogeneity_results
effect_sizes

# Function to calculate mean and sd for a single variable
calculate_stats <- function(variable_name) {
  clean_data %>%
    group_by(cluster = as.factor(cog_df_cl$cluster), group) %>%
    summarise(
      mean = round(mean(!!sym(variable_name), na.rm = TRUE), 4),
      sd = round(sd(!!sym(variable_name), na.rm = TRUE), 4)
    ) %>%
    mutate(variable = variable_name)  # Add variable name as a column
}

# Map function over each variable to calculate mean and sd
stats_list <- map(variables, calculate_stats)

# Extract and name each table by variable
named_stats_tables <- map(setNames(stats_list, variables), bind_rows)

# Display each table
named_stats_tables

# Initialize lists to store ANOVA results for withPCS and withoutPCS groups
anova_results_withPCS <- list()
anova_results_withoutPCS <- list()

# Loop over each variable to perform ANOVA within the "withPCS" and "withoutPCS" groups
for (variable in variables) {
  # Perform ANOVA for the current variable within "withPCS" group
  withPCS_data <- subset(clean_data, group == "withPCS")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "withoutPCS")
  anova_withoutPCS <- aov(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster), data = withoutPCS_data)
  
  # Store the ANOVA results
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_results_withoutPCS[[variable]] <- summary(anova_withoutPCS)
}

# Display ANOVA results for "withPCS" and "withoutPCS" groups
anova_results_withPCS
anova_results_withoutPCS

# Plot cognitive variables within clusters based on withPCS and withoutPCS labels
plot_list_group <- list()

for (variable in variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster and Group"))
  
  plot_list_group[[variable]] <- plot
}

grid.arrange(grobs = plot_list_group, ncol = 2)

# Initialize list to store t-test results
t_test_results <- list()

# Function to perform t-test for a single variable within each cluster
perform_t_test <- function(variable_name) {
  t_test_list <- list()
  
  for (i in 1:3) {
    cluster_data <- subset(clean_data, cluster == i)
    t_test <- t.test(cluster_data[[variable_name]] ~ cluster_data$group)
    t_test_list[[paste("Cluster", i)]] <- t_test
  }
  
  t_test_list
}

# Perform t-tests for each variable within each cluster
for (variable in variables) {
  t_test_results[[variable]] <- perform_t_test(variable)
}

# Display t-test results
t_test_results

#--------
## Questionairs

# Vector of variables for which to create boxplots
new_variables <- c("facit_f_FS", "hads_a_total_score", "hads_d_total_score", "psqi_total_score")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable to create boxplots
for (variable in new_variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable))) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

# Initialize lists to store results
anova_results <- list()
descriptive_stats_list <- list()
normality_results <- list()
homogeneity_results <- list()
effect_sizes <- list()

# Perform ANOVA, calculate descriptive statistics, check assumptions, and calculate effect sizes
for (variable in new_variables) {
  # Perform ANOVA
  anova_result <- aov(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  anova_results[[variable]] <- summary(anova_result)
  
  # Calculate descriptive statistics
  descriptive_stats <- clean_data %>%
    group_by(cluster = as.factor(cluster)) %>%
    summarise(
      mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
      sd = round(sd(!!sym(variable), na.rm = TRUE), 2)
    ) %>%
    mutate(
      mean = format(mean, nsmall = 2),
      sd = format(sd, nsmall = 2)
    )
  descriptive_stats_list[[variable]] <- descriptive_stats
  
  # Check normality of residuals
  shapiro_test <- shapiro.test(residuals(lm(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)))
  normality_results[[variable]] <- shapiro_test
  
  # Check homogeneity of variances
  levene_test <- car::leveneTest(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  homogeneity_results[[variable]] <- levene_test
  
  # Calculate effect size (Eta Squared)
  eta_squared <- summary(anova_result)[[1]][["Sum Sq"]][1] / sum(summary(anova_result)[[1]][["Sum Sq"]])
  effect_sizes[[variable]] <- eta_squared
}

# Display results
anova_results
descriptive_stats_list
normality_results
homogeneity_results
effect_sizes

# Function to calculate mean and sd for a single variable
calculate_stats <- function(variable_name) {
  clean_data %>%
    group_by(cluster = as.factor(cog_df_cl$cluster), group) %>%
    summarise(
      mean = round(mean(!!sym(variable_name), na.rm = TRUE), 4),
      sd = round(sd(!!sym(variable_name), na.rm = TRUE), 4)
    ) %>%
    mutate(variable = variable_name)  # Add variable name as a column
}

# Map function over each variable to calculate mean and sd
stats_list <- map(new_variables, calculate_stats)

# Extract and name each table by variable
named_stats_tables <- map(setNames(stats_list, new_variables), bind_rows)

# Display each table
named_stats_tables

# Initialize lists to store ANOVA results for withPCS and withoutPCS groups
anova_results_withPCS <- list()
anova_results_withoutPCS <- list()

# Loop over each variable to perform ANOVA within the "withPCS" and "withoutPCS" groups
for (variable in new_variables) {
  # Perform ANOVA for the current variable within "withPCS" group
  withPCS_data <- subset(clean_data, group == "withPCS")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "withoutPCS")
  anova_withoutPCS <- aov(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster), data = withoutPCS_data)
  
  # Store the ANOVA results
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_results_withoutPCS[[variable]] <- summary(anova_withoutPCS)
}

# Display ANOVA results for "withPCS" and "withoutPCS" groups
anova_results_withPCS
anova_results_withoutPCS

##
# Initialize lists to store ANOVA results and models
anova_models <- list()
anova_models_withPCS <- list()
anova_models_withoutPCS <- list()

# Loop over each variable to perform ANOVA and store the models
for (variable in new_variables) {
  # Perform ANOVA
  anova_result <- aov(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  anova_results[[variable]] <- summary(anova_result)
  anova_models[[variable]] <- anova_result
  
  # Perform ANOVA for the current variable within "withPCS" group
  withPCS_data <- subset(clean_data, group == "withPCS")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_models_withPCS[[variable]] <- anova_withPCS
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "withoutPCS")
  anova_withoutPCS <- aov(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster), data = withoutPCS_data)
  anova_results_withoutPCS[[variable]] <- summary(anova_withoutPCS)
  anova_models_withoutPCS[[variable]] <- anova_withoutPCS
}

# Display ANOVA results
anova_results
anova_results_withPCS
anova_results_withoutPCS


# Initialize lists to store Tukey HSD results
tukey_results <- list()
tukey_results_withPCS <- list()
tukey_results_withoutPCS <- list()

# Loop over each variable to perform Tukey HSD test if ANOVA is significant
for (variable in new_variables) {
  # Check if the overall ANOVA for the variable is significant
  if (anova_results[[variable]][[1]]["as.factor(cluster)", "Pr(>F)"] < 0.05) {
    tukey_results[[variable]] <- TukeyHSD(anova_models[[variable]])
  }
  
  # Check if the ANOVA for the variable within "withPCS" group is significant
  if (anova_results_withPCS[[variable]][[1]]["as.factor(withPCS_data$cluster)", "Pr(>F)"] < 0.05) {
    tukey_results_withPCS[[variable]] <- TukeyHSD(anova_models_withPCS[[variable]])
  }
  
  # Check if the ANOVA for the variable within "withoutPCS" group is significant
  if (anova_results_withoutPCS[[variable]][[1]]["as.factor(withoutPCS_data$cluster)", "Pr(>F)"] < 0.05) {
    tukey_results_withoutPCS[[variable]] <- TukeyHSD(anova_models_withoutPCS[[variable]])
  }
}

# Display Tukey HSD results for significant variables
tukey_results
tukey_results_withPCS
tukey_results_withoutPCS

# Plot cognitive variables within clusters based on withPCS and withoutPCS labels
plot_list_group <- list()

for (variable in new_variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster and Group"))
  
  plot_list_group[[variable]] <- plot
}

grid.arrange(grobs = plot_list_group, ncol = 2)

# Initialize list to store t-test results
t_test_results <- list()

# Function to perform t-test for a single variable within each cluster
perform_t_test <- function(variable_name) {
  t_test_list <- list()
  
  for (i in 1:3) {
    cluster_data <- subset(clean_data, cluster == i)
    t_test <- t.test(cluster_data[[variable_name]] ~ cluster_data$group)
    t_test_list[[paste("Cluster", i)]] <- t_test
  }
  
  t_test_list
}

# Perform t-tests for each variable within each cluster
for (variable in new_variables) {
  t_test_results[[variable]] <- perform_t_test(variable)
}


# Display t-test results
t_test_results

# Initialize a list to store the results
results <- list()

# Loop through each variable and perform the Mann-Whitney U test
for (variable in new_variables) {
  # Extract the data for Cluster 4
  withPCS_cluster4 <- withPCS_data[withPCS_data$cluster == 4, variable]
  withoutPCS_cluster4 <- withoutPCS_data[withoutPCS_data$cluster == 4, variable]
  
  # Perform Mann-Whitney U test
  result <- wilcox.test(withPCS_cluster4, withoutPCS_cluster4)
  
  # Store the result in the list
  results[[variable]] <- result
}

# Print the results
for (variable in new_variables) {
  cat("Results for", variable, ":\n")
  print(results[[variable]])
  cat("\n")
}
