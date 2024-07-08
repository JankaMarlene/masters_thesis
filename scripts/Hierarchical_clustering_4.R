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

# Example: Checking ANOVA assumptions after hierarchical clustering

# Check normality of residuals for z_pvt_reaction_time_w
shapiro_test <- shapiro.test(residuals(lm(z_tmt_b_time_w ~ as.factor(cluster), data = clean_data)))
print(shapiro_test)

# Q-Q plot of residuals for z_pvt_reaction_time_w
qqnorm(residuals(lm(z_pvt_reaction_time_w ~ as.factor(cluster), data = clean_data)))
qqline(residuals(lm(z_pvt_reaction_time_w ~ as.factor(cluster), data = clean_data)))

# Check homogeneity of variances for z_pvt_reaction_time_w
levene_test <- leveneTest(residuals(lm(z_pvt_reaction_time_w ~ as.factor(cluster), data = clean_data)) ~ as.factor(cluster), data = clean_data)
print(levene_test)

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
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "Age", title = "Age Distribution between Clusters")

# Perform ANOVA for age between clusters
anova_age <- aov(age ~ as.factor(cog_df_cl$cluster), data = clean_data)

# Display ANOVA results
#summary(anova_age)
#age_stats

# Subset the data for clusters 1 and 4
#data_cluster_1_4 <- subset(clean_data, cog_df_cl$cluster %in% c(1, 4))

# Perform t-test for age between Cluster 1 and Cluster 4
#t_test_cluster_1_4 <- t.test(age ~ as.factor(cluster), data = data_cluster_1_4)

# Display t-test results
#t_test_cluster_1_4

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
#cluster_1_data <- subset(clean_data, cog_df_cl$cluster == 1)
#anova_cluster_1 <- aov(age ~ group, data = cluster_1_data)
#summary(anova_cluster_1)

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
