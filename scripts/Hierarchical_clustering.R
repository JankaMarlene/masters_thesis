# Hierarchical clustering
library(tidyverse)
library(dplyr)
library(dendextend)
library(ggplot2)
library(gridExtra)
library(purrr)
library(vroom)
library(cowplot)
theme_set(theme_cowplot())
load("clean_data.RData")

# Extract relevant columns from clean_data
cog_df <- clean_data[, c("group","z_pvt_reaction_time_w","z_tmt_a_time_w","z_tmt_b_time_w")]
clean_data$group[clean_data$group == "withPCS"] <- "self-reported CD"
clean_data$group[clean_data$group == "withoutPCS"] <- "no self-reported CD"

# Check structure and summarize contents of cog_df
str(cog_df)
summary(cog_df)

# Store group labels in a separate variable and exclude label (group column) from the dataset to do clustering
# Later true labels will be used to check how good clustering turned out
cog_label <- cog_df$group
cog_label[cog_label == "withPCS"] <- "self-reported CD"
cog_label[cog_label == "withoutPCS"] <- "no self-reported CD"
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
cut_ward <- cutree(hclust_ward, k = 2)
# To visualize clusters on dendrogram use abline function to draw the cut line
plot(hclust_ward)
rect.hclust(hclust_ward, k = 2, border = 2:30)
abline(h = 28, col = 'red')
# Visualize tree with different colored branches
ward_dend_obj <- as.dendrogram(hclust_ward)
ward_col_dend <- color_branches(ward_dend_obj, h = 28)
plot(ward_col_dend)

# Visualize the clusters see YT Video Hierarchical Clustering in R Spencer Pao
# install.packages("factoextra")
# library(factoextra)
# cluster_obj <- list(data = cog_df_sc, cluster = cut_avg)
# fviz_cluster(cluster_obj)
# rownames(cog_df_sc) <- paste(cog_label, 1:dim(cog_df) [1], sep = "_")
# fviz_cluster(list(data=cog_df_sc))

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
participants <- read.delim("C:/Users/jankj/OneDrive/Desktop/masters_thesis/data/participants.tsv", na.strings = "n/a", header = TRUE)

# Check the structure of participants to identify a common identifier
str(participants)

# Merge the clean_data with participants based on a common identifier
merged_data <- participants %>%
  left_join(cluster_info, by = "participant_id")

merged_data <- merged_data %>%
  rename(cluster_2 = cluster)

# Convert cluster_4 to character if necessary
merged_data <- merged_data %>%
  mutate(cluster_2 = as.character(cluster_2))

# Use dplyr::recode explicitly
merged_data <- merged_data %>%
  mutate(cluster_2 = dplyr::recode(cluster_2, `1` = 'c1', `2` = 'c2'))

# Check the structure of the merged data to ensure everything is correct
str(merged_data)

# Optionally, save the merged data to a new file
write_tsv(merged_data, "C:/Users/jankj/OneDrive/Desktop/masters_thesis/data/merged_data.tsv")


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
  labs(x = "Cluster", y = "age", title = "Age Distribution between Clusters")

# Perform t-test for age between clusters
t_test_age <- t.test(age ~ as.factor(cog_df_cl$cluster), data = clean_data)

# Display t-test results
t_test_age
age_stats

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
               shape = 18, size = 4, color = "black") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), 
               position = position_dodge(width = 0.75), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "age", title = "Age Distribution within Clusters")

# Perform t-test for age within "withPCS" group between clusters
t_test_withPCS <- t.test(age ~ as.factor(cluster), data = subset(clean_data, group == "self-reported CD"))

# Perform t-test for age within "withoutPCS" group between clusters
t_test_withoutPCS <- t.test(age ~ as.factor(cluster), data = subset(clean_data, group == "no self-reported CD"))

# Display t-test results for "withPCS" group
t_test_withPCS

# Display t-test results for "withoutPCS" group
t_test_withoutPCS

# Filter the data for Cluster 1
cluster_1_data <- subset(clean_data, cluster == 1)

# Perform t-test for age between "withPCS" and "withoutPCS" groups within Cluster 1
t_test_cluster_1 <- t.test(age ~ group, data = cluster_1_data)

# Display t-test results
t_test_cluster_1

# Filter the data for Cluster 2
cluster_2_data <- subset(clean_data, cluster == 2)

# Perform t-test for age between "withPCS" and "withoutPCS" groups within Cluster 2
t_test_cluster_2 <- t.test(age ~ group, data = cluster_2_data)

# Display t-test results
t_test_cluster_2

#--------
# Education

# Calculate means of years_of_education by cluster
cluster_means <- tapply(clean_data$years_of_education, cog_df_cl$cluster, FUN = mean)
cluster_means

# Calculate standard deviations of years_of_education by cluster
cluster_sds <- tapply(clean_data$years_of_education, cog_df_cl$cluster, FUN = sd)
cluster_sds

# Combine means and standard deviations into a data frame
cluster_stats <- data.frame(
  cluster = names(cluster_means),
  mean = round(cluster_means, 2),
  sd = round(cluster_sds, 2)
)

# Print the cluster statistics
cluster_stats

# T-Test for years of education between clusters overall
t_test_education <- t.test(years_of_education ~ as.factor(cog_df_cl$cluster), data = clean_data)

# Display t-Test results
t_test_education

# Calculate mean and sd for each group within each cluster
mean_sd_stats <- clean_data %>%
  group_by(cog_df_cl$cluster, group) %>%
  summarise(
    mean = round(mean(years_of_education, na.rm = TRUE), 2),
    sd = round(sd(years_of_education, na.rm = TRUE), 2)
  )

# Display mean and sd for each group within each cluster
print(mean_sd_stats)

# T-Test for years of education within the "withPCS" group between clusters
t_test_withPCS_education <- t.test(years_of_education ~ as.factor(cluster), data = subset(clean_data, group == "self-reported CD"))

# Display t-Test results for the "withPCS" group
t_test_withPCS_education

# T-Test for years of education within the "withoutPCS" group between clusters
t_test_withoutPCS_education <- t.test(years_of_education ~ as.factor(cluster), data = subset(clean_data, group == "no self-reported CD"))

# Display t-Test results for the "withoutPCS" group
t_test_withoutPCS_education

# Plotting years of education distribution within clusters based on withPCS and withoutPCS labels with mean as text
ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = years_of_education, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) + # Adjust position of boxplots
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), # Adjust position of points
               shape = 18, size = 4, color = "red") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), 
               position = position_dodge(width = 0.75), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "years of education", title = "years_of_education Distribution within Clusters")

# Filter the data for Cluster 1
cluster_1_data <- subset(clean_data, cluster == 1)

# Perform t-test for years_of_education between "withPCS" and "withoutPCS" groups within Cluster 1
t_test_cluster_1 <- t.test(years_of_education ~ group, data = cluster_1_data)

# Display t-test results
t_test_cluster_1

# Filter the data for Cluster 2
cluster_2_data <- subset(clean_data, cluster == 2)

# Perform t-test for years_of_education between "withPCS" and "withoutPCS" groups within Cluster 2
t_test_cluster_2 <- t.test(years_of_education ~ group, data = cluster_2_data)

# Display t-test results
t_test_cluster_2

#--------
# Cognitive variables actual values

# Vector of variables for which to create boxplots
variables <- c("pvt_reaction_time", "nback_miss_1", "nback_miss_2", "tmt_a_time", "tmt_b_time","tmt_diff")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable
for (variable in variables) {
  # Create boxplot for the current variable grouped by cluster
  plot <- ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = !!sym(variable))) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  # Append the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)


# Initialize an empty list to store the test results
test_results <- list()

# Loop over each variable
for (variable in variables) {
  # Perform Wilcoxon rank sum test for the current variable
  test_result <- t.test(clean_data[[variable]] ~ as.factor(cog_df_cl$cluster), exact = FALSE)
  
  # Calculate descriptive statistics
  descriptive_stats <- clean_data %>%
    group_by(cog_df_cl$cluster) %>%
    summarise(
      mean = round(mean(!!sym(variable), na.rm = TRUE), 4),
      sd = round(sd(!!sym(variable), na.rm = TRUE), 4)
    ) %>%
    mutate(
      mean = format(mean, nsmall = 2),
      sd = format(sd, nsmall = 2)
    )
  
  # Combine test results and descriptive statistics
  test_results[[variable]] <- list(
    test = test_result,
    descriptives = descriptive_stats
  )
}

# Display the test results
test_results


# Install and load the effsize package if not already installed
if (!requireNamespace("effsize", quietly = TRUE)) {
  install.packages("effsize")
}
library(effsize)

# Initialize an empty list to store the effect sizes
effect_sizes <- list()

# Loop over each variable to calculate effect sizes
for (variable in variables) {
  # Calculate Cohen's d for the current variable
  cohens_d <- cohen.d(clean_data[[variable]], as.factor(cog_df_cl$cluster))
  
  # Store the effect size in the list
  effect_sizes[[variable]] <- cohens_d$estimate
}

# Display the effect sizes
effect_sizes


# Vector of variables for which to create boxplots
variables <- c("pvt_reaction_time", "nback_miss_1", "nback_miss_2", "tmt_a_time", "tmt_b_time", "tmt_diff")

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

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable
for (variable in variables) {
  # Create boxplot for the current variable grouped by cluster and fill by group
  plot <- ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  # Append the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)


# Initialize an empty list to store the test results for withPCS group
test_results_withPCS <- list()

# Initialize an empty list to store the test results for withoutPCS group
test_results_withoutPCS <- list()

# Loop over each variable
for (variable in variables) {
  # Perform t-test for the current variable within "withPCS" group
  t_test_result_withPCS <- t.test(clean_data[[variable]][clean_data$group == "self-reported CD" & cog_df_cl$cluster == 1],
                           clean_data[[variable]][clean_data$group == "self-reported CD" & cog_df_cl$cluster == 2])
  
  # Perform t-test for the current variable within "withoutPCS" group
  t_test_result_withoutPCS <- t.test(clean_data[[variable]][clean_data$group == "no self-reported CD" & cog_df_cl$cluster == 1],
                              clean_data[[variable]][clean_data$group == "no self-reported CD" & cog_df_cl$cluster == 2])
  
  # Store the test results for withPCS group in the list
  test_results_withPCS[[variable]] <- t_test_result_withPCS
  
  # Store the test results for withoutPCS group in the list
  test_results_withoutPCS[[variable]] <- t_test_result_withoutPCS
}

# Display the test results for withPCS group
test_results_withPCS

# Display the test results for withoutPCS group
test_results_withoutPCS


# Function to perform t-test for a single variable within each cluster
perform_t_test <- function(variable_name) {
  # Filter data for Cluster 1
  cluster_1_data <- subset(clean_data, cog_df_cl$cluster == 1)
  
  # Perform t-test for the variable between "withPCS" and "withoutPCS" groups within Cluster 1
  t_test_cluster_1 <- t.test(cluster_1_data[[variable_name]] ~ cluster_1_data$group)
  
  # Filter data for Cluster 2
  cluster_2_data <- subset(clean_data, cog_df_cl$cluster == 2)
  
  # Perform t-test for the variable between "withPCS" and "withoutPCS" groups within Cluster 2
  t_test_cluster_2 <- t.test(cluster_2_data[[variable_name]] ~ cluster_2_data$group)
  
  # Return t-test results as a list
  list(cluster_1 = t_test_cluster_1, cluster_2 = t_test_cluster_2)
}

# Map function over each variable to perform t-test
stats_t_test <- map(variables, perform_t_test)

# Display t-test results for each variable within each cluster
# Print results with variable names
for (i in seq_along(variables)) {
  cat("Variable:", variables[i], "\n")
  
  cat("Cluster 1:\n")
  cat("Mean in group withoutPCS:", round(stats_t_test[[i]]$cluster_1$estimate[1], 2), "\n")
  cat("Mean in group withPCS:", round(stats_t_test[[i]]$cluster_1$estimate[2], 2), "\n")
  cat("p-value:", round(stats_t_test[[i]]$cluster_1$p.value, 4), "\n\n")
  
  cat("Cluster 2:\n")
  cat("Mean in group withoutPCS:", round(stats_t_test[[i]]$cluster_2$estimate[1], 2), "\n")
  cat("Mean in group withPCS:", round(stats_t_test[[i]]$cluster_2$estimate[2], 2), "\n")
  cat("p-value:", round(stats_t_test[[i]]$cluster_2$p.value, 4), "\n\n")
}

# Initialize lists to store results
descriptive_stats_list <- list()
normality_results <- list()
homogeneity_results <- list()

# Calculate descriptive statistics, check assumptions, and calculate effect sizes
for (variable in variables) {
  
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
  
}

# Display results
descriptive_stats_list
normality_results
homogeneity_results




# Initialize result containers
shapiro_per_cluster <- list()
levene_results <- list()

# Loop over each variable
for (variable in variables) {
  
  # Extract data for each cluster
  group1 <- clean_data[[variable]][cog_df_cl$cluster == 1]
  group2 <- clean_data[[variable]][cog_df_cl$cluster == 2]
  
  # Run Shapiro-Wilk normality test on each cluster
  shapiro_1 <- shapiro.test(group1)
  shapiro_2 <- shapiro.test(group2)
  
  # Save results
  shapiro_per_cluster[[variable]] <- list(
    cluster_1 = list(
      W = round(shapiro_1$statistic, 4),
      p_value = round(shapiro_1$p.value, 4)
    ),
    cluster_2 = list(
      W = round(shapiro_2$statistic, 4),
      p_value = round(shapiro_2$p.value, 4)
    )
  )
  
  # Levene's Test for homogeneity of variances
  levene_result <- car::leveneTest(clean_data[[variable]] ~ as.factor(cog_df_cl$cluster))
  levene_results[[variable]] <- levene_result
}

shapiro_per_cluster
levene_results

#--------
# Cognitive variables winsorized Data

# Vector of variables for which to create boxplots
variables <- c("pvt_reaction_time_w", "nback_miss_1_w", "nback_miss_2_w", "tmt_a_time_w", "tmt_b_time_w","tmt_diff_w")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable
for (variable in variables) {
  # Create boxplot for the current variable grouped by cluster
  plot <- ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = !!sym(variable))) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  # Append the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)


# Initialize an empty list to store the test results
test_results <- list()

# Loop over each variable
for (variable in variables) {
  # Perform Wilcoxon rank sum test for the current variable
  test_result <- t.test(clean_data[[variable]] ~ as.factor(cog_df_cl$cluster), exact = FALSE)
  
  # Calculate descriptive statistics
  descriptive_stats <- clean_data %>%
    group_by(cog_df_cl$cluster) %>%
    summarise(
      mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
      sd = round(sd(!!sym(variable), na.rm = TRUE), 2)
    ) %>%
    mutate(
      mean = format(mean, nsmall = 2),
      sd = format(sd, nsmall = 2)
    )
  
  # Combine test results and descriptive statistics
  test_results[[variable]] <- list(
    test = test_result,
    descriptives = descriptive_stats
  )
}

# Display the test results
test_results


# Install and load the effsize package if not already installed
if (!requireNamespace("effsize", quietly = TRUE)) {
  install.packages("effsize")
}
library(effsize)

# Initialize an empty list to store the effect sizes
effect_sizes <- list()

# Loop over each variable to calculate effect sizes
for (variable in variables) {
  # Calculate Cohen's d for the current variable
  cohens_d <- cohen.d(clean_data[[variable]], as.factor(cog_df_cl$cluster))
  
  # Store the effect size in the list
  effect_sizes[[variable]] <- cohens_d$estimate
}

# Display the effect sizes
effect_sizes


# Vector of variables for which to create boxplots
variables <- c("pvt_reaction_time_w", "nback_miss_1_w", "nback_miss_2_w", "tmt_a_time_w", "tmt_b_time_w","tmt_diff_w")

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

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable
for (variable in variables) {
  # Create boxplot for the current variable grouped by cluster and fill by group
  plot <- ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  # Append the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)


# Initialize an empty list to store the test results for withPCS group
test_results_withPCS <- list()

# Initialize an empty list to store the test results for withoutPCS group
test_results_withoutPCS <- list()

# Loop over each variable
for (variable in variables) {
  # Perform t-test for the current variable within "withPCS" group
  t_test_result_withPCS <- t.test(clean_data[[variable]][clean_data$group == "self-reported CD" & cog_df_cl$cluster == 1],
                                  clean_data[[variable]][clean_data$group == "self-reported CD" & cog_df_cl$cluster == 2])
  
  # Perform t-test for the current variable within "withoutPCS" group
  t_test_result_withoutPCS <- t.test(clean_data[[variable]][clean_data$group == "no self-reported CD" & cog_df_cl$cluster == 1],
                                     clean_data[[variable]][clean_data$group == "no self-reported CD" & cog_df_cl$cluster == 2])
  
  # Store the test results for withPCS group in the list
  test_results_withPCS[[variable]] <- t_test_result_withPCS
  
  # Store the test results for withoutPCS group in the list
  test_results_withoutPCS[[variable]] <- t_test_result_withoutPCS
}

# Display the test results for withPCS group
test_results_withPCS

# Display the test results for withoutPCS group
test_results_withoutPCS


# Function to perform t-test for a single variable within each cluster
perform_t_test <- function(variable_name) {
  # Filter data for Cluster 1
  cluster_1_data <- subset(clean_data, cog_df_cl$cluster == 1)
  
  # Perform t-test for the variable between "withPCS" and "withoutPCS" groups within Cluster 1
  t_test_cluster_1 <- t.test(cluster_1_data[[variable_name]] ~ cluster_1_data$group)
  
  # Filter data for Cluster 2
  cluster_2_data <- subset(clean_data, cog_df_cl$cluster == 2)
  
  # Perform t-test for the variable between "withPCS" and "withoutPCS" groups within Cluster 2
  t_test_cluster_2 <- t.test(cluster_2_data[[variable_name]] ~ cluster_2_data$group)
  
  # Return t-test results as a list
  list(cluster_1 = t_test_cluster_1, cluster_2 = t_test_cluster_2)
}

# Map function over each variable to perform t-test
stats_t_test <- map(variables, perform_t_test)

# Display t-test results for each variable within each cluster
# Print results with variable names
for (i in seq_along(variables)) {
  cat("Variable:", variables[i], "\n")
  
  cat("Cluster 1:\n")
  cat("Mean in group no self-reported CD:", round(stats_t_test[[i]]$cluster_1$estimate[1], 2), "\n")
  cat("Mean in group self-reported CD:", round(stats_t_test[[i]]$cluster_1$estimate[2], 2), "\n")
  cat("p-value:", round(stats_t_test[[i]]$cluster_1$p.value, 4), "\n\n")
  
  cat("Cluster 2:\n")
  cat("Mean in group no self-reported CD:", round(stats_t_test[[i]]$cluster_2$estimate[1], 2), "\n")
  cat("Mean in group self-reported CD:", round(stats_t_test[[i]]$cluster_2$estimate[2], 2), "\n")
  cat("p-value:", round(stats_t_test[[i]]$cluster_2$p.value, 4), "\n\n")
}

#--------
# Cognitive variables standardized data

# Vector of variables for which to create boxplots
variables <- c("z_pvt_reaction_time_w", "s_nback_miss_1_w", "s_nback_miss_2_w", "z_tmt_a_time_w", "z_tmt_b_time_w","z_tmt_diff_w")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable
for (variable in variables) {
  # Create boxplot for the current variable grouped by cluster
  plot <- ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = !!sym(variable))) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  # Append the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)


# Initialize an empty list to store the test results
test_results <- list()

# Loop over each variable
for (variable in variables) {
  # Perform Wilcoxon rank sum test for the current variable
  test_result <- t.test(clean_data[[variable]] ~ as.factor(cog_df_cl$cluster), exact = FALSE)
  
  # Calculate descriptive statistics
  descriptive_stats <- clean_data %>%
    group_by(cog_df_cl$cluster) %>%
    summarise(
      mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
      sd = round(sd(!!sym(variable), na.rm = TRUE), 2)
    ) %>%
    mutate(
      mean = format(mean, nsmall = 2),
      sd = format(sd, nsmall = 2)
    )
  
  # Combine test results and descriptive statistics
  test_results[[variable]] <- list(
    test = test_result,
    descriptives = descriptive_stats
  )
}

# Display the test results
test_results


# Install and load the effsize package if not already installed
if (!requireNamespace("effsize", quietly = TRUE)) {
  install.packages("effsize")
}
library(effsize)

# Initialize an empty list to store the effect sizes
effect_sizes <- list()

# Loop over each variable to calculate effect sizes
for (variable in variables) {
  # Calculate Cohen's d for the current variable
  cohens_d <- cohen.d(clean_data[[variable]], as.factor(cog_df_cl$cluster))
  
  # Store the effect size in the list
  effect_sizes[[variable]] <- cohens_d$estimate
}

# Display the effect sizes
effect_sizes


# Vector of variables for which to create boxplots
variables <- c("z_pvt_reaction_time_w", "s_nback_miss_1_w", "s_nback_miss_2_w", "z_tmt_a_time_w", "z_tmt_b_time_w","z_tmt_diff_w")

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

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable
for (variable in variables) {
  # Create boxplot for the current variable grouped by cluster and fill by group
  plot <- ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  # Append the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)


# Initialize an empty list to store the test results for withPCS group
test_results_withPCS <- list()

# Initialize an empty list to store the test results for withoutPCS group
test_results_withoutPCS <- list()

# Loop over each variable
for (variable in variables) {
  # Perform t-test for the current variable within "withPCS" group
  t_test_result_withPCS <- t.test(clean_data[[variable]][clean_data$group == "self-reported CD" & cog_df_cl$cluster == 1],
                                  clean_data[[variable]][clean_data$group == "self-reported CD" & cog_df_cl$cluster == 2])
  
  # Perform t-test for the current variable within "withoutPCS" group
  t_test_result_withoutPCS <- t.test(clean_data[[variable]][clean_data$group == "no self-reported CD" & cog_df_cl$cluster == 1],
                                     clean_data[[variable]][clean_data$group == "no self-reported CD" & cog_df_cl$cluster == 2])
  
  # Store the test results for withPCS group in the list
  test_results_withPCS[[variable]] <- t_test_result_withPCS
  
  # Store the test results for withoutPCS group in the list
  test_results_withoutPCS[[variable]] <- t_test_result_withoutPCS
}

# Display the test results for withPCS group
test_results_withPCS

# Display the test results for withoutPCS group
test_results_withoutPCS


# Function to perform t-test for a single variable within each cluster
perform_t_test <- function(variable_name) {
  # Filter data for Cluster 1
  cluster_1_data <- subset(clean_data, cog_df_cl$cluster == 1)
  
  # Perform t-test for the variable between "withPCS" and "withoutPCS" groups within Cluster 1
  t_test_cluster_1 <- t.test(cluster_1_data[[variable_name]] ~ cluster_1_data$group)
  
  # Filter data for Cluster 2
  cluster_2_data <- subset(clean_data, cog_df_cl$cluster == 2)
  
  # Perform t-test for the variable between "withPCS" and "withoutPCS" groups within Cluster 2
  t_test_cluster_2 <- t.test(cluster_2_data[[variable_name]] ~ cluster_2_data$group)
  
  # Return t-test results as a list
  list(cluster_1 = t_test_cluster_1, cluster_2 = t_test_cluster_2)
}

# Map function over each variable to perform t-test
stats_t_test <- map(variables, perform_t_test)

# Display t-test results for each variable within each cluster
# Print results with variable names
for (i in seq_along(variables)) {
  cat("Variable:", variables[i], "\n")
  
  cat("Cluster 1:\n")
  cat("Mean in group no self-reported CD:", round(stats_t_test[[i]]$cluster_1$estimate[1], 2), "\n")
  cat("Mean in group self-reported CD:", round(stats_t_test[[i]]$cluster_1$estimate[2], 2), "\n")
  cat("p-value:", round(stats_t_test[[i]]$cluster_1$p.value, 4), "\n\n")
  
  cat("Cluster 2:\n")
  cat("Mean in group no self-reported CD:", round(stats_t_test[[i]]$cluster_2$estimate[1], 2), "\n")
  cat("Mean in group self-reported CD:", round(stats_t_test[[i]]$cluster_2$estimate[2], 2), "\n")
  cat("p-value:", round(stats_t_test[[i]]$cluster_2$p.value, 4), "\n\n")
}

#--------
## Questionairs

# Vector of variables for which to create boxplots
new_variables <- c("facit_f_FS", "hads_a_total_score", "hads_d_total_score", "psqi_total_score")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each new variable
for (variable in new_variables) {
  # Create boxplot for the current variable grouped by cluster
  plot <- ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = !!sym(variable))) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  # Append the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

# Initialize an empty list to store the test results (Signifikanztest)
test_results <- list()

# Loop over each variable
for (variable in new_variables) {
  # Perform Wilcoxon rank sum test for the current variable
  test_result <- t.test(clean_data[[variable]] ~ as.factor(cog_df_cl$cluster), exact = FALSE)
  
  # Calculate descriptive statistics
  descriptive_stats <- clean_data %>%
    group_by(cog_df_cl$cluster) %>%
    summarise(
      mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
      sd = round(sd(!!sym(variable), na.rm = TRUE), 2)
    ) %>%
    mutate(
      mean = format(mean, nsmall = 2),
      sd = format(sd, nsmall = 2)
    )
  
  # Combine test results and descriptive statistics
  test_results[[variable]] <- list(
    test = test_result,
    descriptives = descriptive_stats
  )
}

# Display the test results
test_results

# Vector of variables for which to create boxplots
new_variables <- c("facit_f_FS", "hads_a_total_score", "hads_d_total_score", "psqi_total_score")

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

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each variable
for (variable in new_variables) {
  # Create boxplot for the current variable grouped by cluster and fill by group
  plot <- ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster"))
  
  # Append the plot to the list
  plot_list[[variable]] <- plot
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)


# Initialize an empty list to store the test results for withPCS group
test_results_withPCS <- list()

# Initialize an empty list to store the test results for withoutPCS group
test_results_withoutPCS <- list()

# Loop over each variable
for (variable in new_variables) {
  # Perform t-test for the current variable within "withPCS" group
  t_test_result_withPCS <- t.test(clean_data[[variable]][clean_data$group == "self-reported CD" & cog_df_cl$cluster == 1],
                                  clean_data[[variable]][clean_data$group == "self-reported CD" & cog_df_cl$cluster == 2])
  
  # Perform t-test for the current variable within "withoutPCS" group
  t_test_result_withoutPCS <- t.test(clean_data[[variable]][clean_data$group == "no self-reported CD" & cog_df_cl$cluster == 1],
                                     clean_data[[variable]][clean_data$group == "no self-reported CD" & cog_df_cl$cluster == 2])
  
  # Store the test results for withPCS group in the list
  test_results_withPCS[[variable]] <- t_test_result_withPCS
  
  # Store the test results for withoutPCS group in the list
  test_results_withoutPCS[[variable]] <- t_test_result_withoutPCS
}

# Display the test results for withPCS group
test_results_withPCS

# Display the test results for withoutPCS group
test_results_withoutPCS


# Function to perform t-test for a single variable within each cluster
perform_t_test <- function(new_variable_name) {
  # Filter data for Cluster 1
  cluster_1_data <- subset(clean_data, cog_df_cl$cluster == 1)
  
  # Perform t-test for the variable between "withPCS" and "withoutPCS" groups within Cluster 1
  t_test_cluster_1 <- t.test(cluster_1_data[[new_variable_name]] ~ cluster_1_data$group)
  
  # Filter data for Cluster 2
  cluster_2_data <- subset(clean_data, cog_df_cl$cluster == 2)
  
  # Perform t-test for the variable between "withPCS" and "withoutPCS" groups within Cluster 2
  t_test_cluster_2 <- t.test(cluster_2_data[[new_variable_name]] ~ cluster_2_data$group)
  
  # Return t-test results as a list
  list(cluster_1 = t_test_cluster_1, cluster_2 = t_test_cluster_2)
}

# Map function over each variable to perform t-test
stats_t_test <- map(new_variables, perform_t_test)

# Display t-test results for each variable within each cluster
# Print results with variable names
for (i in seq_along(new_variables)) {
  cat("Variable:", new_variables[i], "\n")
  
  cat("Cluster 1:\n")
  cat("Mean in group no self-reported CD:", round(stats_t_test[[i]]$cluster_1$estimate[1], 2), "\n")
  cat("Mean in group self-reported CD:", round(stats_t_test[[i]]$cluster_1$estimate[2], 2), "\n")
  cat("p-value:", round(stats_t_test[[i]]$cluster_1$p.value, 4), "\n\n")
  
  cat("Cluster 2:\n")
  cat("Mean in group no self-reported CD:", round(stats_t_test[[i]]$cluster_2$estimate[1], 2), "\n")
  cat("Mean in group self-reported CD:", round(stats_t_test[[i]]$cluster_2$estimate[2], 2), "\n")
  cat("p-value:", round(stats_t_test[[i]]$cluster_2$p.value, 4), "\n\n")
}

# =============================
# 📋 Questionnaire Assumptions Check
# =============================

# Define questionnaire variables
new_variables <- c("facit_f_FS", "hads_a_total_score", "hads_d_total_score", "psqi_total_score")

# Initialize result containers
descriptive_stats_q <- list()
normality_results_q <- list()
homogeneity_results_q <- list()

# Loop over each questionnaire variable
for (variable in new_variables) {
  
  # Descriptive stats by cluster
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
  descriptive_stats_q[[variable]] <- descriptive_stats
  
  # Shapiro-Wilk normality test on residuals
  shapiro_test <- shapiro.test(residuals(lm(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)))
  normality_results_q[[variable]] <- shapiro_test
  
  # Levene’s test for homogeneity of variances
  levene_test <- car::leveneTest(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  homogeneity_results_q[[variable]] <- levene_test
}

# View results
descriptive_stats_q
normality_results_q
homogeneity_results_q

# =============================
#  Per Cluster Normality + Levene Test
# =============================

shapiro_per_cluster_q <- list()
levene_results_q <- list()

for (variable in new_variables) {
  
  # Extract cluster-wise vectors
  group1 <- clean_data[[variable]][cog_df_cl$cluster == 1]
  group2 <- clean_data[[variable]][cog_df_cl$cluster == 2]
  
  # Run Shapiro tests per cluster
  shapiro_1 <- shapiro.test(group1)
  shapiro_2 <- shapiro.test(group2)
  
  # Store results
  shapiro_per_cluster_q[[variable]] <- list(
    cluster_1 = list(
      W = round(shapiro_1$statistic, 4),
      p_value = round(shapiro_1$p.value, 4)
    ),
    cluster_2 = list(
      W = round(shapiro_2$statistic, 4),
      p_value = round(shapiro_2$p.value, 4)
    )
  )
  
  # Levene’s test for variance homogeneity
  levene_result <- car::leveneTest(clean_data[[variable]] ~ as.factor(cog_df_cl$cluster))
  levene_results_q[[variable]] <- levene_result
}

# View per-cluster Shapiro and Levene results
shapiro_per_cluster_q
levene_results_q

#-------
  
# Build distance matrix
# Since all values are continuous numerical values I use euclidean distance method
#dist_mat <- dist(cog_df, method = 'euclidean')

# Now decide which linkage method to use
# Try different kinds of linkage methods after decide which performed better
# Build dendrogram by plotting hierarchical cluster object with hclust
# Specify linkage method via 'method' argument
#hclust_median <- hclust(dist_mat, method = 'median')
#plot(hclust_median)

# Create the desired number of clusters
# Since I want two groups 'withPCS' and 'withoutPCS' number of clusters = 2
#cut_median <- cutree(hclust_median, k = 2)
# To visualize clusters on dendrogram use abline function to draw the cut line
#plot(hclust_median)
#rect.hclust(hclust_median, k = 2, border = 2:40)
#abline(h = 28, col = 'red')
# Visualize tree with different colored branches
#median_dend_obj <- as.dendrogram(hclust_median)
#median_col_dend <- color_branches(median_dend_obj, h = 28)
#plot(median_col_dend)

# Visualize the clusters see YT Video Hierarchical Clustering in R Spencer Pao
# install.packages("factoextra")
# library(factoextra)
# cluster_obj <- list(data = cog_df_sc, cluster = cut_avg)
# fviz_cluster(cluster_obj)
# rownames(cog_df_sc) <- paste(cog_label, 1:dim(cog_df) [1], sep = "_")
# fviz_cluster(list(data=cog_df_sc))

# Append cluster results obtained back in the original dataframe 
# Use mutate
# Count how many observations were assigned to each cluster with the count function
#cog_df_cl <- mutate(cog_df, cluster = cut_median)
#count(cog_df_cl,cluster)

# Cross-checking clustering results using table funcion
#table(cog_df_cl$cluster,cog_label)

# Add the cluster information from cog_df_cl to clean_data
#clean_data$cluster <- cog_df_cl$cluster

# Check the updated structure of clean_data
#str(clean_data)

#------------

#library(fossil)

# Calculate the Adjusted Rand Index
#ari_median_ward <- rand.index(cut_median, cut_ward)

# Print the ARI
#print(ari_median_ward)


#-------

# Build distance matrix
# Since all values are continuous numerical values I use euclidean distance method
#dist_mat_cityblock <- dist(cog_df, method = 'euclidean')

# Now decide which linkage method to use
# Try different kinds of linkage methods after decide which performed better
# Build dendrogram by plotting hierarchical cluster object with hclust
# Specify linkage method via 'method' argument
#hclust_city <- hclust(dist_mat_cityblock, method = 'ward')
#plot(hclust_city)

# Create the desired number of clusters
# Since I want two groups 'withPCS' and 'withoutPCS' number of clusters = 2
#cut_city <- cutree(hclust_city, k = 2)

#------------

#library(fossil)

# Calculate the Adjusted Rand Index
#ari_city_ward <- rand.index(cut_city, cut_ward)

# Print the ARI
#print(ari_city_ward)

#-----------

# Load necessary libraries
#library(pheatmap)

# Add cluster information to the data and order by clusters
#cog_df$cluster <- as.factor(cut_ward)
#cog_df <- cog_df[order(cog_df$cluster), ]

# Create a data frame for annotations to add cluster information to the heatmap
#annotation <- data.frame(cluster = cog_df$cluster)
#row.names(annotation) <- row.names(cog_df)

# Reorder the rows to have all Cluster 1 first followed by Cluster 2
#cog_df <- cog_df[order(cog_df$cluster), ]
#annotation <- annotation[order(annotation$cluster), , drop = FALSE]

# Remove the cluster column for the heatmap plot
#cog_df$cluster <- NULL

# Plot the heatmap without clustering the columns
#pheatmap(cog_df, 
         #annotation_row = annotation,
         #main = "Heatmap of Cognitive Measures by Cluster",
         #cluster_cols = FALSE,   # Disable clustering of columns
         #scale = "row",          # Scale each row (feature) for better visualization
         #color = colorRampPalette(c("navy", "white", "firebrick3"))(50))

#-------------

# Add cluster information to the data and order by clusters
#cog_df$cluster <- as.factor(cut_ward)
#cog_df <- cog_df[order(cog_df$cluster), ]

# Create a data frame for annotations to add cluster information to the heatmap
#annotation <- data.frame(cluster = cog_df$cluster)
#row.names(annotation) <- row.names(cog_df)

# Remove the cluster column for the heatmap plot
#cog_df$cluster <- NULL

# Define custom color palette
#custom_palette <- colorRampPalette(c("blue", "white", "red"))(50)

# Plot the heatmap with custom color palette and sorted by clusters
#pheatmap(cog_df, 
         #annotation_row = annotation,
         #main = "Heatmap of Cognitive Measures by Cluster",
         #cluster_rows = FALSE,  # Disable clustering of rows
         #cluster_cols = FALSE,  # Disable clustering of columns
         #scale = "row",         # Scale each row (feature) for better visualization
         #color = custom_palette)  # Use custom color palette

#-----------

# Sort data matrix by the color gradient
#sorted_cog_df <- cog_df[order(rowMeans(cog_df)), ]

# Add cluster information to the sorted data
#sorted_cog_df$cluster <- as.factor(cut_ward)
#sorted_cog_df <- sorted_cog_df[order(sorted_cog_df$cluster), ]

# Create a data frame for annotations to add cluster information to the heatmap
#annotation <- data.frame(cluster = sorted_cog_df$cluster)
#rownames(annotation) <- rownames(sorted_cog_df)

# Remove the cluster column for the heatmap plot
#sorted_cog_df$cluster <- NULL

# Plot the heatmap with custom color palette and sorted by color gradient
#pheatmap(sorted_cog_df, 
         #annotation_row = annotation,
         #main = "Heatmap of Cognitive Measures by Cluster",
         #cluster_rows = FALSE,  # Disable clustering of rows
         #cluster_cols = FALSE,  # Disable clustering of columns
         #scale = "row",         # Scale each row (feature) for better visualization
         #color = custom_palette)  # Use custom color palette

#----------------

# Select the variable of interest
#variable <- cog_df$z_pvt_reaction_time

# Add cluster information to the data and order by clusters
#variable_cluster <- data.frame(variable, cluster = as.factor(cut_ward))
#variable_cluster <- variable_cluster[order(variable_cluster$cluster), ]

# Create a data frame for annotations to add cluster information to the heatmap
#annotation <- data.frame(cluster = variable_cluster$cluster)
#row.names(annotation) <- row.names(variable_cluster)

# Remove the cluster column for the heatmap plot
#variable_cluster$cluster <- NULL

# Define custom color palette
#custom_palette <- colorRampPalette(c("blue", "white", "red"))(50)

# Plot the heatmap with custom color palette and sorted by clusters
#pheatmap(t(variable_cluster), 
         #annotation_row = annotation,
         #main = "Heatmap of z_pvt_reaction_time by Cluster",
         #cluster_rows = FALSE,  # Disable clustering of rows
         #cluster_cols = FALSE,  # Disable clustering of columns
         #scale = "row",         # Scale each row (feature) for better visualization
         #color = custom_palette)  # Use custom color palette

