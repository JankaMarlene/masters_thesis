# Hierarchical clustering
library(tidyverse)
library(dplyr)
library(dendextend)
library(ggplot2)
library(gridExtra)
load("clean_data.RData")

# Extract relevant columns from clean_data
cog_df <- clean_data[, c("group","z_pvt_reaction_time","s_nback_miss_1","s_nback_miss_2","z_tmt_a_time","z_tmt_b_time")]
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
cut_ward <- cutree(hclust_ward, k = 2)
# To visualize clusters on dendrogram use abline function to draw the cut line
plot(hclust_ward)
rect.hclust(hclust_ward, k = 2, border = 2:40)
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

# Cross-checking clustering results using table funcion
table(cog_df_cl$cluster,cog_label)

# Add the cluster information from cog_df_cl to clean_data
clean_data$cluster <- cog_df_cl$cluster

# Check the updated structure of clean_data
str(clean_data)

# Checking sex in cluster
table(cog_df_cl$cluster,clean_data$sex)

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


# Plotting age distribution within clusters based on withPCS and withoutPCS labels with mean as text
ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = age, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) + # Adjust position of boxplots
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), # Adjust position of points
               shape = 18, size = 4, color = "red") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), 
               position = position_dodge(width = 0.75), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "age", title = "Age Distribution within Clusters based on withPCS and withoutPCS Labels")

# Perform t-test for age within "withPCS" group between clusters
t_test_withPCS <- t.test(age ~ as.factor(cluster), data = subset(clean_data, group == "withPCS"))

# Perform t-test for age within "withoutPCS" group between clusters
t_test_withoutPCS <- t.test(age ~ as.factor(cluster), data = subset(clean_data, group == "withoutPCS"))

# Display t-test results for "withPCS" group
t_test_withPCS

# Display t-test results for "withoutPCS" group
t_test_withoutPCS

# Filter the data for Cluster 2
cluster_2_data <- subset(clean_data, cluster == 2)

# Perform t-test for facit_f_FS between "withPCS" and "withoutPCS" groups within Cluster 2
t_test_cluster_2 <- t.test(age ~ group, data = cluster_2_data)

# Display t-test results
t_test_cluster_2




# Vector of variables for which to create boxplots
variables <- c("pvt_reaction_time", "nback_miss_1", "nback_miss_2", "tmt_a_time", "tmt_b_time")

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


# Initialize an empty list to store the test results (Signifikanztest)
test_results <- list()

# Loop over each variable
for (variable in variables) {
  # Perform Wilcoxon rank sum test for the current variable
  test_result <- t.test(clean_data[[variable]] ~ as.factor(cog_df_cl$cluster), exact = FALSE)
  
  # Store the test result in the list
  test_results[[variable]] <- test_result
}

# Display the test results
test_results


# Vector of variables for which to create boxplots
variables <- c("pvt_reaction_time", "nback_miss_1", "nback_miss_2", "tmt_a_time", "tmt_b_time")

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
  t_test_result_withPCS <- t.test(clean_data$age[clean_data$group == "withPCS"], clean_data[[variable]][clean_data$group == "withPCS"])
  
  # Perform t-test for the current variable within "withoutPCS" group
  t_test_result_withoutPCS <- t.test(clean_data$age[clean_data$group == "withoutPCS"], clean_data[[variable]][clean_data$group == "withoutPCS"])
  
  # Store the test results for withPCS group in the list
  test_results_withPCS[[variable]] <- t_test_result_withPCS
  
  # Store the test results for withoutPCS group in the list
  test_results_withoutPCS[[variable]] <- t_test_result_withoutPCS
}

# Display the test results for withPCS group
test_results_withPCS

# Display the test results for withoutPCS group
test_results_withoutPCS


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
  
  # Store the test result in the list
  test_results[[variable]] <- test_result
}

# Display the test results
test_results


# Vector of variables for which to create boxplots
new_variables <- c("facit_f_FS", "hads_a_total_score", "hads_d_total_score", "psqi_total_score")

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over each new variable
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

# Perform t-test for facit_f_FS within "withPCS" group between clusters
t_test_withPCS <- t.test(facit_f_FS ~ as.factor(cluster), data = subset(clean_data, group == "withPCS"))

# Perform t-test for facit_f_FS within "withoutPCS" group between clusters
t_test_withoutPCS <- t.test(facit_f_FS ~ as.factor(cluster), data = subset(clean_data, group == "withoutPCS"))

# Display t-test results for "withPCS" group
t_test_withPCS

# Display t-test results for "withoutPCS" group
t_test_withoutPCS


# Filter the data for Cluster 1
cluster_1_data <- subset(clean_data, cluster == 1)

# Perform t-test for facit_f_FS between "withPCS" and "withoutPCS" groups within Cluster 1
t_test_cluster_1 <- t.test(facit_f_FS ~ group, data = cluster_1_data)

# Display t-test results
t_test_cluster_1


# Filter the data for Cluster 2
cluster_2_data <- subset(clean_data, cluster == 2)

# Perform t-test for facit_f_FS between "withPCS" and "withoutPCS" groups within Cluster 2
t_test_cluster_2 <- t.test(facit_f_FS ~ group, data = cluster_2_data)

# Display t-test results
t_test_cluster_2

#-------
  
# Build distance matrix
# Since all values are continuous numerical values I use euclidean distance method
dist_mat <- dist(cog_df, method = 'euclidean')

# Now decide which linkage method to use
# Try different kinds of linkage methods after decide which performed better
# Build dendrogram by plotting hierarchical cluster object with hclust
# Specify linkage method via 'method' argument
hclust_median <- hclust(dist_mat, method = 'median')
plot(hclust_median)

# Create the desired number of clusters
# Since I want two groups 'withPCS' and 'withoutPCS' number of clusters = 2
cut_median <- cutree(hclust_median, k = 2)
# To visualize clusters on dendrogram use abline function to draw the cut line
plot(hclust_median)
rect.hclust(hclust_median, k = 2, border = 2:40)
abline(h = 28, col = 'red')
# Visualize tree with different colored branches
median_dend_obj <- as.dendrogram(hclust_median)
median_col_dend <- color_branches(median_dend_obj, h = 28)
plot(median_col_dend)

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
cog_df_cl <- mutate(cog_df, cluster = cut_median)
count(cog_df_cl,cluster)

# Cross-checking clustering results using table funcion
table(cog_df_cl$cluster,cog_label)

# Add the cluster information from cog_df_cl to clean_data
clean_data$cluster <- cog_df_cl$cluster

# Check the updated structure of clean_data
str(clean_data)

#------------

library(fossil)

# Calculate the Adjusted Rand Index
ari_median_ward <- adj.rand.index(cut_median, cut_ward)

# Print the ARI
print(ari_median_ward)
