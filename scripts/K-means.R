# K-means
library(tidyverse)
library(dplyr)
library(dendextend)
library(ggplot2)
library(gridExtra)
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

## k-means
set.seed(123)
km.out <- kmeans(cog_df, centers = 4, nstart = 20)
km.out

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(cog_df, centers = i, nstart = 20)
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

# Cluster solution for 4 clusters
# Select number of clusters
k <- 4
set.seed(123)

# Build model with k clusters: km.out
km.out <- kmeans(cog_df, centers = k, nstart = 20)

# Append cluster results obtained back in the original dataframe
cog_df_cl <- mutate(cog_df, cluster = km.out$cluster)

# Count how many observations were assigned to each cluster with the count function
count(cog_df_cl, cluster)

# Cross-checking clustering results using table function
table(cog_df_cl$cluster, cog_label)


# Prepare data (count of observations in each cluster for each label category)
cluster_label_counts <- table(cog_label, cog_df_cl$cluster)

# Create bar plot with custom y-axis range and ticks
barplot(cluster_label_counts, beside = TRUE, col = c("blue", "red"), 
        main = "Cluster Assignments by Label Category", xlab = "Cluster", ylab = "Count",
        ylim = c(0, 20),
        axes = FALSE)

# Add y-axis with custom ticks and intermediate tick marks
axis(side = 2, at = seq(0, 20, by = 1), labels = FALSE)  
axis(side = 2, at = seq(0, 20, by = 5), labels = seq(0, 20, by = 5), tcl = -1)  

# Add legend
legend("topleft", legend = c("withoutPCS", "withPCS"), fill = c("blue", "red"))


# Plotting age distribution between clusters with mean as text
ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = age)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "age", title = "Age Distribution between Clusters")

# Plotting age distribution within clusters based on withPCS and withoutPCS labels with mean as text
ggplot(clean_data, aes(x = as.factor(cog_df_cl$cluster), y = age, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) + # Adjust position of boxplots
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), # Adjust position of points
               shape = 18, size = 4, color = "red") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), 
               position = position_dodge(width = 0.75), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "age", title = "Age Distribution within Clusters based on withPCS and withoutPCS Labels")

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

# Cluster solution for 2 clusters
# Select number of clusters
k <- 2
set.seed(123)

# Build model with k clusters: km.out
km.out <- kmeans(cog_df, centers = k, nstart = 20)

# Append cluster results obtained back in the original dataframe
cog_df_cl <- mutate(cog_df, cluster = km.out$cluster)

# Count how many observations were assigned to each cluster with the count function
count(cog_df_cl, cluster)

# Cross-checking clustering results using table function
table(cog_df_cl$cluster, cog_label)

# Prepare data (count of observations in each cluster for each label category)
cluster_label_counts <- table(cog_label, cog_df_cl$cluster)

# Create bar plot with custom y-axis range and ticks
barplot(cluster_label_counts, beside = TRUE, col = c("blue", "red"), 
        main = "Cluster Assignments by Label Category", xlab = "Cluster", ylab = "Count",
        ylim = c(0, 25),
        axes = FALSE)

# Add y-axis with custom ticks and intermediate tick marks
axis(side = 2, at = seq(0, 25, by = 1), labels = FALSE)  
axis(side = 2, at = seq(0, 25, by = 5), labels = seq(0, 25, by = 5), tcl = -1)  

# Add legend
legend("top", legend = c("withoutPCS", "withPCS"), fill = c("blue", "red"),
       bty = "n", xpd = TRUE, inset = c(0, 1.05))
