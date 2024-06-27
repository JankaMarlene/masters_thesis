# Hierarchical clustering
library(tidyverse)
library(dplyr)
library(dendextend)
library(ggplot2)
library(gridExtra)
library(purrr)
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
cluster_1_data <- subset(clean_data, cog_df_cl$cluster == 1)
anova_cluster_1 <- aov(age ~ group, data = cluster_1_data)
summary(anova_cluster_1)

# Filter the data for Cluster 1
cluster_1_data <- subset(clean_data, cluster == 1)

# Perform t-test for age between "withPCS" and "withoutPCS" groups within Cluster 1
t_test_cluster_1 <- t.test(age ~ group, data = cluster_1_data)

# Display t-test results
t_test_cluster_1

# Cluster 2
cluster_2_data <- subset(clean_data, cog_df_cl$cluster == 2)
anova_cluster_2 <- aov(age ~ group, data = cluster_2_data)
summary(anova_cluster_2)

# Cluster 3
cluster_3_data <- subset(clean_data, cog_df_cl$cluster == 3)
anova_cluster_3 <- aov(age ~ group, data = cluster_3_data)
summary(anova_cluster_3)

# Cluster 4
cluster_4_data <- subset(clean_data, cog_df_cl$cluster == 4)
anova_cluster_4 <- aov(age ~ group, data = cluster_4_data)
summary(anova_cluster_4)

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

# Perform ANOVA for years of education between "withPCS" and "withoutPCS" groups within each cluster

# Cluster 1
cluster_1_data <- subset(clean_data, cluster == 1)
anova_cluster_1 <- aov(years_of_education ~ group, data = cluster_1_data)
summary(anova_cluster_1)

# Cluster 2
cluster_2_data <- subset(clean_data, cluster == 2)
anova_cluster_2 <- aov(years_of_education ~ group, data = cluster_2_data)
summary(anova_cluster_2)

# Cluster 3
cluster_3_data <- subset(clean_data, cluster == 3)
anova_cluster_3 <- aov(years_of_education ~ group, data = cluster_3_data)
summary(anova_cluster_3)

# Cluster 4
cluster_4_data <- subset(clean_data, cluster == 4)
anova_cluster_4 <- aov(years_of_education ~ group, data = cluster_4_data)
summary(anova_cluster_4)

