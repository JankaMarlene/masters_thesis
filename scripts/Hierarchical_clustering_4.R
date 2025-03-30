# Hierarchical clustering
library(tidyverse)
library(dplyr)
library(dendextend)
library(ggplot2)
library(gridExtra)
library(car)
library(readr)
library(ggdist)
library(ggExtra)# displaying distributions next to plots
library(ggsignif)# displaying stats in plots
library(ggpubr)
library(coin)# need this for z value of wilcox test
library(effectsize)
library(effsize)# for cohens d
library(backports) 
library(rstatix)# for wilcox test
library(purrr)
library(FSA)

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
cut_ward <- cutree(hclust_ward, k = 4)
# To visualize clusters on dendrogram use abline function to draw the cut line
plot(hclust_ward)
rect.hclust(hclust_ward, k = 4, border = 2:30)
abline(h = 10, col = 'red')

# Visualize tree with different colored branches
ward_dend_obj <- as.dendrogram(hclust_ward)
# Cut into 2 clusters
dend_cut <- cutree(hclust_ward, k = 4)

# Assign cluster labels to dendrogram
ward_col_dend <- color_branches(ward_dend_obj, k = 4)
# Get cluster order to assign colors properly
labels_ordered <- labels(ward_col_dend)
cluster_ordered <- dend_cut[labels_ordered]

# Create a vector of colors corresponding to cluster
cluster_colors <- c(
  "1" = "#F59541",   # Green
  "2" = "#4CAF50",   # Orange
  "3" = "#B589D6",   # Purple
  "4" = "#F5418C"    # Pink
)

#custom_colors <- cluster_colors[as.character(cluster_ordered)]

# Apply custom colors to branches
ward_col_dend <- color_branches(ward_dend_obj, k = 4,
                                col = cluster_colors)

plot(ward_col_dend,
     ylab = "Height",
     ylim = c(0, 40 + 2)) 

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



# Statistical comparison
kruskal.test(facit_f_FS ~ group_combined, data = clean_data)

dunnTest(facit_f_FS ~ group_combined, data = clean_data, method = "bonferroni")


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
  labs(x = "Cluster", y = "Age", title = "Age Distribution within Clusters")

# Perform ANOVA for age within "withPCS" group between clusters
anova_withPCS <- aov(age ~ as.factor(cluster), data = subset(clean_data, group == "self-reported CD"))

# Perform ANOVA for age within "withoutPCS" group between clusters
anova_withoutPCS <- aov(age ~ as.factor(cluster), data = subset(clean_data, group == "no self-reported CD"))

# Display ANOVA results for "withPCS" group
summary(anova_withPCS)

# Display ANOVA results for "withoutPCS" group
summary(anova_withoutPCS)

shapiro_age <- shapiro.test(residuals(lm(age ~ as.factor(cog_df_cl$cluster), data = clean_data)))
print(shapiro_age)
shapiro.test(residuals(lm(age ~ as.factor(cluster), data = clean_data[clean_data$group == "self-reported CD", ])))
shapiro.test(residuals(lm(age ~ as.factor(cluster), data = clean_data[clean_data$group == "no self-reported CD", ])))

leveneTest(age ~ as.factor(cog_df_cl$cluster), data = clean_data)
leveneTest(age ~ as.factor(cluster), data = subset(clean_data, group == "self-reported CD"))
leveneTest(age ~ as.factor(cluster), data = subset(clean_data, group == "no self-reported CD"))


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
anova_withPCS_education <- aov(years_of_education ~ as.factor(cluster), data = subset(clean_data, group == "self-reported CD"))

# Display ANOVA results for the "withPCS" group
summary(anova_withPCS_education)

# Perform ANOVA for years of education within the "withoutPCS" group between clusters
anova_withoutPCS_education <- aov(years_of_education ~ as.factor(cluster), data = subset(clean_data, group == "no self-reported CD"))

# Display ANOVA results for the "withoutPCS" group
summary(anova_withoutPCS_education)

# Plotting years of education distribution within clusters based on withPCS and withoutPCS labels with mean as text
ggplot(clean_data, aes(x = as.factor(cluster), y = years_of_education, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) + # Adjust position of boxplots
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), # Adjust position of points
               shape = 18, size = 4, color = "red") + # Add mean point
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 1)), 
               position = position_dodge(width = 0.75), vjust = -0.5) + # Add mean as text
  labs(x = "Cluster", y = "Years of Education", title = "Years of Education Distribution within Clusters based on self-reported CD")

shapiro_education <- shapiro.test(residuals(lm(years_of_education ~ as.factor(cog_df_cl$cluster), data = clean_data)))
print(shapiro_education)
shapiro.test(residuals(lm(years_of_education ~ as.factor(cluster), data = clean_data[clean_data$group == "self-reported CD", ])))
shapiro.test(residuals(lm(years_of_education ~ as.factor(cluster), data = clean_data[clean_data$group == "no self-reported CD", ])))

leveneTest(years_of_education ~ as.factor(cog_df_cl$cluster), data = clean_data)
leveneTest(years_of_education ~ as.factor(cluster), data = subset(clean_data, group == "self-reported CD"))
leveneTest(years_of_education ~ as.factor(cluster), data = subset(clean_data, group == "no self-reported CD"))

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
# Cognitive variables actual values - Test for normality, etc. ANOVA (Cannot use ANOVA)

# Vector of cognitive variables
variables <- c("pvt_reaction_time", "nback_miss_1", "nback_miss_2", "tmt_a_time", "tmt_b_time", "tmt_diff")

# Initialize an empty list to store the plots
plot_list <- list()


# Your 4-group color palette uuuuuussseeed!!!!
color_palette <- c(
  "1" = "#F5418C",
  "2" = "#F59541",
  "3" = "#B589D6",
  "4" = "#4CAF50"
)



clean_data$group_combined <- factor(clean_data$group_combined, levels = names(color_palette))

plot_list <- list()

for (variable in variables) {
  
  # Clean: remove NAs for this variable
  data_subset <- clean_data %>%
    select(group_combined, !!sym(variable)) %>%
    filter(!is.na(.[[2]]))
  
  if (nrow(data_subset) < 2) next
  
  # Run Dunn test
  dunn_result <- tryCatch({
    dunnTest(x = data_subset[[variable]], g = data_subset$group_combined, method = "bonferroni")
  }, error = function(e) return(NULL))
  
  significant_comparisons <- list()
  p_values <- c()
  
  if (!is.null(dunn_result)) {
    dunn_df <- dunn_result$res
    sig_df <- dunn_df %>% filter(P.adj < 0.05)
    
    if (nrow(sig_df) > 0) {
      significant_comparisons <- strsplit(as.character(sig_df$Comparison), " - ")
      p_values <- round(sig_df$P.adj, 3)
    }
  }
  
  # Plot range
  ylim_buffer <- max(data_subset[[variable]], na.rm = TRUE) * 0.2
  ymax <- max(data_subset[[variable]], na.rm = TRUE) + ylim_buffer
  
  plot <- ggplot(data_subset, aes(x = group_combined, y = .data[[variable]])) +
    geom_boxplot(aes(color = group_combined), fill = "white", outlier.shape = NA, width = 0.6, size = 0.9) +
    geom_jitter(aes(color = group_combined), width = 0.2, alpha = 0.6, size = 2) +
    scale_color_manual(values = color_palette) +
    labs(x = "", y = variable) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_ban(),
      text = element_text(size = 14)
    ) +
    coord_cartesian(ylim = c(NA, ymax * 1.15))
  
  
  # Add annotations only if there are significant results
  if (length(significant_comparisons) > 0) {
    plot <- plot +
      geom_signif(
        comparisons = significant_comparisons,
        annotations = sapply(p_values, function(p) {
          if (p < 0.001) return("***")
          else if (p < 0.01) return("**")
          else if (p < 0.05) return("*")
          else return()
        }),
        color = "black",
        textsize = 5,
        step_increase = 0.12
      )
  }
  
  plot_list[[variable]] <- plot
}

# Display all plots (regardless of significance)
if (length(plot_list) > 0) {
  grid.arrange(grobs = plot_list, ncol = 2)
} else {
  message("No valid plots to display.")
}









# Ensure cluster is a factor with correct levels
clean_data$cluster <- factor(clean_data$cluster, levels = names(color_palette))

# Initialize plot list
plot_list <- list()

for (variable in new_variables) {
  
  # Drop rows with NA in the current variable
  subset_data <- clean_data[!is.na(clean_data[[variable]]), ]
  
  # Only continue if at least 2 clusters have data
  if (length(unique(subset_data$cluster)) < 2) {
    message(paste("Skipping", variable, "- fewer than 2 clusters with data"))
    next
  }
  
  # Kruskal-Wallis test
  test <- kruskal.test(as.formula(paste(variable, "~ cluster")), data = subset_data)
  p_val <- signif(test$p.value, 3)
  
  # Set Y-limit with some buffer
  ylim_buffer <- max(subset_data[[variable]], na.rm = TRUE) * 0.2
  ymax <- max(subset_data[[variable]], na.rm = TRUE) + ylim_buffer
  
  # Boxplot
  plot <- ggplot(subset_data, aes(x = cluster, y = .data[[variable]], color = cluster)) +
    geom_boxplot(fill = "white", outlier.shape = NA, width = 0.6, size = 0.9) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    scale_color_manual(values = color_palette) +
    labs(x = "", y = variable) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = NA, angle = 45, hjust = 1),
      axis.ticks.x = element_line(),
      axis.title.x = element_blank(),
      text = element_text(size = 14)
    ) +
    coord_cartesian(ylim = c(NA, ymax * 1.15))
  
  plot_list[[variable]] <- plot
}

# Display all plots
grid.arrange(grobs = plot_list, ncol = 2)


# Cognitive variables in the four groups
clean_data <- clean_data %>%
  mutate(
    group_combined = paste(group, cluster, sep = "_")
  )

# Your 4-group color palette
color_palette <- c(
  "no self-reported CD_1" = "#FA8DB1",
  "self-reported CD_1" = "#C21C66",
  "no self-reported CD_2" = '#FDB57A',
  "self-reported CD_2" = '#D97700',
  "no self-reported CD_3" = "#D1A9EB",
  "self-reported CD_3" = "#8953B1",
  "no self-reported CD_4" = "#80D88A",
  "self-reported CD_4" = "#2E7D32"
  
)

# Make sure group_combined is a factor with correct levels
clean_data$group_combined <- factor(clean_data$group_combined, levels = names(color_palette))

# Initialize list for plots
plot_list <- list()

# Loop over variables
for (variable in new_variables) {
  
  # Get all pairwise combinations of groups
  pairwise_comparisons <- combn(levels(clean_data$group_combined), 2, simplify = FALSE)
  
  print(paste("Variable:", variable, "—", length(pairwise_comparisons), "pairwise comparisons"))
  
  # Run Wilcoxon tests
  p_values_raw <- sapply(pairwise_comparisons, function(groups) {
    wilcox.test(
      clean_data[[variable]][clean_data$group_combined == groups[1]],
      clean_data[[variable]][clean_data$group_combined == groups[2]],
      exact = FALSE
    )$p.value
  })
  
  
  # Bonferroni correction
  p_adj_bonf <- p.adjust(p_values_raw, method = "bonferroni")
  
  # Use consistent index
  sig_idx <- which(!is.na(p_adj_bonf) & p_adj_bonf < 0.05)
  significant_comparisons <- pairwise_comparisons[sig_idx]
  p_values <- p_adj_bonf[sig_idx]
  
  ylim_buffer <- max(clean_data[[variable]], na.rm = TRUE) * 0.2  # 20% headroom
  ymax <- max(clean_data[[variable]], na.rm = TRUE) + ylim_buffer
  
  # Create the plot
  plot <- ggplot(clean_data, aes(x = group_combined, y = !!sym(variable), color = group_combined)) +
    geom_boxplot(fill = "white", outlier.shape = NA, width = 0.6, size = 0.9) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    scale_color_manual(values = color_palette) +
    labs(x = "", y = variable) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = NA, angle = 45, hjust = 1),         
      axis.ticks.x = element_line(),         
      axis.title.x = element_blank(),        
      text = element_text(size = 14)
    )+
    coord_cartesian(ylim = c(NA, ymax * 1.15)) 
  
  
  # Add significance bars if any
  if (length(significant_comparisons) > 0) {
    plot <- plot +
      geom_signif(
        comparisons = significant_comparisons,
        annotations = sapply(p_values, function(p) sprintf("p = %.2g", p)),
        color = "black",
        textsize = 3.5,
        step_increase = 0.1
      )
  }
  
  # Store plot
  plot_list[[variable]] <- plot
}

# Display all plots
grid.arrange(grobs = plot_list, ncol = 2)


# Store all comparison results in a list
all_stats <- list()

for (variable in new_variables) {
  
  # Pairwise Wilcoxon tests
  pairwise_comparisons <- combn(levels(clean_data$group_combined), 2, simplify = FALSE)
  pairwise_results <- sapply(pairwise_comparisons, function(groups) {
    wilcox.test(
      clean_data[[variable]][clean_data$group_combined == groups[1]],
      clean_data[[variable]][clean_data$group_combined == groups[2]],
      exact = FALSE
    )$p.value
  })
  
  # Create a summary table
  stat_table <- data.frame(
    Variable = rep(variable, length(pairwise_results)),
    Group1 = sapply(pairwise_comparisons, `[`, 1),
    Group2 = sapply(pairwise_comparisons, `[`, 2),
    p_value = pairwise_results
  )
  
  # Add adjusted p-values (Bonferroni)
  stat_table$p_adj_bonferroni <- p.adjust(stat_table$p_value, method = "bonferroni")
  
  # Effect sizes
  effsize <- wilcox_effsize(clean_data, formula = as.formula(paste(variable, "~ group_combined")))
  stat_table <- left_join(stat_table, effsize, by = c("Group1" = "group1", "Group2" = "group2"))
  
  # Store in list
  all_stats[[variable]] <- stat_table
}

summary_stats_all <- do.call(rbind, all_stats)

significant_results_4_quest_groups <- summary_stats_all %>%
  filter(!is.na(p_adj_bonferroni) & p_adj_bonferroni < 0.05)



kruskal_results_4_quest_groups <- data.frame()

for (variable in new_variables) {
  test <- kruskal.test(as.formula(paste(variable, "~ group_combined")), data = clean_data)
  
  kruskal_results_4_quest_groups <- rbind(kruskal_results_4_quest_groups, data.frame(
    Variable = variable,
    Chi_squared = round(test$statistic, 3),
    df = test$parameter,
    p_value = signif(test$p.value, 4)
  ))
}

print(kruskal_results_4_quest_groups)

# Make sure cluster is a factor with correct levels
clean_data$cluster <- factor(clean_data$cluster, levels = names(color_palette))

# Initialize list for plots
plot_list <- list()

# Loop over variables
for (variable in variables) {
  
  # Get all pairwise combinations of groups
  pairwise_comparisons <- combn(levels(clean_data$cluster), 2, simplify = FALSE)
  
  print(paste("Variable:", variable, "—", length(pairwise_comparisons), "pairwise comparisons"))
  
  # Run Wilcoxon tests
  p_values_raw <- sapply(pairwise_comparisons, function(groups) {
    wilcox.test(
      clean_data[[variable]][clean_data$cluster == groups[1]],
      clean_data[[variable]][clean_data$cluster == groups[2]],
      exact = FALSE
    )$p.value
  })
  
  
  # Bonferroni correction
  p_adj_bonf <- p.adjust(p_values_raw, method = "bonferroni")
  
  # Use consistent index
  sig_idx <- which(!is.na(p_adj_bonf) & p_adj_bonf < 0.05)
  significant_comparisons <- pairwise_comparisons[sig_idx]
  p_values <- p_adj_bonf[sig_idx]
  
  ylim_buffer <- max(clean_data[[variable]], na.rm = TRUE) * 0.2  # 20% headroom
  ymax <- max(clean_data[[variable]], na.rm = TRUE) + ylim_buffer
  
  # Create the plot
  plot <- ggplot(clean_data, aes(x = cluster, y = !!sym(variable), color = cluster)) +
    geom_boxplot(fill = "white", outlier.shape = NA, width = 0.6, size = 0.9) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    scale_color_manual(values = color_palette) +
    labs(x = "", y = variable) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = NA, angle = 45, hjust = 1),         
      axis.ticks.x = element_line(),         
      axis.title.x = element_blank(),        
      text = element_text(size = 14)
    )+
    coord_cartesian(ylim = c(NA, ymax * 1.15)) 
  
  
  # Add significance bars if any
  if (length(significant_comparisons) > 0) {
    plot <- plot +
      geom_signif(
        comparisons = significant_comparisons,
        annotations = sapply(p_values, function(p) sprintf("p = %.2g", p)),
        color = "black",
        textsize = 3.5,
        step_increase = 0.1
      )
  }
  
  # Store plot
  plot_list[[variable]] <- plot
}

# Display all plots
grid.arrange(grobs = plot_list, ncol = 2)


kruskal_results_cog <- data.frame()

for (variable in variables) {
  test <- kruskal.test(as.formula(paste(variable, "~ cluster")), data = clean_data)
  
  kruskal_results_cog <- rbind(kruskal_results_cog, data.frame(
    Variable = variable,
    Chi_squared = round(test$statistic, 3),
    df = test$parameter,
    p_value = signif(test$p.value, 4)
  ))
}

write.csv(kruskal_results_cog, "kruskal_results_4_cog.csv", row.names = FALSE)


# Store all comparison results in a list
all_stats <- list()

for (variable in variables) {
  
  # Pairwise Wilcoxon tests
  pairwise_comparisons <- combn(levels(clean_data$cluster), 2, simplify = FALSE)
  pairwise_results <- sapply(pairwise_comparisons, function(groups) {
    wilcox.test(
      clean_data[[variable]][clean_data$cluster == groups[1]],
      clean_data[[variable]][clean_data$cluster == groups[2]],
      exact = FALSE
    )$p.value
  })
  
  # Create a summary table
  stat_table <- data.frame(
    Variable = rep(variable, length(pairwise_results)),
    Group1 = sapply(pairwise_comparisons, `[`, 1),
    Group2 = sapply(pairwise_comparisons, `[`, 2),
    p_value = pairwise_results
  )
  
  # Add adjusted p-values (Bonferroni)
  stat_table$p_adj_bonferroni <- p.adjust(stat_table$p_value, method = "bonferroni")
  
  # Effect sizes
  effsize <- wilcox_effsize(clean_data, formula = as.formula(paste(variable, "~ cluster")))
  stat_table <- left_join(stat_table, effsize, by = c("Group1" = "group1", "Group2" = "group2"))
  
  # Store in list
  all_stats[[variable]] <- stat_table
}

summary_stats_all <- do.call(rbind, all_stats)

significant_results_cog <- summary_stats_all %>%
  filter(!is.na(p_adj_bonferroni) & p_adj_bonferroni < 0.05)

write.csv(significant_results_cog, "significant_results_4_cog.csv", row.names = FALSE)





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
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
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
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_models_withPCS[[variable]] <- anova_withPCS
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
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
# Cognitive variables - Kruskal Wallis

# Vector of cognitive variables
variables <- c("pvt_reaction_time", "nback_miss_1", "nback_miss_2",
               "tmt_a_time", "tmt_b_time", "tmt_diff")

# Initialize lists to store Kruskal-Wallis results and models
kruskal_results <- list()
kruskal_models <- list()
kruskal_results_withPCS <- list()
kruskal_models_withPCS <- list()
kruskal_results_withoutPCS <- list()
kruskal_models_withoutPCS <- list()

# Initialize lists for effect sizes
effect_sizes <- list()
effect_sizes_withPCS <- list()
effect_sizes_withoutPCS <- list()

# Initialize lists for Dunn test results
dunn_results <- list()
dunn_results_withPCS <- list()
dunn_results_withoutPCS <- list()

# Function to calculate epsilon squared from Kruskal-Wallis test
# Epsilon squared function
epsilon_squared <- function(kruskal_result) {
  H <- kruskal_result$statistic
  n <- sum(kruskal_result$parameter) + 1
  epsilon <- as.numeric(H) / (n - 1)
  return(round(epsilon, 4))
}

for (variable in variables) {
  # === Overall ===
  kruskal_result <- kruskal.test(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  kruskal_results[[variable]] <- kruskal_result
  kruskal_models[[variable]] <- kruskal_result
  effect_sizes[[variable]] <- epsilon_squared(kruskal_result)
  
  if (kruskal_result$p.value < 0.05) {
    dunn_results[[variable]] <- dunnTest(
      x = clean_data[[variable]],
      g = as.factor(clean_data$cluster),
      method = "bonferroni"
    )
  }
  
  # === WithPCS (self-reported CD) ===
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  kw_withPCS <- kruskal.test(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster))
  kruskal_results_withPCS[[variable]] <- kw_withPCS
  kruskal_models_withPCS[[variable]] <- kw_withPCS
  effect_sizes_withPCS[[variable]] <- epsilon_squared(kw_withPCS)
  
  if (kw_withPCS$p.value < 0.05) {
    dunn_results_withPCS[[variable]] <- dunnTest(
      x = withPCS_data[[variable]],
      g = as.factor(withPCS_data$cluster),
      method = "bonferroni"
    )
  }
  
  # === WithoutPCS (no self-reported CD) ===
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
  kw_withoutPCS <- kruskal.test(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster))
  kruskal_results_withoutPCS[[variable]] <- kw_withoutPCS
  kruskal_models_withoutPCS[[variable]] <- kw_withoutPCS
  effect_sizes_withoutPCS[[variable]] <- epsilon_squared(kw_withoutPCS)
  
  if (kw_withoutPCS$p.value < 0.05) {
    dunn_results_withoutPCS[[variable]] <- dunnTest(
      x = withoutPCS_data[[variable]],
      g = as.factor(withoutPCS_data$cluster),
      method = "bonferroni"
    )
  }
}


# Overall Kruskal
kruskal_results
effect_sizes

# By group
kruskal_results_withPCS
kruskal_results_withoutPCS
effect_sizes_withPCS
effect_sizes_withoutPCS

# Dunn post-hoc
dunn_results
dunn_results_withPCS
dunn_results_withoutPCS


##
# Prepare cluster comparison labels
cluster_pairs <- c("1 - 2", "1 - 3", "1 - 4", "2 - 3", "2 - 4", "3 - 4")
signif_matrix <- data.frame(matrix(nrow = length(variables), ncol = length(cluster_pairs)))
rownames(signif_matrix) <- variables
colnames(signif_matrix) <- cluster_pairs

# Fill in ✅ / ❌ for each pair based on Dunn p.adj
for (var in variables) {
  res <- dunn_results[[var]]$res
  signif_matrix[var, res$Comparison] <- ifelse(res$P.adj < 0.05, "✅", "❌")
}

# View the matrix
print(signif_matrix)

write.csv(signif_matrix, "dunn_significance_matrix.csv", row.names = TRUE)

# Create summary table
summary_table <- data.frame(
  Variable = variables,
  KW_Chi2 = sapply(variables, function(v) round(kruskal_results[[v]]$statistic, 2)),
  df = sapply(variables, function(v) kruskal_results[[v]]$parameter),
  p_value = sapply(variables, function(v) format.pval(kruskal_results[[v]]$p.value, digits = 3, eps = .001)),
  Epsilon2 = sapply(variables, function(v) round(effect_sizes[[v]], 2)),
  Significant_Comparisons = sapply(variables, function(v) {
    res <- dunn_results[[v]]$res
    sigs <- res$Comparison[res$P.adj < 0.05]
    if (length(sigs) == 0) return("-")
    paste(sigs, collapse = ", ")
  })
)

# View the table
print(summary_table)


# Convert to long format
long_sig_df <- data.frame()

for (v in variables) {
  dunn_df <- dunn_results[[v]]$res
  dunn_df$Variable <- v
  dunn_df$Significant <- dunn_df$P.adj < 0.05
  long_sig_df <- rbind(long_sig_df, dunn_df)
}

write.csv(summary_table, "kruskal_dunn_summary.csv", row.names = FALSE)


##






#--------
kruskal_results <- list()

for (variable in variables) {
  result <- kruskal.test(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  kruskal_results[[variable]] <- result
}

# View results
kruskal_results

kruskal_results_withPCS <- list()
kruskal_results_withoutPCS <- list()

for (variable in variables) {
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
  
  kruskal_results_withPCS[[variable]] <- kruskal.test(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster))
  kruskal_results_withoutPCS[[variable]] <- kruskal.test(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster))
}

dunn_results <- list()

for (variable in variables) {
  dunn_results[[variable]] <- dunnTest(
    formula = as.formula(paste(variable, "~ as.factor(cluster)")),
    data = clean_data,
    method = "bonferroni"
  )
}

epsilon_squared <- function(kruskal_result) {
  H <- kruskal_result$statistic
  n <- sum(kruskal_result$parameter) + 1
  epsilon <- as.numeric(H) / (n - 1)
  return(round(epsilon, 4))
}

# Apply for all
effect_sizes_kruskal <- map(kruskal_results, epsilon_squared)

#####
variables <- c("pvt_reaction_time", "nback_miss_1", "nback_miss_2",
               "tmt_a_time", "tmt_b_time", "tmt_diff")
kruskal_results <- list()

for (variable in variables) {
  result <- kruskal.test(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  kruskal_results[[variable]] <- result
}
kruskal_results_withPCS <- list()
kruskal_results_withoutPCS <- list()

for (variable in variables) {
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
  
  kruskal_results_withPCS[[variable]] <- kruskal.test(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster))
  kruskal_results_withoutPCS[[variable]] <- kruskal.test(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster))
}
dunn_results <- list()

for (variable in variables) {
  if (kruskal_results[[variable]]$p.value < 0.05) {
    dunn_results[[variable]] <- dunnTest(
      formula = as.formula(paste(variable, "~ as.factor(cluster)")),
      data = clean_data,
      method = "bonferroni"
    )
  }
}
epsilon_squared <- function(kruskal_result) {
  H <- kruskal_result$statistic
  n <- sum(kruskal_result$parameter) + 1
  epsilon <- as.numeric(H) / (n - 1)
  return(round(epsilon, 4))
}

effect_sizes_kruskal <- map(kruskal_results, epsilon_squared)
plot_list <- list()

for (variable in variables) {
  plot <- ggplot(clean_data, aes(x = as.factor(cluster), y = !!sym(variable), fill = group)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    labs(x = "Cluster", y = variable, title = paste("Distribution of", variable, "by Cluster and Group"))
  
  plot_list[[variable]] <- plot
}

grid.arrange(grobs = plot_list, ncol = 2)
for (variable in variables) {
  cat("===== ", variable, " =====\n")
  
  cat("Kruskal-Wallis p-value (overall):", round(kruskal_results[[variable]]$p.value, 4), "\n")
  cat("Epsilon squared effect size:", effect_sizes_kruskal[[variable]], "\n")
  
  if (!is.null(dunn_results[[variable]])) {
    cat("Post-hoc Dunn test:\n")
    print(dunn_results[[variable]]$res)
  } else {
    cat("No significant differences found → Dunn test skipped.\n")
  }
  
  cat("\n---\n")
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
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
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
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_models_withPCS[[variable]] <- anova_withPCS
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
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
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
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

# Your 4-group color palette
color_palette <- c(
  "1" = "#F5418C",
  "2" = "#F59541",
  "3" = "#B589D6",
  "4" = "#4CAF50"
)


# Make sure cluster is a factor with correct levels
clean_data$cluster <- factor(clean_data$cluster, levels = names(color_palette))

# Initialize list for plots
plot_list <- list()

# Loop over variables
for (variable in new_variables) {
  
  # Get all pairwise combinations of groups
  pairwise_comparisons <- combn(levels(clean_data$cluster), 2, simplify = FALSE)
  
  print(paste("Variable:", variable, "—", length(pairwise_comparisons), "pairwise comparisons"))
  
  # Run Wilcoxon tests
  p_values_raw <- sapply(pairwise_comparisons, function(groups) {
    wilcox.test(
      clean_data[[variable]][clean_data$cluster == groups[1]],
      clean_data[[variable]][clean_data$cluster == groups[2]],
      exact = FALSE
    )$p.value
  })
  
  # Bonferroni correction
  p_adj_bonf <- p.adjust(p_values_raw, method = "bonferroni")
  
  # Use consistent index
  sig_idx <- which(!is.na(p_adj_bonf) & p_adj_bonf < 0.05)
  significant_comparisons <- pairwise_comparisons[sig_idx]
  p_values <- p_adj_bonf[sig_idx]
  
  ylim_buffer <- max(clean_data[[variable]], na.rm = TRUE) * 0.2  # 20% headroom
  ymax <- max(clean_data[[variable]], na.rm = TRUE) + ylim_buffer
  
  # Create the plot
  plot <- ggplot(clean_data, aes(x = cluster, y = !!sym(variable), color = cluster)) +
    geom_boxplot(fill = "white", outlier.shape = NA, width = 0.6, size = 0.9) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    scale_color_manual(values = color_palette) +
    labs(x = "", y = variable) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = NA, angle = 45, hjust = 1),         
      axis.ticks.x = element_line(),         
      axis.title.x = element_blank(),        
      text = element_text(size = 14)
    )+
    coord_cartesian(ylim = c(NA, ymax * 1.15)) 
  
  
  # Add significance bars if any
  if (length(significant_comparisons) > 0) {
    plot <- plot +
      geom_signif(
        comparisons = significant_comparisons,
        annotations = sapply(p_values, function(p) sprintf("p = %.2g", p)),
        color = "black",
        textsize = 3.5,
        step_increase = 0.1
      )
  }
  
  # Store plot
  plot_list[[variable]] <- plot
}

# Display all plots
grid.arrange(grobs = plot_list, ncol = 2)


kruskal_results_quest <- data.frame()

for (variable in new_variables) {
  test <- kruskal.test(as.formula(paste(variable, "~ cluster")), data = clean_data)
  
  kruskal_results_quest <- rbind(kruskal_results_quest, data.frame(
    Variable = variable,
    Chi_squared = round(test$statistic, 3),
    df = test$parameter,
    p_value = signif(test$p.value, 4)
  ))
}

write.csv(kruskal_results_quest, "kruskal_results_4_quest.csv", row.names = FALSE)

# Store all comparison results in a list
all_stats <- list()

for (variable in new_variables) {
  
  # Pairwise Wilcoxon tests
  pairwise_comparisons <- combn(levels(clean_data$cluster), 2, simplify = FALSE)
  pairwise_results <- sapply(pairwise_comparisons, function(groups) {
    wilcox.test(
      clean_data[[variable]][clean_data$cluster == groups[1]],
      clean_data[[variable]][clean_data$cluster == groups[2]],
      exact = FALSE
    )$p.value
  })
  
  # Create a summary table
  stat_table <- data.frame(
    Variable = rep(variable, length(pairwise_results)),
    Group1 = sapply(pairwise_comparisons, `[`, 1),
    Group2 = sapply(pairwise_comparisons, `[`, 2),
    p_value = pairwise_results
  )
  
  # Add adjusted p-values (Bonferroni)
  stat_table$p_adj_bonferroni <- p.adjust(stat_table$p_value, method = "bonferroni")
  
  # Effect sizes
  effsize <- wilcox_effsize(clean_data, formula = as.formula(paste(variable, "~ cluster")))
  stat_table <- left_join(stat_table, effsize, by = c("Group1" = "group1", "Group2" = "group2"))
  
  # Store in list
  all_stats[[variable]] <- stat_table
}

summary_stats_all <- do.call(rbind, all_stats)

significant_results_quest <- summary_stats_all %>%
  filter(!is.na(p_adj_bonferroni) & p_adj_bonferroni < 0.05)

write.csv(significant_results_quest, "significant_results_4_quest.csv", row.names = FALSE)











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
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
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
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  anova_withPCS <- aov(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster), data = withPCS_data)
  anova_results_withPCS[[variable]] <- summary(anova_withPCS)
  anova_models_withPCS[[variable]] <- anova_withPCS
  
  # Perform ANOVA for the current variable within "withoutPCS" group
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
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

#-------
# Questionnaire variables - Kruskal-Wallis

# Vector of questionnaire variables
new_variables <- c("facit_f_FS", "hads_a_total_score", "hads_d_total_score", "psqi_total_score")

# Initialize result lists
kruskal_results_q <- list()
kruskal_models_q <- list()
kruskal_results_withPCS_q <- list()
kruskal_models_withPCS_q <- list()
kruskal_results_withoutPCS_q <- list()
kruskal_models_withoutPCS_q <- list()

effect_sizes_q <- list()
effect_sizes_withPCS_q <- list()
effect_sizes_withoutPCS_q <- list()

dunn_results_q <- list()
dunn_results_withPCS_q <- list()
dunn_results_withoutPCS_q <- list()

# Epsilon squared function
epsilon_squared <- function(kruskal_result) {
  H <- kruskal_result$statistic
  n <- sum(kruskal_result$parameter) + 1
  epsilon <- as.numeric(H) / (n - 1)
  return(round(epsilon, 4))
}

# Loop through all questionnaire variables
for (variable in new_variables) {
  # === Overall ===
  kruskal_result <- kruskal.test(clean_data[[variable]] ~ as.factor(cluster), data = clean_data)
  kruskal_results_q[[variable]] <- kruskal_result
  kruskal_models_q[[variable]] <- kruskal_result
  effect_sizes_q[[variable]] <- epsilon_squared(kruskal_result)
  
  if (kruskal_result$p.value < 0.05) {
    dunn_results_q[[variable]] <- dunnTest(
      x = clean_data[[variable]],
      g = as.factor(clean_data$cluster),
      method = "bonferroni"
    )
  }
  
  # === WithPCS ===
  withPCS_data <- subset(clean_data, group == "self-reported CD")
  kw_withPCS <- kruskal.test(withPCS_data[[variable]] ~ as.factor(withPCS_data$cluster))
  kruskal_results_withPCS_q[[variable]] <- kw_withPCS
  kruskal_models_withPCS_q[[variable]] <- kw_withPCS
  effect_sizes_withPCS_q[[variable]] <- epsilon_squared(kw_withPCS)
  
  if (kw_withPCS$p.value < 0.05) {
    dunn_results_withPCS_q[[variable]] <- dunnTest(
      x = withPCS_data[[variable]],
      g = as.factor(withPCS_data$cluster),
      method = "bonferroni"
    )
  }
  
  # === WithoutPCS ===
  withoutPCS_data <- subset(clean_data, group == "no self-reported CD")
  kw_withoutPCS <- kruskal.test(withoutPCS_data[[variable]] ~ as.factor(withoutPCS_data$cluster))
  kruskal_results_withoutPCS_q[[variable]] <- kw_withoutPCS
  kruskal_models_withoutPCS_q[[variable]] <- kw_withoutPCS
  effect_sizes_withoutPCS_q[[variable]] <- epsilon_squared(kw_withoutPCS)
  
  if (kw_withoutPCS$p.value < 0.05) {
    dunn_results_withoutPCS_q[[variable]] <- dunnTest(
      x = withoutPCS_data[[variable]],
      g = as.factor(withoutPCS_data$cluster),
      method = "bonferroni"
    )
  }
}

#------------------------------------
# SIGNIFICANCE MATRIX for Dunn tests

cluster_pairs <- c("1 - 2", "1 - 3", "1 - 4", "2 - 3", "2 - 4", "3 - 4")
signif_matrix_q <- data.frame(matrix(nrow = length(new_variables), ncol = length(cluster_pairs)))
rownames(signif_matrix_q) <- new_variables
colnames(signif_matrix_q) <- cluster_pairs

for (var in new_variables) {
  if (!is.null(dunn_results_q[[var]])) {
    res <- dunn_results_q[[var]]$res
    signif_matrix_q[var, res$Comparison] <- ifelse(res$P.adj < 0.05, "✅", "❌")
  } else {
    signif_matrix_q[var, ] <- "❌"
  }
}

# View and export
print(signif_matrix_q)
write.csv(signif_matrix_q, "dunn_significance_matrix_questionnaires.csv", row.names = TRUE)

# SUMMARY TABLE for Kruskal + Dunn
summary_table_q <- data.frame(
  Variable = new_variables,
  KW_Chi2 = sapply(new_variables, function(v) round(kruskal_results_q[[v]]$statistic, 2)),
  df = sapply(new_variables, function(v) kruskal_results_q[[v]]$parameter),
  p_value = sapply(new_variables, function(v) format.pval(kruskal_results_q[[v]]$p.value, digits = 3, eps = .001)),
  Epsilon2 = sapply(new_variables, function(v) round(effect_sizes_q[[v]], 2)),
  Significant_Comparisons = sapply(new_variables, function(v) {
    if (!is.null(dunn_results_q[[v]])) {
      res <- dunn_results_q[[v]]$res
      sigs <- res$Comparison[res$P.adj < 0.05]
      if (length(sigs) == 0) return("-")
      paste(sigs, collapse = ", ")
    } else {
      "-"
    }
  })
)

print(summary_table_q)
write.csv(summary_table_q, "kruskal_dunn_summary_questionnaires.csv", row.names = FALSE)

