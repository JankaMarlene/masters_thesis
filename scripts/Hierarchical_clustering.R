# Hierarchical clustering
library(tidyverse)
library(dplyr)
library(dendextend)
load("clean_data.RData")

# Extract relevant columns from clean_data
cog_df <- clean_data[, c("group","pvt_reaction_time","nback_miss_1","nback_miss_2","tmt_a_time","tmt_b_time")]
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
abline(h = 33, col = 'red')
# Visualize tree with different colored branches
ward_dend_obj <- as.dendrogram(hclust_ward)
ward_col_dend <- color_branches(ward_dend_obj, h = 30)
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

suppressPackageStartupMessages(library(ggplot2))
ggplot(cog_df_cl, aes(x= nback_miss_1, y = nback_miss_2, color = factor(cluster))) + geom_point()
