## R Script for power analysis
# ---------- content -----------------------
# 1. load packages
# 2. load data
# 3. summarise mean
# 4. demographics
# 5. outlier removal
# 5.1 delta (relative and absolute)
# 5.2 beta (relative)
# 5.3 aperiodic components
# 6. export tables for topoplots
# 7. check requirements (normality, variances, etc.)
# 8. boxplots and stats
# 8.1 aperiodic exponent (whole brain)
# 8.2 aperiodic offset (whole brain)
# 8.3 rel and abs delta frontal
# 8.4 rel beta central
# 8.5 tables of all EEG values
# 9. plot behavioral data and corr tests
# 9.1 just behavioral data
# 9.2 corr tests with behav - EEG data
# 9.2.1 rel delta w TMTA & B-A
# 9.2.2 rel delta w moca
# 9.2.3 rel/abs delta w FACIT
# 9.2.4 rel delta w hads
# 9.2.5 rel beta w TMTA & B-A
# 9.2.6 rel beta w FACIT
# 9.2.7 aperiodic exponent with everything
# 9.2.8 aperiodic offset with everything
# 10. r squared
# 11. permutation tests

#------------ 1. load packages------------------
library(tidyverse)
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
library(dplyr)
library(cowplot)
theme_set(theme_cowplot())

#--------------- 2. load data--------------------
# load csv file that I created in MATLAB (has ID, channel, aperiodic offset, aperiodic exponent, abs and rel delta and beta power)
table_power_5 <- read_csv("data/analysis_power/table_power_final_5.csv") # this is the 5s data set with a 0.1 high pass filtering
number_of_epochs_5 <- read_csv("data/analysis_power/number_of_epochs_5.csv")# and load the number of 'good' epochs
number_of_bad_channels <- read_csv("data/analysis_power/number_of_bad_channels.csv")
table_power_5 <- merge(table_power_5, number_of_epochs_5)# put them together
table_power_5 <- merge(table_power_5, number_of_bad_channels)# put them together


# modify table (f.ex. add tmt b-a)
table_power_5 <- table_power_5%>%
  mutate(facit_f_FS = as.numeric(facit_f_FS),
         tmt_diff = tmt_b_time-tmt_a_time)

# Remove rows with NA values in 'cluster_4'
table_power_5 <- table_power_5 %>%
  filter(!is.na(cluster_4))

# Define the channel names you want to select (for delta)
frontal_channels <- c('22','105','11','40','75','39','49','82','48','19','112','25','94','93','83','92','95','96','21','50','10','59','26')

# Filter rows with the specified channel names
table_power_frontal <- table_power_5%>%
  filter(table_power_5$channel %in% frontal_channels)

# Define the channel names you want to select (for beta)
central_channels <- c('85','65','90','66','1','68','3','67','2','70','74','76','81','34','37','42','86','43','87','44','88','45','89','46','77','5','78','6','7','79','8','80','71','35','72','36','73')

# Filter rows with the specified channel names
table_power_central <- table_power_5%>%
  filter(table_power_5$channel %in% central_channels)

#-------3. summarise mean -----------------
df_corr_frontal <- table_power_frontal%>%
  group_by(participant_id,age, sex, years_of_education, cluster_2, cluster_4, facit_f_FS, hads_a_total_score, hads_d_total_score, psqi_total_score, moca, pvt_reaction_time, nback_miss_1, nback_false_alarm_1 ,nback_miss_2 ,nback_false_alarm_2 ,tmt_a_time,tmt_b_time, tmt_diff, number_epochs) %>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_beta_power = mean(rel_beta),
            mean_theta_power = mean(rel_theta),
            mean_alpha_power = mean(rel_alpha),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

df_corr_central <- table_power_central%>%
  group_by(participant_id,age, sex, years_of_education, cluster_2, cluster_4, facit_f_FS, hads_a_total_score, hads_d_total_score, psqi_total_score, moca, pvt_reaction_time, nback_miss_1, nback_false_alarm_1 ,nback_miss_2 ,nback_false_alarm_2 ,tmt_a_time,tmt_b_time, tmt_diff, number_epochs) %>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_beta_power = mean(rel_beta),
            mean_theta_power = mean(rel_theta),
            mean_alpha_power = mean(rel_alpha),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

# is the variance different between the groups?
leveneTest(mean_delta_power~cluster_4,data = df_corr_frontal)# not significant, homogeneity of variance
leveneTest(mean_beta_power~cluster_4,data = df_corr_central)# not significant, homogeneity of variance
#------- 4. demographics-----------------
shapiro_df_c1 <- df_corr_frontal %>% filter(cluster_4 == 'c1')
shapiro_df_c2 <- df_corr_frontal %>% filter(cluster_4 == 'c2')
shapiro_df_c3 <- df_corr_frontal %>% filter(cluster_4 == 'c3')
shapiro_df_c4 <- df_corr_frontal %>% filter(cluster_4 == 'c4')

# sex distribution
df_corr_frontal %>%
  group_by(cluster_4, sex) %>%
  count()

# age
df_corr_frontal %>%
  ggplot(aes(age)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = 'free') +
  theme_classic()

df_corr_frontal %>%
  group_by(cluster_4) %>%
  summarise(mean_age = mean(age),
            sd_age = sd(age))

kruskal.test(age ~ cluster_4, data = df_corr_frontal)
df_corr_frontal %>%
  ungroup() %>%
  wilcox_effsize(age ~ cluster_4)  # interpret with caution – better with pairwise

# years of education
df_corr_frontal %>%
  ggplot(aes(years_of_education)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = 'free') +
  theme_classic()

kruskal.test(years_of_education ~ cluster_4, data = df_corr_frontal)
df_corr_frontal %>%
  ungroup() %>%
  wilcox_effsize(years_of_education ~ cluster_4)

# FACIT
df_corr_frontal %>%
  ggplot(aes(facit_f_FS)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = 'free') +
  theme_classic()

leveneTest(facit_f_FS ~ cluster_4, data = df_corr_frontal)
kruskal.test(facit_f_FS ~ cluster_4, data = df_corr_frontal)
df_corr_frontal %>%
  ungroup() %>%
  wilcox_effsize(facit_f_FS ~ cluster_4)

# pairwise comparisons
df_corr_frontal %>%
  pairwise_wilcox_test(facit_f_FS ~ cluster_4, p.adjust.method = "bonferroni")

# HADS
df_corr_frontal %>%
  ggplot(aes(hads_d_total_score)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = 'free') +
  theme_classic()

leveneTest(hads_d_total_score ~ cluster_4, data = df_corr_frontal)
kruskal.test(hads_d_total_score ~ cluster_4, data = df_corr_frontal)
df_corr_frontal %>%
  ungroup() %>%
  wilcox_effsize(hads_d_total_score ~ cluster_4)

df_corr_frontal %>%
  pairwise_wilcox_test(hads_d_total_score ~ cluster_4, p.adjust.method = "bonferroni")

# TMT A
df_corr_frontal %>%
  ggplot(aes(tmt_a_time)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = 'free') +
  theme_classic()

leveneTest(tmt_a_time ~ cluster_4, data = df_corr_frontal)
kruskal.test(tmt_a_time ~ cluster_4, data = df_corr_frontal)
df_corr_frontal %>%
  ungroup() %>%
  wilcox_effsize(tmt_a_time ~ cluster_4)

df_corr_frontal %>%
  pairwise_wilcox_test(tmt_a_time ~ cluster_4, p.adjust.method = "bonferroni")

# TMT B - A
df_corr_frontal %>%
  ggplot(aes(tmt_diff)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = 'free') +
  theme_classic()

leveneTest(tmt_diff ~ cluster_4, data = df_corr_frontal)
kruskal.test(tmt_diff ~ cluster_4, data = df_corr_frontal)
df_corr_frontal %>%
  ungroup() %>%
  wilcox_effsize(tmt_diff ~ cluster_4)

df_corr_frontal %>%
  pairwise_wilcox_test(tmt_diff ~ cluster_4, p.adjust.method = "bonferroni")

# MOCA
df_corr_frontal %>%
  ggplot(aes(moca)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = 'free') +
  theme_classic()

leveneTest(moca ~ cluster_4, data = df_corr_frontal)
kruskal.test(moca ~ cluster_4, data = df_corr_frontal)
df_corr_frontal %>%
  ungroup() %>%
  wilcox_effsize(moca ~ cluster_4)

df_corr_frontal %>%
  pairwise_wilcox_test(moca ~ cluster_4, p.adjust.method = "bonferroni")

# number of epochs
df_corr_frontal %>%
  group_by(cluster_4) %>%
  summarise(mean_epoch = mean(number_epochs),
            sd_epoch = sd(number_epochs),
            min_epoch = min(number_epochs),
            max_epoch = max(number_epochs))

kruskal.test(number_epochs ~ cluster_4, data = df_corr_frontal)
df_corr_frontal %>%
  ungroup() %>%
  cohens_d(number_epochs ~ cluster_4)

# correlation: epochs and power
df_corr_frontal %>%
  ggplot(aes(x = number_epochs, y = mean_delta_power, color = cluster_4)) +
  geom_point()

cor.test(df_corr_frontal$number_epochs, df_corr_frontal$mean_delta_power)

df_corr_central %>%
  ggplot(aes(x = number_epochs, y = mean_beta_power, color = cluster_4)) +
  geom_point()

cor.test(df_corr_central$number_epochs, df_corr_central$mean_beta_power)

# number of epochs vs fatigue score
table_power_5 %>%
  ggplot(aes(x = number_epochs, y = facit_f_FS)) +
  geom_point()

cor.test(table_power_5$number_epochs, table_power_5$facit_f_FS)

# summarized behavioral values
table_behav <- df_corr_frontal %>%
  group_by(cluster_4) %>%
  summarise(mean_facit = mean(facit_f_FS, na.rm = TRUE),
            sd_facit = sd(facit_f_FS, na.rm = TRUE),
            mean_hads = mean(hads_d_total_score, na.rm = TRUE),
            sd_hads = sd(hads_d_total_score, na.rm = TRUE),
            mean_tmta = mean(tmt_a_time, na.rm = TRUE),
            sd_tmta = sd(tmt_a_time, na.rm = TRUE),
            mean_tmtb_a = mean(tmt_diff, na.rm = TRUE),
            sd_tmtb_a = sd(tmt_diff, na.rm = TRUE),
            mean_y_o = mean(years_of_education, na.rm = TRUE),
            sd_y_o = sd(years_of_education, na.rm = TRUE),
            mean_epoc = mean(number_epochs, na.rm = TRUE),
            sd_epoc = sd(number_epochs, na.rm = TRUE),
            mean_moca = mean(moca, na.rm = TRUE),
            sd_moca = sd(moca, na.rm = TRUE))

# artifact-related channels
channel_artefacts <- table_power_5 %>%
  group_by(cluster_4) %>%
  summarise(mean_channels_ica = mean(num_chan_ica, na.rm = TRUE),
            sd_channels_ica = sd(num_chan_ica, na.rm = TRUE),
            max_channels_ica = max(num_chan_ica, na.rm = TRUE),
            min_channels_ica = min(num_chan_ica, na.rm = TRUE),
            mean_channels_arte = mean(num_chan_artefact, na.rm = TRUE),
            sd_channels_arte = sd(num_chan_artefact, na.rm = TRUE),
            max_channels_arte = max(num_chan_artefact, na.rm = TRUE),
            min_channels_arte = min(num_chan_artefact, na.rm = TRUE))

#------ 5. exclude outliers--------
##-------- 5.1 delta (relative) ---------------

# Quick check for outliers
df_corr_frontal %>%
  group_by(cluster_4) %>%
  ggplot(aes(x = cluster_4, y = mean_delta_power, color = cluster_4)) +
  geom_boxplot()

# Participant-wise filtering (within-person 3SD)
table_delta_filtered <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_rel_delta = mean(rel_delta),
         sd_rel_delta = sd(rel_delta),
         lower_bound = mean_rel_delta - 3 * sd_rel_delta,
         upper_bound = mean_rel_delta + 3 * sd_rel_delta) %>%
  filter(rel_delta >= lower_bound & rel_delta <= upper_bound) %>%
  ungroup()

# Set negative values to 0
table_delta_filtered$rel_delta <- ifelse(table_delta_filtered$rel_delta < 0, 0, table_delta_filtered$rel_delta)

# Visualize cleaned data
table_delta_filtered %>%
  group_by(cluster_4) %>%
  ggplot(aes(x = cluster_4, y = rel_delta, color = cluster_4)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter()

# Keep only frontal channels
table_delta_frontal_filtered <- table_delta_filtered %>%
  filter(channel %in% frontal_channels)

# Average per participant
df_corr_frontal_filtered <- table_delta_frontal_filtered %>%
  group_by(participant_id, cluster_4, tmt_a_time, facit_f_FS, tmt_diff, age, years_of_education, moca) %>%
  summarise(mean_delta_power = mean(rel_delta), .groups = "drop")

# Visualize
df_corr_frontal_filtered %>%
  ggplot(aes(x = cluster_4, y = mean_delta_power, color = cluster_4)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter()

# Additional filtering across clusters (group-based 3SD)
table_delta_filtered_group <- table_delta_filtered %>%
  group_by(cluster_4) %>%
  mutate(mean_rel_delta = mean(rel_delta),
         sd_rel_delta = sd(rel_delta),
         lower_bound = mean_rel_delta - 3 * sd_rel_delta,
         upper_bound = mean_rel_delta + 3 * sd_rel_delta) %>%
  filter(rel_delta >= lower_bound & rel_delta <= upper_bound) %>%
  ungroup()

table_frontal_filtered_group <- table_delta_filtered_group %>%
  filter(channel %in% frontal_channels)

df_corr_frontal_filtered_group <- table_frontal_filtered_group %>%
  group_by(participant_id, cluster_4, tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(
    mean_delta_power = mean(rel_delta),
    mean_aperiodic_exponent = mean(aperiodic_exponent),
    .groups = "drop"
  )

# Visualize
df_corr_frontal_filtered_group %>%
  ggplot(aes(x = cluster_4, y = mean_delta_power, color = cluster_4)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter()

# Statistical test
kruskal.test(mean_delta_power ~ cluster_4, data = df_corr_frontal_filtered_group)

##-------- 5.1 delta (absolute) ---------------

table_delta_filtered_abs <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_abs_delta = mean(abs_delta),
         sd_abs_delta = sd(abs_delta),
         lower_bound = mean_abs_delta - 3 * sd_abs_delta,
         upper_bound = mean_abs_delta + 3 * sd_abs_delta) %>%
  filter(abs_delta >= lower_bound & abs_delta <= upper_bound) %>%
  ungroup()

table_delta_frontal_filtered_abs <- table_delta_filtered_abs %>%
  filter(channel %in% frontal_channels)

df_corr_frontal_filtered_abs <- table_delta_frontal_filtered_abs %>%
  group_by(participant_id, cluster_4, tmt_a_time, facit_f_FS, tmt_diff, age, years_of_education) %>%
  summarise(mean_delta_power_abs = mean(abs_delta), .groups = "drop")

df_corr_frontal_filtered_abs %>%
  ggplot(aes(x = cluster_4, y = mean_delta_power_abs, color = cluster_4)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter()

kruskal.test(mean_delta_power_abs ~ cluster_4, data = df_corr_frontal_filtered_abs)

## ----------- 5.2 beta (relative) central ----------------------------

# Check original distribution
df_corr_central %>%
  ggplot(aes(x = cluster_4, y = mean_beta_power, color = cluster_4)) +
  geom_boxplot()

# Participant-wise 3SD filter
table_beta_filtered <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_rel_beta = mean(rel_beta),
         sd_rel_beta = sd(rel_beta),
         lower_bound = mean_rel_beta - 3 * sd_rel_beta,
         upper_bound = mean_rel_beta + 3 * sd_rel_beta) %>%
  filter(rel_beta >= lower_bound & rel_beta <= upper_bound) %>%
  ungroup()

table_central_filtered <- table_beta_filtered %>%
  filter(channel %in% central_channels)

df_corr_central_filtered <- table_central_filtered %>%
  group_by(participant_id, cluster_4, tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_beta_power = mean(rel_beta), .groups = "drop")

df_corr_central_filtered %>%
  ggplot(aes(x = cluster_4, y = mean_beta_power, color = cluster_4)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter()

# Additional group-level filtering
table_beta_filtered_group <- table_beta_filtered %>%
  group_by(cluster_4) %>%
  mutate(mean_rel_beta = mean(rel_beta),
         sd_rel_beta = sd(rel_beta),
         lower_bound = mean_rel_beta - 3 * sd_rel_beta,
         upper_bound = mean_rel_beta + 3 * sd_rel_beta) %>%
  filter(rel_beta >= lower_bound & rel_beta <= upper_bound) %>%
  ungroup()

table_central_filtered_group <- table_beta_filtered_group %>%
  filter(channel %in% central_channels)

df_corr_central_filtered_group <- table_central_filtered_group %>%
  group_by(participant_id, cluster_4, tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_beta_power = mean(rel_beta), .groups = "drop")

df_corr_central_filtered_group %>%
  ggplot(aes(x = cluster_4, y = mean_beta_power, color = cluster_4)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter()

# Statistical test
kruskal.test(mean_beta_power ~ cluster_4, data = df_corr_central_filtered_group)


## ----------- 5.3 aperiodic components -------------------------------------------

# ----- Aperiodic Exponent: participant-wise 3SD filtering -----

table_ape_filtered <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_ape = mean(aperiodic_exponent),
         sd_ape = sd(aperiodic_exponent),
         lower_bound = mean_ape - 3 * sd_ape,
         upper_bound = mean_ape + 3 * sd_ape) %>%
  filter(aperiodic_exponent >= lower_bound & aperiodic_exponent <= upper_bound) %>%
  ungroup()

df_corr_ape <- table_ape_filtered %>%
  group_by(participant_id, cluster_4, tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_aperiodic_exponent = mean(aperiodic_exponent), .groups = "drop")

# Plot
df_corr_ape %>%
  ggplot(aes(x = cluster_4, y = mean_aperiodic_exponent, color = cluster_4)) +
  geom_boxplot(outlier.colour = "black") +
  geom_jitter(width = 0.2, alpha = 0.7) +
  theme_classic() +
  labs(y = "Mean Aperiodic Exponent")

# Kruskal-Wallis test
kruskal.test(mean_aperiodic_exponent ~ cluster_4, data = df_corr_ape)


# ----- Aperiodic Offset: participant-wise 3SD filtering -----

table_apo_filtered <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_apo = mean(aperiodic_offset),
         sd_apo = sd(aperiodic_offset),
         lower_bound = mean_apo - 3 * sd_apo,
         upper_bound = mean_apo + 3 * sd_apo) %>%
  filter(aperiodic_offset >= lower_bound & aperiodic_offset <= upper_bound) %>%
  ungroup()

df_corr_apo <- table_apo_filtered %>%
  group_by(participant_id, cluster_4, tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_aperiodic_offset = mean(aperiodic_offset), .groups = "drop")

# Plot
df_corr_apo %>%
  ggplot(aes(x = cluster_4, y = mean_aperiodic_offset, color = cluster_4)) +
  geom_boxplot(outlier.colour = "black") +
  geom_jitter(width = 0.2, alpha = 0.7) +
  theme_classic() +
  labs(y = "Mean Aperiodic Offset")

# Kruskal-Wallis test
kruskal.test(mean_aperiodic_offset ~ cluster_4, data = df_corr_apo)

# Post-hoc pairwise comparisons (notsignificant)
#df_corr_ape %>%
 # pairwise_wilcox_test(mean_aperiodic_exponent ~ cluster_4, p.adjust.method = "bonferroni")

#df_corr_apo %>%
 # pairwise_wilcox_test(mean_aperiodic_offset ~ cluster_4, p.adjust.method = "bonferroni")

#-------- 6. export tables for topoplots ---------------------
# Define cluster labels
clusters <- c("c1", "c2", "c3", "c4")

# Function to export topoplot data
export_topoplot_data <- function(df, cluster_col, value_col, file_prefix) {
  for (cl in clusters) {
    export_df <- df %>%
      filter(.data[[cluster_col]] == cl) %>%
      mutate(channel = as.numeric(channel)) %>%
      group_by(channel) %>%
      summarise(mean_val = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
      arrange(channel) %>%
      mutate(channel = replace(channel, is.na(channel), "Gnd"))
    
    write.table(export_df,
                file = paste0("export_", file_prefix, "_", cl, ".txt"),
                row.names = FALSE,
                col.names = FALSE)
    
    cat("Cluster:", cl, "\n")
    print(export_df %>%
            summarise(min = min(mean_val),
                      max = max(mean_val)))
    cat("\n")
  }
}

# Export beta power
export_topoplot_data(table_beta_filtered_group, "cluster_4", "rel_beta", "beta")

# Export delta power
export_topoplot_data(table_delta_filtered_group, "cluster_4", "rel_delta", "delta")

# Export aperiodic exponent
export_topoplot_data(table_ape_filtered, "cluster_4", "aperiodic_exponent", "ape")

# Export aperiodic offset
export_topoplot_data(table_apo_filtered, "cluster_4", "aperiodic_offset", "apo")

# Export r_squared (use table_power_5 directly)
export_topoplot_data(table_power_5, "cluster_4", "r_squared", "r")


#--------- 7. check requirements-----------------------------

# Shapiro-Wilk normality test for each cluster: DELTA POWER
shapiro_clusters <- split(df_corr_frontal_filtered_group, df_corr_frontal_filtered_group$cluster_4)

cat("Shapiro-Wilk test for mean_delta_power:\n")
lapply(names(shapiro_clusters), function(cl) {
  cat("Cluster", cl, ": ")
  print(shapiro.test(shapiro_clusters[[cl]]$mean_delta_power))
})

# Plot histogram for delta
df_corr_frontal_filtered_group %>%
  ggplot(aes(x = mean_delta_power)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = "free") +
  theme_classic() +
  labs(title = "Mean Delta Power Distribution by Cluster")


# Shapiro-Wilk test for BETA POWER
shapiro_clusters_beta <- split(df_corr_central_filtered_group, df_corr_central_filtered_group$cluster_4)

cat("\nShapiro-Wilk test for mean_beta_power:\n")
lapply(names(shapiro_clusters_beta), function(cl) {
  cat("Cluster", cl, ": ")
  print(shapiro.test(shapiro_clusters_beta[[cl]]$mean_beta_power))
})

df_corr_central_filtered_group %>%
  ggplot(aes(x = mean_beta_power)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = "free") +
  theme_classic() +
  labs(title = "Mean Beta Power Distribution by Cluster")


# Shapiro-Wilk test for APERIODIC OFFSET
shapiro_clusters_apo <- split(df_corr_apo, df_corr_apo$cluster_4)

cat("\nShapiro-Wilk test for mean_aperiodic_offset:\n")
lapply(names(shapiro_clusters_apo), function(cl) {
  cat("Cluster", cl, ": ")
  print(shapiro.test(shapiro_clusters_apo[[cl]]$mean_aperiodic_offset))
})

df_corr_apo %>%
  ggplot(aes(x = mean_aperiodic_offset)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = "free") +
  theme_classic() +
  labs(title = "Mean Aperiodic Offset Distribution by Cluster")


# Shapiro-Wilk test for APERIODIC EXPONENT
shapiro_clusters_ape <- split(df_corr_ape, df_corr_ape$cluster_4)

cat("\nShapiro-Wilk test for mean_aperiodic_exponent:\n")
lapply(names(shapiro_clusters_ape), function(cl) {
  cat("Cluster", cl, ": ")
  print(shapiro.test(shapiro_clusters_ape[[cl]]$mean_aperiodic_exponent))
})

df_corr_ape %>%
  ggplot(aes(x = mean_aperiodic_exponent)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~cluster_4, scales = "free") +
  theme_classic() +
  labs(title = "Mean Aperiodic Exponent Distribution by Cluster")


# ---------------- VARIANCE TESTS ----------------

cat("\nLevene's Test Results:\n")

cat("Delta Power:\n")
print(leveneTest(mean_delta_power ~ cluster_4, data = df_corr_frontal_filtered_group))

cat("\nBeta Power:\n")
print(leveneTest(mean_beta_power ~ cluster_4, data = df_corr_central_filtered_group))

cat("\nAperiodic Offset:\n")
print(leveneTest(mean_aperiodic_offset ~ cluster_4, data = df_corr_apo))

cat("\nAperiodic Exponent:\n")
print(leveneTest(mean_aperiodic_exponent ~ cluster_4, data = df_corr_ape))

# (Optional: If you have beta1 or beta2 measures)
# leveneTest(mean_beta1_power ~ cluster_4, data = df_corr_central1_filtered)
# leveneTest(mean_beta2_power ~ cluster_4, data = df_corr_central2_filtered)


# ---------------- INTERPRETATION NOTE ----------------
# Normality assumption: mostly violated across clusters (p < 0.05)
# Variance assumption: generally okay (except maybe aperiodic offset)
# => Recommended: Use non-parametric tests (e.g., Kruskal-Wallis) for delta, beta, exponent, and offset

# ----- 8. boxplots and stats -------------------
# Define color palette for 4 clusters
color_palette <- c(
  "c1" = '#02CAF5',
  "c2" = "#F59541",
  "c3" = "#B589D6",
  "c4" = "#F5418C"
)

plot_and_stats <- function(df, outcome, ylabel) {
  outcome_sym <- rlang::sym(outcome)
  df$cluster_4 <- as.factor(df$cluster_4)
  
  # Kruskal-Wallis
  cat("\n🔹 Kruskal-Wallis test for", outcome, ":\n")
  print(kruskal.test(reformulate("cluster_4", outcome), data = df))
  
  # Pairwise Wilcoxon
  pairwise_stats <- df %>%
    pairwise_wilcox_test(
      formula = reformulate("cluster_4", outcome),
      p.adjust.method = "bonferroni"
    ) %>%
    add_xy_position(x = "cluster_4", fun = "median")
  
  # 🔺 Set ALL labels to be at top of the plot
  top_y <- max(df[[outcome]], na.rm = TRUE)
  pairwise_stats$y.position <- top_y * 1.15  # Pushes them high above everything
  
  cat("\n🔸 Pairwise Wilcoxon tests:\n")
  print(pairwise_stats)
  
  # Effect size
  cat("\n📏 Kruskal effect size:\n")
  print(df %>% kruskal_effsize(reformulate("cluster_4", outcome)))
  
  # Plot
  p <- ggplot(df, aes(x = cluster_4, y = !!outcome_sym, color = cluster_4)) +
    geom_boxplot(size = 0.75, outlier.colour = 'black', width = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    stat_pvalue_manual(
      pairwise_stats,
      hide.ns = FALSE,
      label = "p.adj.signif",  # or use paste() for raw p-values
      tip.length = 0.01,
      size = 4
    ) +
    expand_limits(y = top_y * 1.3) +  # Extra headroom for long comparisons
    scale_color_manual(values = color_palette) +
    theme_classic(base_size = 14) +
    labs(y = ylabel, x = "Cluster") +
    guides(color = FALSE)
  
  print(p)
}

plot_and_stats(df_corr_ape, "mean_aperiodic_exponent", "Mean Aperiodic Exponent")
plot_and_stats(df_corr_apo, "mean_aperiodic_offset", "Mean Aperiodic Offset")
plot_and_stats(df_corr_frontal_filtered_group, "mean_delta_power", "Mean Delta Power [μV²] (Frontal ROI)")
plot_and_stats(df_corr_frontal_filtered_abs, "mean_delta_power_abs", "Absolute Delta Power [μV²] (Frontal ROI)")
plot_and_stats(df_corr_central_filtered_group, "mean_beta_power", "Mean Beta Power [μV²] (Central ROI)")
plot_and_stats(df_corr_central_filtered_group, "mean_beta_power_abs", "Absolute Beta Power [μV²] (Central ROI)")

# --------- 8.5 Summary tables of EEG values per cluster_4 ---------

# Relative delta power (frontal ROI)
df_corr_frontal_filtered_group %>%
  group_by(cluster_4) %>%
  summarise(
    mean_delta = mean(mean_delta_power, na.rm = TRUE),
    sd_delta = sd(mean_delta_power, na.rm = TRUE)
  )

# Relative beta power (central ROI)
df_corr_central_filtered_group %>%
  group_by(cluster_4) %>%
  summarise(
    mean_beta = mean(mean_beta_power, na.rm = TRUE),
    sd_beta = sd(mean_beta_power, na.rm = TRUE)
  )

# Aperiodic exponent (whole brain)
df_corr_ape %>%
  group_by(cluster_4) %>%
  summarise(
    mean_ape = mean(mean_aperiodic_exponent, na.rm = TRUE),
    sd_ape = sd(mean_aperiodic_exponent, na.rm = TRUE)
  )

# Aperiodic offset (whole brain)
df_corr_apo %>%
  group_by(cluster_4) %>%
  summarise(
    mean_apo = mean(mean_aperiodic_offset, na.rm = TRUE),
    sd_apo = sd(mean_aperiodic_offset, na.rm = TRUE)
  )

#------ 9. plot behavioral data and corr test ------
## ------- 9.1 just behavioral data ---------------
# TMT A
df_corr_frontal_filtered%>%
  ggplot(aes(x = cluster_4, y = tmt_a_time))+
  geom_boxplot()+
  geom_jitter(width = 0.3, height = 0, alpha = 0.1)

# FACIT
df_corr_frontal_filtered_group%>%
  mutate(cluster_4 = fct_recode(cluster_4,
                                "c1" = "c1",
                                "c2" = "c2"))%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = cluster_4, y = facit_f_FS, color = cluster_4))+
  geom_boxplot(size = 0.75,outlier.colour = 'black', width=0.5)+
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2)+                                         # Add p-value to plot
  geom_signif(comparisons = list(c("c1","c2")),map_signif_level = function(p) sprintf("p = %.2g", p),test = "t.test", color = 'black')+
  labs(y = 'FACIT Fatigue Scale [Range: 0-52]')+
  scale_color_manual(values = color_palette) +
  theme_classic()+
  guides(color = FALSE)+
  theme(
    text = element_text(size = 15)  # Adjust the size here
  )

# TMT B-A
df_corr_frontal_filtered%>%
  ggplot(aes(x = cluster_4, y = tmt_diff))+
  geom_boxplot()+
  geom_jitter(width = 0.3, height = 0, alpha = 0.1)

# moca
df_corr_frontal_filtered%>%
  ggplot(aes(x = cluster_4, y = moca))+
  geom_boxplot()+
  geom_jitter(width = 0.3, height = 0, alpha = 0.1)

wilcox.test(moca~cluster_4, data = df_corr_frontal_filtered, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)

# FACIT and HADS-D
p8<- df_corr_frontal_filtered_group%>%
  mutate(cluster_4 = fct_recode(cluster_4,
                                "c1" = "c1",
                                "c2" = "c2"))%>%
  ggplot(aes(x = hads_d_total_score,y = facit_f_FS, color = cluster_4))+
  geom_point(size = 2)+
  labs(y = 'FACIT Fatigue Scale [Range: 0-52]',
       x = 'HADS-D Score [Range: 0-21]')+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) +
  scale_color_manual(values = color_palette) +
  theme_classic() +
  theme(legend.position = c(0.20, 0.15))+
  stat_cor(aes(color = "Correlation: "),method = "spearman", label.x = 12, label.y = 40,hjust=0)
ggMarginal(p8, type = "densigram")

cor.test(df_corr_frontal_filtered_group$facit_f_FS,df_corr_frontal_filtered_group$hads_d_total_score, method = 'spearman', exact = FALSE)

# TMT with FACIT
df_corr_frontal_filtered%>%
  ggplot(aes(x = facit_f_FS, y = tmt_a_time))+
  geom_point()

cor.test(df_corr_frontal_filtered$tmt_a_time,df_corr_frontal_filtered$facit_f_FS, method = 'spearman', exact = FALSE)

df_corr_frontal_filtered%>%
  ggplot(aes(x = facit_f_FS, y = tmt_diff))+
  geom_point()

cor.test(df_corr_frontal_filtered$tmt_diff,df_corr_frontal_filtered$facit_f_FS, method = 'spearman', exact = FALSE)
#moca
cor.test(df_corr_frontal_filtered$moca,df_corr_frontal_filtered$facit_f_FS, method = 'spearman', exact = FALSE)

# HADS with TMT-A/B-A and MoCA
cor.test(df_corr_frontal_filtered_group$tmt_a_time,df_corr_frontal_filtered_group$hads_d_total_score, method = 'spearman', exact = FALSE)
cor.test(df_corr_frontal_filtered_group$tmt_diff,df_corr_frontal_filtered_group$hads_d_total_score, method = 'spearman', exact = FALSE)
cor.test(df_corr_frontal_filtered_group$moca,df_corr_frontal_filtered_group$hads_d_total_score, method = 'spearman', exact = FALSE)

# TMT-A with TMT B-A and MoCA
cor.test(df_corr_frontal_filtered_group$tmt_a_time,df_corr_frontal_filtered_group$tmt_diff, method = 'spearman', exact = FALSE)
cor.test(df_corr_frontal_filtered_group$tmt_a_time,df_corr_frontal_filtered_group$moca, method = 'spearman', exact = FALSE)

# TMT B-A and MoCA
cor.test(df_corr_frontal_filtered_group$tmt_diff,df_corr_frontal_filtered_group$moca, method = 'spearman', exact = FALSE)


# very high correlation
## --------- 9.2 corr tests ---------------------------
### ---- 9.2.1 relative delta power with TMT-A and TMT-B-A------------
df_corr_frontal_filtered_group%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = mean_delta_power,y = tmt_a_time, color = cluster_4))+
  geom_point()

cor.test(df_corr_frontal_filtered_group$mean_delta_power,df_corr_frontal_filtered_group$tmt_a_time, method = 'spearman', exact = FALSE)

df_corr_frontal_filtered_group%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = tmt_diff,y = mean_delta_power,color = cluster_4))+
  geom_point()

cor.test(df_corr_frontal_filtered_group$mean_delta_power,df_corr_frontal_filtered_group$tmt_diff, method = 'spearman', exact = FALSE)


### ------ 9.2.2 rel delta and moca ---------------
df_corr_frontal_filtered_group%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = moca,y = mean_delta_power,color = cluster_4))+
  geom_point()

cor.test(df_corr_frontal_filtered$mean_delta_power,df_corr_frontal_filtered$moca, method = 'spearman', exact = FALSE)

###-------- 9.2.3 relative and absolute delta and FACIT score ------------------------
p1<- df_corr_frontal_filtered_group%>%
  mutate(cluster_4 = fct_recode(cluster_4,
                                "c1" = "c1",
                                "c2" = "c2"))%>%
  ggplot(aes(x = mean_delta_power,y = facit_f_FS, color = cluster_4))+
  geom_point(size = 2.5)+
  labs(y = 'FACIT Fatigue Scale [Range: 0-52]',
       x = 'mean delta power [μV^2]')+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) +
  scale_color_manual(values = color_palette) +
  theme_classic() +
  theme(legend.position = c(0.14, 0.15))+
  stat_cor(aes(color = "Correlation: "),method = "spearman", label.x = 2, label.y = 41,hjust=0)+
  theme(
    text = element_text(size = 15)  # Adjust the size here
  )

ggMarginal(p1, type = "densigram")


# one NA
cor.test(df_corr_frontal_filtered_group$mean_delta_power,df_corr_frontal_filtered_group$facit_f_FS, method = 'spearman', exact = FALSE)

# just curious = > divide into the two groups and test separately
p5<- df_corr_frontal_filtered_group%>%
  filter(cluster_4 == 'c1')%>%
  ggplot(aes(x = mean_delta_power,y = facit_f_FS))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) +
  theme_classic() +
  theme(legend.position = c(0.25, 0.15))

ggMarginal(p5, type = "densigram")


test <- df_corr_frontal_filtered_group%>%
  filter(cluster_4 == 'c1')

cor.test(test$mean_delta_power,test$facit_f_FS) # 

p6 <- df_corr_frontal_filtered_group%>%
  filter(cluster_4 == 'c2')%>%
  ggplot(aes(x = mean_delta_power,y = facit_f_FS))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) +
  theme_classic() +
  theme(legend.position = c(0.25, 0.15))

ggMarginal(p6, type = "densigram")

test2 <- df_corr_frontal_filtered_group%>%
  filter(cluster_4 == 'c2')

cor.test(test2$mean_delta_power,test2$facit_f_FS) 


# absolute delta power
p1<- df_corr_frontal_filtered_abs%>%
  mutate(cluster_4 = fct_recode(cluster_4,
                                "c1" = "c1",
                                "c2" = "c2"))%>%
  ggplot(aes(x = mean_delta_power_abs,y = facit_f_FS, color = cluster_4))+
  geom_point(size = 2.5)+
  labs(y = 'FACIT Fatigue Scale [Range: 0-52]',
       x = 'mean delta power [μV^2]')+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) +
  scale_color_manual(values = color_palette) +
  theme_classic() +
  theme(legend.position = c(0.14, 0.15))+
  stat_cor(aes(color = "Correlation: "),method = "spearman", label.x = 2, label.y = 41,hjust=0)+
  theme(
    text = element_text(size = 15)  # Adjust the size here
  )

ggMarginal(p1, type = "densigram")

###------------ 9.2.4 delta and hads d--------------------
p7<- df_corr_frontal_filtered_group%>%
  mutate(cluster_4 = fct_recode(cluster_4,
                                "c1" = "c1",
                                "c2" = "c2"))%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = mean_delta_power,y = hads_d_total_score, color = cluster_4))+
  geom_point(size = 2.5)+
  labs(y = 'HADS-D Score [Range: 0-21]',
       x = 'mean delta power [μV^2]')+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) +
  scale_color_manual(values = color_palette) +
  theme_classic() +
  theme(legend.position = c(0.15, 0.85))+
  stat_cor(aes(color = "Correlation: "),method = "spearman", label.x = 2, label.y = 17,hjust=0)+
  theme(
    text = element_text(size = 15)  # Adjust the size here
  )

ggMarginal(p7, type = "densigram")
cor.test(df_corr_frontal_filtered_group$mean_delta_power,df_corr_frontal_filtered_group$hads_d_total_score, method = 'spearman', exact = FALSE)


### ------ 9.2.5 relative beta power and with TMT-A and TMT-B-A--------------------
df_corr_central_filtered_group%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = mean_beta_power,y = tmt_a_time, color = cluster_4))+
  geom_point()

cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$tmt_a_time, method = 'spearman', exact = FALSE)


df_corr_central_filtered_group%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = mean_beta_power,y = tmt_diff,color = cluster_4))+
  geom_point()

cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$tmt_diff, method = 'spearman', exact = FALSE)

###---- 9.2.6 relative beta power and with FACIT score --------------------
df_corr_central_filtered_group%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = mean_beta_power,y = facit_f_FS,color = cluster_4))+
  geom_point()

cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$facit_f_FS, method = 'spearman', exact = FALSE)
#hads
cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$hads_d_total_score, method = 'spearman', exact = FALSE)
#moca
cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$moca, method = 'spearman', exact = FALSE)

# beta 1
df_corr_central1_filtered%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = mean_beta1_power,y = facit_f_FS,color = cluster_4))+
  geom_point()

cor.test(df_corr_central1_filtered$mean_beta1_power,df_corr_central1_filtered$facit_f_FS, method = 'spearman', exact = FALSE)

# beta 2
df_corr_central2_filtered%>%
  group_by(cluster_4)%>%
  ggplot(aes(x = mean_beta2_power,y = facit_f_FS,color = cluster_4))+
  geom_point()

cor.test(df_corr_central2_filtered$mean_beta2_power,df_corr_central2_filtered$facit_f_FS, method = 'spearman', exact = FALSE)

# beta and delta
corr_power <- cbind(df_corr_central_filtered_group,df_corr_frontal_filtered_group)
cor.test(corr_power$mean_beta_power,corr_power$mean_delta_power, method = 'spearman', exact = FALSE)

### --------9.2.7 aperiodic exponent with everything --------
corr_ape_apo <- cbind(df_corr_central_filtered_group,df_corr_frontal_filtered_group,df_corr_ape,df_corr_apo)
cor.test(corr_ape_apo$mean_aperiodic_exponent...19,corr_ape_apo$mean_delta_power, method = 'spearman', exact = FALSE)
cor.test(corr_ape_apo$mean_aperiodic_exponent...19,corr_ape_apo$mean_beta_power, method = 'spearman', exact = FALSE)
cor.test(df_corr_ape$mean_aperiodic_exponent,df_corr_ape$facit_f_FS, method = 'spearman', exact = FALSE)
cor.test(df_corr_ape$mean_aperiodic_exponent,df_corr_ape$hads_d_total_score, method = 'spearman', exact = FALSE)
cor.test(df_corr_ape$mean_aperiodic_exponent,df_corr_ape$tmt_a_time, method = 'spearman', exact = FALSE)
cor.test(df_corr_ape$mean_aperiodic_exponent,df_corr_ape$tmt_diff, method = 'spearman', exact = FALSE)
cor.test(df_corr_ape$mean_aperiodic_exponent,df_corr_ape$moca, method = 'spearman', exact = FALSE)
cor.test(corr_ape_apo$mean_aperiodic_exponent...19,corr_ape_apo$mean_aperiodic_offset, method = 'spearman', exact = FALSE)

### ---------9.2.8 aperiodic offset with everything -------------
cor.test(corr_ape_apo$mean_aperiodic_offset,corr_ape_apo$mean_delta_power, method = 'spearman', exact = FALSE)
cor.test(corr_ape_apo$mean_aperiodic_offset,corr_ape_apo$mean_beta_power, method = 'spearman', exact = FALSE)
cor.test(df_corr_apo$mean_aperiodic_offset,df_corr_ape$facit_f_FS, method = 'spearman', exact = FALSE)
cor.test(df_corr_apo$mean_aperiodic_offset,df_corr_ape$hads_d_total_score, method = 'spearman', exact = FALSE)
cor.test(df_corr_apo$mean_aperiodic_offset,df_corr_ape$tmt_a_time, method = 'spearman', exact = FALSE)
cor.test(df_corr_apo$mean_aperiodic_offset,df_corr_ape$tmt_diff, method = 'spearman', exact = FALSE)
cor.test(df_corr_apo$mean_aperiodic_offset,df_corr_ape$moca, method = 'spearman', exact = FALSE)

# ---------- 10. r squared ----------------
table_power_5%>%
  ggplot(aes(x = r_squared, y = aperiodic_exponent))+
  geom_point()
cor.test(table_power_5$aperiodic_exponent, table_power_5$r_squared)

table_power_5%>%
  ggplot(aes(x = r_squared, y = aperiodic_offset))+
  geom_point()
cor.test(table_power_5$aperiodic_offset, table_power_5$r_squared)

# ---------- 11. permutation tests -------------------------
# comparing apo and ape at every channel
table_apo_filtered <- table_apo_filtered%>%
  mutate(cluster_4 = as.factor(cluster_4))

table_ape_filtered <- table_ape_filtered%>%
  mutate(cluster_4 = as.factor(cluster_4))

# Create permutation test function
permutation_function <- function(nsim,df,chan,offset_or_exponent){
  res <- numeric(nsim) ## set aside space for results
  df <- df%>%
    filter(channel == chan)
  for (i in 1:nsim) {
    ## standard approach: scramble response value
    perm <- sample(nrow(df))
    bdat <- transform(df,cluster_4 = cluster_4[perm])
    ## compute & store difference in means; store the value
    tt <- t.test(bdat[[offset_or_exponent]]~cluster_4,data=bdat,var.equal=FALSE)
    res[i] <- tt$statistic
  }
  obs <- t.test(df[[offset_or_exponent]]~cluster_4,data=df,var.equal=FALSE)
  ## append the observed value to the list of results
  res <- c(res,obs$statistic)
  res <<- res
  obs <<- obs$statistic
}

# Create permutation test function for wilcox test
permutation_function_wilcox <- function(nsim,df,chan,offset_or_exponent){
  res <- numeric(nsim) ## set aside space for results
  df <- df%>%
    filter(channel == chan)
  for (i in 1:nsim) {
    ## standard approach: scramble response value
    perm <- sample(nrow(df))
    bdat <- transform(df,cluster_4 = cluster_4[perm])
    ## compute & store difference in means; store the value
    w <- wilcox.test(bdat[[offset_or_exponent]]~cluster_4,data=bdat,var.equal=FALSE)
    res[i] <- w$statistic
  }
  obs <- wilcox.test(df[[offset_or_exponent]]~cluster_4,data=df,var.equal=FALSE)
  ## append the observed value to the list of results
  res <- c(res,obs$statistic)
  res <<- res
  obs <<- obs$statistic
}

# create a function for visualisation
visualize_results <- function(res,obs){
  result_sim <- sum(res > obs)/(length(res)-1)
  Histogramm_pre <- hist(res,
                         ylab = 'Anzahl',
                         xlab = substitute(paste(italic('t'), ' values')),
                         main = substitute(paste(italic('t'), ' values with observed value')))
  Histogramm <- abline(v=obs,col="red")
  return(result_sim)
  return(Histogramm)
}

visualize_results_wilcox <- function(res,obs){
  result_sim <- sum(res > obs)/(length(res)-1)
  Histogramm_pre <- hist(res,
                         ylab = 'Anzahl',
                         xlab = substitute(paste(italic('W'), ' values')),
                         main = substitute(paste(italic('W'), ' values with observed value')))
  Histogramm <- abline(v=obs,col="red")
  return(result_sim)
  return(Histogramm)
}


# now do this for every channel: offset 
set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,1,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.298

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,2,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.241

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,3,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.467

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,4,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.554

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,5,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.342

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,6,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.385

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,7,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.255

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,8,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.396

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,9,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.296

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,10,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.654

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,11,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.342

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,12,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.319

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,13,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.431

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,14,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.401

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,15,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.608

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,16,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.611

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,17,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.394

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,18,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.643

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,19,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.614

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,21,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.79

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,22,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.101

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,23,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.537

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,24,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.765

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,25,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.333

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,26,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.617

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,27,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.373

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,28,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.638

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,29,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.818

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,30,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.579

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,33,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.385

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,34,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.685

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,35,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.337

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,36,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.417

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,37,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.357

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,38,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.505

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,39,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.583

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,40,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.606

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,41,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.219

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,42,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.188

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,43,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.474

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,44,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.389

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,45,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.273

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,46,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.452

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,47,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.211

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,48,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.309

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,49,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.351

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,50,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.784

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,51,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.235

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,52,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.479

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,53,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.54

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,54,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.545

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,55,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.392

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,56,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.505

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,57,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.662

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,58,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.755

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,59,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.472

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,60,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.821

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,61,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.279

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,62,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.436

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,63,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.52

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,64,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.587

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,65,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.456

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,66,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.455

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,67,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.268

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,68,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.337

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,69,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.393

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,70,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.422

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,71,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.415

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,72,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.281

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,73,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.397

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,74,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.335

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,75,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.561

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,76,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.359

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,77,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.447

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,78,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.258

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,79,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.28

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,80,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.28

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,81,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.29

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,82,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.397

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,83,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.66

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,84,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.329

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,85,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.243

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,86,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.238

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,87,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.393

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,88,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.593

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,89,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.519

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,90,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.265

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,91,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.542

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,92,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.349

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,93,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.721

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,94,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.395

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,95,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.559

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,96,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.655

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,97,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.124

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,98,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.179

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,99,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.444

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,100,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.406

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,101,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.533

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,102,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.361

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,103,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.428

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,104,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.433

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,105,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.412

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,106,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.44

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,107,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.748

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,108,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.688

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,109,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.499

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,110,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.481

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,111,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.636

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,112,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.616

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,113,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.317

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,114,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.32

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,115,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.319

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,116,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.284

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,117,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.723

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,118,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.416

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,119,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.686

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,120,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.755

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,121,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.499

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,122,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.675

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,123,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.251

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,124,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.914

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,125,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.233

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,126,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.329

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,127,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.507

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,128,'aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.418

set.seed(2)
permutation_function_wilcox(1000,table_apo_filtered,'Gnd','aperiodic_offset')
visualize_results_wilcox(res,obs)# 0.672


# aperiodic exponent
set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,1,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.062

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,2,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.062

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,3,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.137

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,4,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.507

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,5,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.076

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,6,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.071

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,7,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.057

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,8,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.13

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,9,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.141

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,10,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.13

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,11,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.319

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,12,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.366

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,13,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.313

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,14,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.439

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,15,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.534

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,16,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.357

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,17,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.435

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,18,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.578

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,19,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.308

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,21,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.847

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,22,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.239

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,23,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.399

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,24,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.762

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,25,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.349

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,26,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.634

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,27,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.198

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,28,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.306

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,29,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.46

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,30,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.257

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,33,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.159

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,34,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.149

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,35,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.063

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,36,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.122

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,37,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.106

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,38,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.129

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,39,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.126

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,40,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.436

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,41,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.496

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,42,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.24

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,43,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.082

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,44,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.156

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,45,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.16

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,46,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.481

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,47,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.22

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,48,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.214

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,49,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.113

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,50,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.538

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,51,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.253

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,52,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.34

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,53,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.413

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,54,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.429

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,55,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.265

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,56,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.32

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,57,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.577

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,58,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.501

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,59,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.407

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,60,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.249

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,61,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.337

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,62,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.246

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,63,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.223

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,64,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.26

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,65,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.067

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,66,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.121

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,67,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.118

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,68,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.046

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,69,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.072

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,70,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.102

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,71,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.062

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,72,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.08

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,73,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.1

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,74,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.11

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,75,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.293

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,76,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.225

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,77,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.198

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,78,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.082

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,79,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.053

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,80,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.078

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,81,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.164

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,82,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.162

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,83,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.312

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,84,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.249

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,85,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.487

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,86,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.046

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,87,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.133

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,88,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.252

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,89,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.372

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,90,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.442

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,91,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.102

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,92,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.119

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,93,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.569

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,94,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.49

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,95,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.708

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,96,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.549

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,97,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.114

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,98,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.332

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,99,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.278

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,100,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.277

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,101,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.371

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,102,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.447

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,103,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.561

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,104,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.235

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,105,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.389

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,106,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.469

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,107,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.582

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,108,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.413

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,109,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.212

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,110,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.365

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,111,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.602

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,112,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.459

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,113,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.447

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,114,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.195

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,115,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.206

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,116,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.144

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,117,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.419

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,118,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.255

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,119,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.263

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,120,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.594

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,121,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.131

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,122,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.336

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,123,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.475

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,124,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.249

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,125,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.267

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,126,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.207

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,127,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.379

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,128,'aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.212

set.seed(2)
permutation_function_wilcox(1000,table_ape_filtered,'Gnd','aperiodic_exponent')
visualize_results_wilcox(res,obs)# 0.702

