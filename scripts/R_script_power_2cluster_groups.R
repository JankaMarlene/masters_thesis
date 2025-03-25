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
# Load data
table_power_5 <- read_csv("data/analysis_power/table_power_final_5.csv")
number_of_epochs_5 <- read_csv("data/analysis_power/number_of_epochs_5.csv")
number_of_bad_channels <- read_csv("data/analysis_power/number_of_bad_channels.csv")
table_power_5 <- merge(table_power_5, number_of_epochs_5)
table_power_5 <- merge(table_power_5, number_of_bad_channels)

# Modify table (e.g. add TMT B-A and define combined group)
# Add calculated variables and rename groups
table_power_5 <- table_power_5 %>%
  mutate(
    facit_f_FS = as.numeric(facit_f_FS),
    tmt_diff = tmt_b_time - tmt_a_time,
    PCS_group = ifelse(group == "withPCS", "self-reported CD", "no self-reported CD"),
    group_combined = paste(PCS_group, cluster_2, sep = "_")
  )

# Filter out incomplete rows
table_power_5 <- table_power_5 %>%
  filter(!is.na(cluster_2), !is.na(group))

# Define the channel names you want to select (for delta)
frontal_channels <- c('22','105','11','40','75','39','49','82','48','19','112','25','94','93','83','92','95','96','21','50','10','59','26')

# Filter rows with the specified channel names for frontal ROI
table_power_frontal <- table_power_5 %>%
  filter(channel %in% frontal_channels)

# Define the channel names you want to select (for beta)
central_channels <- c('85','65','90','66','1','68','3','67','2','70','74','76','81','34','37','42','86','43','87','44','88','45','89','46','77','5','78','6','7','79','8','80','71','35','72','36','73')

# Filter rows with the specified channel names for central ROI
table_power_central <- table_power_5 %>%
  filter(channel %in% central_channels)

#-------3. summarise mean -----------------

# Frontal ROI: summarize delta power per participant
df_corr_frontal <- table_power_frontal %>%
  group_by(participant_id, cluster_2, group, group_combined, age, sex, years_of_education,
           facit_f_FS, hads_a_total_score, hads_d_total_score, psqi_total_score, moca,
           pvt_reaction_time, nback_miss_1, nback_false_alarm_1, nback_miss_2, nback_false_alarm_2,
           tmt_a_time, tmt_b_time, tmt_diff, number_epochs) %>%
  summarise(mean_delta_power = mean(rel_delta, na.rm = TRUE),
            mean_beta_power = mean(rel_beta, na.rm = TRUE),
            mean_theta_power = mean(rel_theta, na.rm = TRUE),
            mean_alpha_power = mean(rel_alpha, na.rm = TRUE),
            mean_aperiodic_exponent = mean(aperiodic_exponent, na.rm = TRUE),
            .groups = "drop")

# Central ROI: summarize beta power per participant
df_corr_central <- table_power_central %>%
  group_by(participant_id, cluster_2, group, group_combined, age, sex, years_of_education,
           facit_f_FS, hads_a_total_score, hads_d_total_score, psqi_total_score, moca,
           pvt_reaction_time, nback_miss_1, nback_false_alarm_1, nback_miss_2, nback_false_alarm_2,
           tmt_a_time, tmt_b_time, tmt_diff, number_epochs) %>%
  summarise(mean_delta_power = mean(rel_delta, na.rm = TRUE),
            mean_beta_power = mean(rel_beta, na.rm = TRUE),
            mean_theta_power = mean(rel_theta, na.rm = TRUE),
            mean_alpha_power = mean(rel_alpha, na.rm = TRUE),
            mean_aperiodic_exponent = mean(aperiodic_exponent, na.rm = TRUE),
            .groups = "drop")

# Levene's Test for delta power across 4 groups
leveneTest(mean_delta_power ~ group_combined, data = df_corr_frontal)
# → tests homogeneity of variance in delta power across with/without PCS in c1 and c2

# Levene's Test for beta power across 4 groups
leveneTest(mean_beta_power ~ group_combined, data = df_corr_central)
# → tests homogeneity of variance in beta power across with/without PCS in c1 and c2

#------- 4. demographics-----------------
# Create group-wise subsets for plotting and stats
shapiro_groups <- df_corr_frontal %>%
  group_by(group_combined)

# Sex distribution per group
sex_distribution <- df_corr_frontal %>%
  group_by(group_combined, sex) %>%
  count()

# Age distribution
age_hist <- df_corr_frontal %>%
  ggplot(aes(age)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~group_combined, scales = 'free') +
  theme_classic()

# Summary of age per group
age_summary <- df_corr_frontal %>%
  group_by(group_combined) %>%
  summarise(mean_age = mean(age), sd_age = sd(age))

# Statistical comparison
kruskal.test(age ~ group_combined, data = df_corr_frontal)
df_corr_frontal %>% ungroup() %>% wilcox_effsize(age ~ group_combined)

# Years of education
education_hist <- df_corr_frontal %>%
  ggplot(aes(years_of_education)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~group_combined, scales = 'free') +
  theme_classic()

kruskal.test(years_of_education ~ group_combined, data = df_corr_frontal)
df_corr_frontal %>% ungroup() %>% wilcox_effsize(years_of_education ~ group_combined)

# FACIT
facit_hist <- df_corr_frontal %>%
  ggplot(aes(facit_f_FS)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~group_combined, scales = 'free') +
  theme_classic()

kruskal.test(facit_f_FS ~ group_combined, data = df_corr_frontal)
df_corr_frontal %>% ungroup() %>% wilcox_effsize(facit_f_FS ~ group_combined)

# HADS
hads_hist <- df_corr_frontal %>%
  ggplot(aes(hads_d_total_score)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~group_combined, scales = 'free') +
  theme_classic()

kruskal.test(hads_d_total_score ~ group_combined, data = df_corr_frontal)
df_corr_frontal %>% ungroup() %>% wilcox_effsize(hads_d_total_score ~ group_combined)

# TMT A
kruskal.test(tmt_a_time ~ group_combined, data = df_corr_frontal)
df_corr_frontal %>% ungroup() %>% wilcox_effsize(tmt_a_time ~ group_combined)

# TMT B - A
kruskal.test(tmt_diff ~ group_combined, data = df_corr_frontal)
df_corr_frontal %>% ungroup() %>% wilcox_effsize(tmt_diff ~ group_combined)

# MOCA
kruskal.test(moca ~ group_combined, data = df_corr_frontal)
df_corr_frontal %>% ungroup() %>% cohens_d(moca ~ group_combined)

# Number of epochs
epoch_summary <- df_corr_frontal %>%
  group_by(group_combined) %>%
  summarise(mean_epoch = mean(number_epochs), sd_epoch = sd(number_epochs))

kruskal.test(number_epochs ~ group_combined, data = df_corr_frontal)
df_corr_frontal %>% ungroup() %>% cohens_d(number_epochs ~ group_combined)

# Correlations
cor.test(df_corr_frontal$number_epochs, df_corr_frontal$mean_delta_power)
cor.test(df_corr_central$number_epochs, df_corr_central$mean_beta_power)
cor.test(table_power_5$number_epochs, table_power_5$facit_f_FS)

# Summarized behavioral table
table_behav <- df_corr_frontal %>%
  group_by(group_combined) %>%
  summarise(mean_facit = mean(facit_f_FS, na.rm = TRUE),
            sd_facit = sd(facit_f_FS, na.rm = TRUE),
            mean_hads = mean(hads_d_total_score, na.rm = TRUE),
            sd_hads = sd(hads_d_total_score, na.rm = TRUE),
            mean_tmta = mean(tmt_a_time),
            sd_tmta = sd(tmt_a_time),
            mean_tmtb_a = mean(tmt_diff),
            sd_tmtb_a = sd(tmt_diff),
            mean_y_o = mean(years_of_education),
            sd_y_o = sd(years_of_education),
            mean_epoc = mean(number_epochs),
            sd_epoc = sd(number_epochs),
            mean_moca = mean(moca, na.rm = TRUE),
            sd_moca = sd(moca, na.rm = TRUE))

# Channel artifact summary
channel_artefacts <- table_power_5 %>%
  group_by(group_combined) %>%
  summarise(mean_channels_ica = mean(num_chan_ica),
            sd_channels_ica = sd(num_chan_ica),
            max_channels_ica = max(num_chan_ica),
            min_channels_ica = min(num_chan_ica),
            mean_channels_arte = mean(num_chan_artefact),
            sd_channels_arte = sd(num_chan_artefact),
            max_channels_arte = max(num_chan_artefact),
            min_channels_arte = min(num_chan_artefact))

#------ 5. exclude outliers--------
##-------- 5.1 delta ---------------
# Check delta distribution per group
df_corr_frontal %>%
  group_by(group_combined) %>%
  ggplot(aes(x = group_combined, y = mean_delta_power, color = group_combined)) +
  geom_boxplot() +
  geom_jitter() +
  theme_classic()

# Outlier removal within participant (±3 SD)
table_delta_filtered <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_rel_delta = mean(rel_delta),
         sd_rel_delta = sd(rel_delta),
         lower_bound = mean_rel_delta - 3 * sd_rel_delta,
         upper_bound = mean_rel_delta + 3 * sd_rel_delta) %>%
  filter(rel_delta >= lower_bound & rel_delta <= upper_bound) %>%
  ungroup()

# Remove negative delta values
table_delta_filtered <- table_delta_filtered %>%
  mutate(rel_delta = ifelse(rel_delta < 0, 0, rel_delta))

# Only keep frontal channels
table_delta_frontal_filtered <- table_delta_filtered %>%
  filter(channel %in% frontal_channels)

# Summary: mean delta power (filtered), group_combined included
df_corr_frontal_filtered <- table_delta_frontal_filtered %>%
  group_by(participant_id, group_combined, cluster_2, age, tmt_a_time, facit_f_FS, tmt_diff, moca) %>%
  summarise(mean_delta_power = mean(rel_delta), .groups = "drop")

# Plot after filtering
df_corr_frontal_filtered %>%
  ggplot(aes(x = group_combined, y = mean_delta_power, color = group_combined)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter(width = 0.3, height = 0, alpha = 0.5) +
  theme_classic()

# Additional filtering across group_combined
table_delta_filtered_group <- table_delta_filtered %>%
  group_by(group_combined) %>%
  mutate(mean_rel_delta = mean(rel_delta),
         sd_rel_delta = sd(rel_delta),
         lower_bound = mean_rel_delta - 3 * sd_rel_delta,
         upper_bound = mean_rel_delta + 3 * sd_rel_delta) %>%
  filter(rel_delta >= lower_bound & rel_delta <= upper_bound) %>%
  ungroup()

# Filter to frontal channels
table_frontal_filtered_group <- table_delta_filtered_group %>%
  filter(channel %in% frontal_channels)

# Summarize again after across-group filtering
df_corr_frontal_filtered_group <- table_frontal_filtered_group %>%
  group_by(participant_id, group_combined, cluster_2, tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_aperiodic_exponent = mean(aperiodic_exponent),
            .groups = "drop")

# Plot updated delta power
df_corr_frontal_filtered_group %>%
  ggplot(aes(x = group_combined, y = mean_delta_power, color = group_combined)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter(width = 0.3, alpha = 0.5) +
  theme_classic()

# ---- Absolute delta power ----
table_delta_filtered_abs <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_abs_delta = mean(abs_delta),
         sd_abs_delta = sd(abs_delta),
         lower_bound = mean_abs_delta - 3 * sd_abs_delta,
         upper_bound = mean_abs_delta + 3 * sd_abs_delta) %>%
  filter(abs_delta >= lower_bound & abs_delta <= upper_bound) %>%
  ungroup()

# Keep frontal channels only
table_delta_frontal_filtered_abs <- table_delta_filtered_abs %>%
  filter(channel %in% frontal_channels)

# Summarize absolute delta
df_corr_frontal_filtered_abs <- table_delta_frontal_filtered_abs %>%
  group_by(participant_id, group_combined, cluster_2, tmt_a_time, facit_f_FS, tmt_diff, age, years_of_education) %>%
  summarise(mean_delta_power_abs = mean(abs_delta), .groups = "drop")

# Plot
df_corr_frontal_filtered_abs %>%
  ggplot(aes(x = group_combined, y = mean_delta_power_abs, color = group_combined)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter(width = 0.3, height = 0, alpha = 0.3) +
  theme_classic()

# Kruskal-Wallis test across 4 groups
kruskal.test(mean_delta_power_abs ~ group_combined, data = df_corr_frontal_filtered_abs)

## ----------- 5.2 relative beta power central ----------------------------

# Quick initial look
df_corr_central %>%
  group_by(group_combined) %>%
  ggplot(aes(x = group_combined, y = mean_beta_power, color = group_combined)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter(alpha = 0.3, width = 0.2) +
  theme_classic()

# Step 1: Outlier removal (within participant)
table_beta_filtered <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_rel_beta = mean(rel_beta, na.rm = TRUE),
         sd_rel_beta = sd(rel_beta, na.rm = TRUE),
         lower_bound = mean_rel_beta - 3 * sd_rel_beta,
         upper_bound = mean_rel_beta + 3 * sd_rel_beta) %>%
  filter(rel_beta >= lower_bound & rel_beta <= upper_bound) %>%
  ungroup()

# Step 2: Keep only central channels
table_central_filtered <- table_beta_filtered %>%
  filter(channel %in% central_channels)

# Step 3: Summarize beta power (within participant)
df_corr_central_filtered <- table_central_filtered %>%
  group_by(participant_id, group, cluster_2, group_combined,
           tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_beta_power = mean(rel_beta, na.rm = TRUE), .groups = "drop")

# Step 4: Visualize cleaned beta power
df_corr_central_filtered %>%
  ggplot(aes(x = group_combined, y = mean_beta_power, color = group_combined)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter(alpha = 0.3, width = 0.2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 5: Additional outlier filtering across groups
table_beta_filtered_group <- table_beta_filtered %>%
  group_by(group_combined) %>%
  mutate(mean_rel_beta = mean(rel_beta, na.rm = TRUE),
         sd_rel_beta = sd(rel_beta, na.rm = TRUE),
         lower_bound = mean_rel_beta - 3 * sd_rel_beta,
         upper_bound = mean_rel_beta + 3 * sd_rel_beta) %>%
  filter(rel_beta >= lower_bound & rel_beta <= upper_bound) %>%
  ungroup()

# Step 6: Filter again for central channels
table_central_filtered_group <- table_beta_filtered_group %>%
  filter(channel %in% central_channels)

# Step 7: Summarize for analysis
df_corr_central_filtered_group <- table_central_filtered_group %>%
  group_by(participant_id, group, cluster_2, group_combined,
           tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_beta_power = mean(rel_beta, na.rm = TRUE), .groups = "drop")

# Step 8: Final plot
df_corr_central_filtered_group %>%
  ggplot(aes(x = group_combined, y = mean_beta_power, color = group_combined)) +
  geom_boxplot(outlier.colour = 'black') +
  geom_jitter(alpha = 0.3, width = 0.2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Relative Beta Power in Central ROI",
       x = "Group",
       y = "Mean Beta Power [μV²]")

## ----------- 5.3 aperiodic components -------------------------------------------

# --- Aperiodic Exponent ---

# Remove outliers within participants (±3 SD)
table_ape_filtered <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_ape = mean(aperiodic_exponent, na.rm = TRUE),
         sd_ape = sd(aperiodic_exponent, na.rm = TRUE),
         lower_bound = mean_ape - 3 * sd_ape,
         upper_bound = mean_ape + 3 * sd_ape) %>%
  filter(aperiodic_exponent >= lower_bound & aperiodic_exponent <= upper_bound) %>%
  ungroup()

# Summarize per participant
df_corr_ape <- table_ape_filtered %>%
  group_by(participant_id, group, cluster_2, group_combined,
           tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_aperiodic_exponent = mean(aperiodic_exponent, na.rm = TRUE), .groups = "drop")

# Plot aperiodic exponent by group
df_corr_ape %>%
  ggplot(aes(x = group_combined, y = mean_aperiodic_exponent, color = group_combined)) +
  geom_boxplot(outlier.colour = "black") +
  geom_jitter(alpha = 0.3, width = 0.2) +
  theme_classic() +
  labs(x = "Group", y = "Mean Aperiodic Exponent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --- Aperiodic Offset ---

# Remove outliers within participants (±3 SD)
table_apo_filtered <- table_power_5 %>%
  group_by(participant_id) %>%
  mutate(mean_apo = mean(aperiodic_offset, na.rm = TRUE),
         sd_apo = sd(aperiodic_offset, na.rm = TRUE),
         lower_bound = mean_apo - 3 * sd_apo,
         upper_bound = mean_apo + 3 * sd_apo) %>%
  filter(aperiodic_offset >= lower_bound & aperiodic_offset <= upper_bound) %>%
  ungroup()

# Summarize per participant
df_corr_apo <- table_apo_filtered %>%
  group_by(participant_id, group, cluster_2, group_combined,
           tmt_a_time, facit_f_FS, tmt_diff, age, moca, hads_d_total_score) %>%
  summarise(mean_aperiodic_offset = mean(aperiodic_offset, na.rm = TRUE), .groups = "drop")

# Plot aperiodic offset by group
df_corr_apo %>%
  ggplot(aes(x = group_combined, y = mean_aperiodic_offset, color = group_combined)) +
  geom_boxplot(outlier.colour = "black") +
  geom_jitter(alpha = 0.3, width = 0.2) +
  theme_classic() +
  labs(x = "Group", y = "Mean Aperiodic Offset") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------- 6. export tables for topoplots ---------------------

# Define helper function for exporting
export_topoplot_table <- function(data, value_col, file_prefix) {
  data %>%
    mutate(channel = as.numeric(channel)) %>%
    group_by(channel) %>%
    summarise(mean_val = mean({{ value_col }}, na.rm = TRUE), .groups = "drop") %>%
    arrange(channel) %>%
    mutate(channel = replace(channel, is.na(channel), "Gnd")) %>%
    write.table(file = paste0(file_prefix, ".txt"), row.names = FALSE, col.names = FALSE)
}

#--- Export rel beta power
export_topoplot_table(filter(table_beta_filtered_group, group_combined == "self-reported CD_c1"), rel_beta, "export_beta_selfCD_c1")
export_topoplot_table(filter(table_beta_filtered_group, group_combined == "self-reported CD_c2"), rel_beta, "export_beta_selfCD_c2")
export_topoplot_table(filter(table_beta_filtered_group, group_combined == "no self-reported CD_c1"), rel_beta, "export_beta_noCD_c1")
export_topoplot_table(filter(table_beta_filtered_group, group_combined == "no self-reported CD_c2"), rel_beta, "export_beta_noCD_c2")

#--- Export rel delta power
export_topoplot_table(filter(table_delta_filtered_group, group_combined == "self-reported CD_c1"), rel_delta, "export_delta_selfCD_c1")
export_topoplot_table(filter(table_delta_filtered_group, group_combined == "self-reported CD_c2"), rel_delta, "export_delta_selfCD_c2")
export_topoplot_table(filter(table_delta_filtered_group, group_combined == "no self-reported CD_c1"), rel_delta, "export_delta_noCD_c1")
export_topoplot_table(filter(table_delta_filtered_group, group_combined == "no self-reported CD_c2"), rel_delta, "export_delta_noCD_c2")

#--- Export aperiodic exponent
export_topoplot_table(filter(table_ape_filtered, group_combined == "self-reported CD_c1"), aperiodic_exponent, "export_ape_selfCD_c1")
export_topoplot_table(filter(table_ape_filtered, group_combined == "self-reported CD_c2"), aperiodic_exponent, "export_ape_selfCD_c2")
export_topoplot_table(filter(table_ape_filtered, group_combined == "no self-reported CD_c1"), aperiodic_exponent, "export_ape_noCD_c1")
export_topoplot_table(filter(table_ape_filtered, group_combined == "no self-reported CD_c2"), aperiodic_exponent, "export_ape_noCD_c2")

#--- Export aperiodic offset
export_topoplot_table(filter(table_apo_filtered, group_combined == "self-reported CD_c1"), aperiodic_offset, "export_apo_selfCD_c1")
export_topoplot_table(filter(table_apo_filtered, group_combined == "self-reported CD_c2"), aperiodic_offset, "export_apo_selfCD_c2")
export_topoplot_table(filter(table_apo_filtered, group_combined == "no self-reported CD_c1"), aperiodic_offset, "export_apo_noCD_c1")
export_topoplot_table(filter(table_apo_filtered, group_combined == "no self-reported CD_c2"), aperiodic_offset, "export_apo_noCD_c2")

#--- Export r-squared
export_topoplot_table(filter(table_power_5, group_combined == "self-reported CD_c1"), r_squared, "export_r_selfCD_c1")
export_topoplot_table(filter(table_power_5, group_combined == "self-reported CD_c2"), r_squared, "export_r_selfCD_c2")
export_topoplot_table(filter(table_power_5, group_combined == "no self-reported CD_c1"), r_squared, "export_r_noCD_c1")
export_topoplot_table(filter(table_power_5, group_combined == "no self-reported CD_c2"), r_squared, "export_r_noCD_c2")

#--------- 7. Check assumptions: normality & variance -----------------------------
# ----- Shapiro-Wilk normality tests (per group_combined)

# Delta power (frontal)
df_corr_frontal_filtered_group %>%
  ggplot(aes(x = mean_delta_power)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~group_combined, scales = 'free') +
  theme_classic()

split(df_corr_frontal_filtered_group, df_corr_frontal_filtered_group$group_combined) %>%
  lapply(function(group) shapiro.test(group$mean_delta_power))

# Beta power (central)
df_corr_central_filtered_group %>%
  ggplot(aes(x = mean_beta_power)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~group_combined, scales = 'free') +
  theme_classic()

split(df_corr_central_filtered_group, df_corr_central_filtered_group$group_combined) %>%
  lapply(function(group) shapiro.test(group$mean_beta_power))

# Aperiodic offset
df_corr_apo %>%
  ggplot(aes(x = mean_aperiodic_offset)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~group_combined, scales = 'free') +
  theme_classic()

split(df_corr_apo, df_corr_apo$group_combined) %>%
  lapply(function(group) shapiro.test(group$mean_aperiodic_offset))

# Aperiodic exponent
df_corr_ape %>%
  ggplot(aes(x = mean_aperiodic_exponent)) +
  geom_histogram(color = "black", fill = "white", bins = sqrt(100)) +
  facet_wrap(~group_combined, scales = 'free') +
  theme_classic()

split(df_corr_ape, df_corr_ape$group_combined) %>%
  lapply(function(group) shapiro.test(group$mean_aperiodic_exponent))


# ----- Levene’s Test for Homogeneity of Variance (across 4 groups)

# Make sure grouping variable is a factor
df_corr_frontal_filtered_group$group_combined <- as.factor(df_corr_frontal_filtered_group$group_combined)
df_corr_central_filtered_group$group_combined <- as.factor(df_corr_central_filtered_group$group_combined)
df_corr_apo$group_combined <- as.factor(df_corr_apo$group_combined)
df_corr_ape$group_combined <- as.factor(df_corr_ape$group_combined)

# Delta
leveneTest(mean_delta_power ~ group_combined, data = df_corr_frontal_filtered_group)

# Beta
leveneTest(mean_beta_power ~ group_combined, data = df_corr_central_filtered_group)

# Offset
leveneTest(mean_aperiodic_offset ~ group_combined, data = df_corr_apo)

# Exponent
leveneTest(mean_aperiodic_exponent ~ group_combined, data = df_corr_ape)

# ----- 8. boxplots and stats -------------------

# Define the output file path
output_folder <- "plots/final/cluster2_groups/plots"

# Define custom colors for the 4 groups
color_palette <- c(
  "self-reported CD_c1" = '#02CAF5',
  "self-reported CD_c2" = '#F59541',
  "no self-reported CD_c1" = '#AA42F5',
  "no self-reported CD_c2" = '#FF5F5F'
)

##---- 8.1 aperiodic exponent general ------------
plot_ape <- df_corr_ape %>%
  group_by(group_combined) %>%
  ggplot(aes(x = group_combined, y = mean_aperiodic_exponent, color = group_combined))+
  geom_boxplot(size = 0.75, outlier.colour = NA, width = 0.5)+
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2)+
  geom_signif(comparisons = combn(unique(df_corr_ape$group_combined), 2, simplify = FALSE),
              map_signif_level = TRUE, test = 'wilcox.test', color = 'black')+
  labs(y = 'mean aperiodic exponent', x = 'cluster')+
  scale_color_manual(values = color_palette) +
  theme_classic() +
  guides(color = FALSE) +
  theme(text = element_text(size = 14))

# Save the plot
ggsave(filename = file.path(output_folder, "aperiodic_exponent_boxplot.png"), plot = plot_ape)

# Wilcoxon test and effect size
pairwise.wilcox.test(df_corr_ape$mean_aperiodic_exponent, df_corr_ape$group_combined, p.adjust.method = "bonferroni")
df_corr_ape %>% ungroup() %>% wilcox_effsize(mean_aperiodic_exponent ~ group_combined)

##-------8.2 aperiodic offset general-----------
plot_apo <- df_corr_apo %>%
  group_by(group_combined) %>%
  ggplot(aes(x = group_combined, y = mean_aperiodic_offset, color = group_combined))+
  geom_boxplot(size = 0.75, outlier.colour = NA, width = 0.5)+
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2)+
  geom_signif(comparisons = combn(unique(df_corr_apo$group_combined), 2, simplify = FALSE),
              map_signif_level = TRUE, test = 'wilcox.test', color = 'black')+
  labs(y = 'mean aperiodic offset', x = 'cluster')+
  scale_color_manual(values = color_palette) +
  theme_classic() +
  guides(color = FALSE) +
  theme(text = element_text(size = 14))

# Save the plot
ggsave(filename = file.path(output_folder, "aperiodic_exponent_boxplot.png"), plot = plot_apo)

pairwise.wilcox.test(df_corr_apo$mean_aperiodic_offset, df_corr_apo$group_combined, p.adjust.method = "bonferroni")
df_corr_apo %>% ungroup() %>% wilcox_effsize(mean_aperiodic_offset ~ group_combined)

# Correlation with age: exponent
df_corr_ape %>%
  ggplot(aes(x = age, y = mean_aperiodic_exponent, color = group_combined))+
  geom_point()+
  geom_smooth(method = lm, color = "black", fill = "grey", se = TRUE)+
  scale_color_manual(values = color_palette) +
  theme_classic() +
  theme(legend.position = c(0.20, 0.15))+
  stat_cor(method = "pearson", label.x = 60, label.y = 1.1, hjust = 0)+
  labs(y = 'mean aperiodic exponent')

cor.test(df_corr_ape$age, df_corr_ape$mean_aperiodic_exponent)

# Correlation with age: offset
df_corr_apo %>%
  ggplot(aes(x = age, y = mean_aperiodic_offset, color = group_combined))+
  geom_point()+
  geom_smooth(method = lm, color = "black", fill = "grey", se = TRUE)+
  scale_color_manual(values = color_palette) +
  theme_classic() +
  theme(legend.position = c(0.20, 0.15))+
  stat_cor(method = "pearson", label.x = 60, label.y = 0.8, hjust = 0)+
  labs(y = 'mean aperiodic offset')

cor.test(df_corr_apo$age, df_corr_apo$mean_aperiodic_offset)

##------ 8.3 Relative Delta Power (Frontal ROI) ---------------
# Run pairwise Wilcoxon test and filter out non-significant comparisons
pairwise_comparisons <- combn(unique(df_corr_frontal_filtered_group$group_combined), 2, simplify = FALSE)

# Perform Wilcoxon tests and store results
pairwise_results <- sapply(pairwise_comparisons, function(groups) {
  test_result <- wilcox.test(
    df_corr_frontal_filtered_group$mean_delta_power[df_corr_frontal_filtered_group$group_combined == groups[1]],
    df_corr_frontal_filtered_group$mean_delta_power[df_corr_frontal_filtered_group$group_combined == groups[2]],
    exact = FALSE
  )
  test_result$p.value
})

# Only keep significant results (p-value < 0.05)
significant_comparisons <- pairwise_comparisons[which(pairwise_results < 0.05)]
p_values <- pairwise_results[which(pairwise_results < 0.05)]

# Plot
plot_rel_delta <- df_corr_frontal_filtered_group %>%
  group_by(group_combined) %>%
  ggplot(aes(x = group_combined, y = mean_delta_power, color = group_combined)) +
  geom_boxplot(size = 0.75, outlier.colour = NA, width = 0.5) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2) +
  geom_signif(
    comparisons = significant_comparisons,
    map_signif_level = TRUE, 
    test = 'wilcox.test', 
    color = 'black',
    annotations = sapply(p_values, function(p) sprintf("p = %.2g", p))
  ) +
  labs(y = 'Mean Relative Delta Power [μV²] (Frontal ROI)', x = 'group and cluster') +
  scale_color_manual(values = color_palette) +
  theme_classic() +
  guides(color = FALSE) +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
        )
# Save the plot
ggsave(filename = file.path(output_folder, "rel_delta_boxplot_1.png"), 
       plot = plot_rel_delta,
       width = 10,
       height = 7)

# Save the plot
ggsave(filename = file.path(output_folder, "rel_delta_boxplot.png"), plot = plot_rel_delta)

# Define pairwise comparisons for the Wilcoxon test
pairwise_comparisons <- combn(unique(df_corr_frontal_filtered_group$group_combined), 2, simplify = FALSE)

# Perform Wilcoxon tests and store p-values
pairwise_results <- sapply(pairwise_comparisons, function(groups) {
  test_result <- wilcox.test(
    df_corr_frontal_filtered_group$mean_delta_power[df_corr_frontal_filtered_group$group_combined == groups[1]],
    df_corr_frontal_filtered_group$mean_delta_power[df_corr_frontal_filtered_group$group_combined == groups[2]],
    exact = FALSE
  )
  test_result$p.value
})

# Create a data frame with the comparisons and their p-values
comparison_df <- data.frame(
  Group1 = sapply(pairwise_comparisons, `[`, 1),
  Group2 = sapply(pairwise_comparisons, `[`, 2),
  p_value = pairwise_results
)

# Display the table of comparisons and p-values
print(comparison_df)


# Pairwise Wilcoxon test
pairwise.wilcox.test(df_corr_frontal_filtered_group$mean_delta_power,
                     df_corr_frontal_filtered_group$group_combined,
                     p.adjust.method = "bonferroni")

# Effect sizes
df_corr_frontal_filtered_group %>%
  ungroup() %>%
  wilcox_effsize(mean_delta_power ~ group_combined)

##------ 8.3 Absolute Delta Power (Frontal ROI) ---------------

# Run pairwise Wilcoxon test and filter out non-significant comparisons
pairwise_comparisons_abs <- combn(unique(df_corr_frontal_filtered_abs$group_combined), 2, simplify = FALSE)

# Perform Wilcoxon tests and store results
pairwise_results_abs <- sapply(pairwise_comparisons_abs, function(groups) {
  test_result <- wilcox.test(
    df_corr_frontal_filtered_abs$mean_delta_power_abs[df_corr_frontal_filtered_abs$group_combined == groups[1]],
    df_corr_frontal_filtered_abs$mean_delta_power_abs[df_corr_frontal_filtered_abs$group_combined == groups[2]],
    exact = FALSE
  )
  test_result$p.value
})

# Only keep significant results (p-value < 0.05)
significant_comparisons_abs <- pairwise_comparisons_abs[which(pairwise_results_abs < 0.05)]
p_values_abs <- pairwise_results_abs[which(pairwise_results_abs < 0.05)]

# Create a data frame to store the significant comparisons and p-values
comparison_df_abs <- data.frame(
  Group1 = sapply(significant_comparisons_abs, `[`, 1),
  Group2 = sapply(significant_comparisons_abs, `[`, 2),
  p_value = p_values_abs
)

# Determine y-positions for annotations to avoid overlap
# Here, you can adjust the y_positions to better fit your data
max_y_abs <- max(df_corr_frontal_filtered_abs$mean_delta_power_abs)
y_positions_abs <- seq(max_y_abs * 0.8, max_y_abs * 0.95, length.out = length(p_values_abs))

# Plot
plot_abs_delta <- df_corr_frontal_filtered_abs %>%
  group_by(group_combined) %>%
  ggplot(aes(x = group_combined, y = mean_delta_power_abs, color = group_combined)) +
  geom_boxplot(size = 0.75, outlier.colour = NA, width = 0.5) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2) +
  geom_signif(
    comparisons = significant_comparisons_abs,
    map_signif_level = TRUE, 
    test = 'wilcox.test', 
    color = 'black',
    y_position = y_positions_abs, # Specify y-positions for annotations
    annotations = sapply(p_values_abs, function(p) sprintf("p = %.2g", p))
  ) +
  labs(y = 'Mean Absolute Delta Power [μV²] (Frontal ROI)', x = 'Group and Cluster') +
  scale_color_manual(values = color_palette) +
  theme_classic() +
  guides(color = FALSE) +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Save the plot
ggsave(filename = file.path(output_folder, "abs_delta_boxplot.png"), plot = plot_abs_delta)

# Save the plot
ggsave(filename = file.path(output_folder, "abs_delta_boxplot_1.png"), 
       plot = plot_abs_delta,
       width = 10,
       height = 7)




# Pairwise Wilcoxon test
pairwise.wilcox.test(df_corr_frontal_filtered_abs$mean_delta_power_abs,
                     df_corr_frontal_filtered_abs$group_combined,
                     p.adjust.method = "bonferroni")

# Effect sizes
df_corr_frontal_filtered_abs %>%
  ungroup() %>%
  wilcox_effsize(mean_delta_power_abs ~ group_combined)

##----- 8.4 Relative Beta Power (Central ROI) -----------------

# Boxplot with significance for all group comparisons
plot_rel_beta <- df_corr_central_filtered_group %>%
  group_by(group_combined) %>%
  ggplot(aes(x = group_combined, y = mean_beta_power, color = group_combined)) +
  geom_boxplot(size = 0.75, outlier.colour = NA, width = 0.5) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2) +
  geom_signif(
    comparisons = combn(unique(df_corr_central_filtered_group$group_combined), 2, simplify = FALSE),
    map_signif_level = TRUE,
    test = "wilcox.test",
    color = "black"
  ) +
  labs(y = 'Mean Relative Beta Power [μV²] (Central ROI)', x = 'group and cluster') +
  scale_color_manual(values = color_palette) +
  theme_classic() +
  guides(color = FALSE) +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
        )

# Save the plot
ggsave(filename = file.path(output_folder, "rel_beta_boxplot.png"), plot = plot_rel_beta)

# Save the plot
ggsave(filename = file.path(output_folder, "rel_beta_boxplot_1.png"), 
       plot = plot_rel_beta,
       width = 10,
       height = 7)

# Pairwise Wilcoxon test (Bonferroni correction)
pairwise.wilcox.test(df_corr_central_filtered_group$mean_beta_power,
                     df_corr_central_filtered_group$group_combined,
                     p.adjust.method = "bonferroni")

# Wilcoxon rank effect sizes
df_corr_central_filtered_group %>%
  ungroup() %>%
  wilcox_effsize(mean_beta_power ~ group_combined)

## --------- 8.5 Tables of All EEG Values (Means and SDs by Group) ----

# Delta power (frontal)
df_corr_frontal_filtered_group %>%
  group_by(group_combined) %>%
  summarise(
    mean_delta = mean(mean_delta_power, na.rm = TRUE),
    sd_delta = sd(mean_delta_power, na.rm = TRUE)
  )

# Beta power (central)
df_corr_central_filtered_group %>%
  group_by(group_combined) %>%
  summarise(
    mean_beta = mean(mean_beta_power, na.rm = TRUE),
    sd_beta = sd(mean_beta_power, na.rm = TRUE)
  )

# Aperiodic exponent
df_corr_ape %>%
  group_by(group_combined) %>%
  summarise(
    mean_ape = mean(mean_aperiodic_exponent, na.rm = TRUE),
    sd_ape = sd(mean_aperiodic_exponent, na.rm = TRUE)
  )

# Aperiodic offset
df_corr_apo %>%
  group_by(group_combined) %>%
  summarise(
    mean_apo = mean(mean_aperiodic_offset, na.rm = TRUE),
    sd_apo = sd(mean_aperiodic_offset, na.rm = TRUE)
  )

