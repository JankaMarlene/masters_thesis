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

# Remove rows with NA values in 'cluster_2'
table_power_5 <- table_power_5 %>%
  filter(!is.na(cluster_2))

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
leveneTest(mean_delta_power~cluster_2,data = df_corr_frontal)# not significant, homogeneity of variance
leveneTest(mean_beta_power~cluster_2,data = df_corr_central)# not significant, homogeneity of variance
#------- 4. demographics-----------------
shapiro_df_c1 <- df_corr_frontal%>%
  filter(cluster_2 == 'c1')

shapiro_df_c2 <- df_corr_frontal%>%
  filter(cluster_2 == 'c2')
# sex
df_corr_frontal%>%
  group_by(cluster_2,sex)%>%
  count()

# age
df_corr_frontal%>%
  group_by(participant_id)%>%
  ggplot(aes(age))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()

df_corr_frontal%>%
  group_by(cluster_2)%>%
  summarise(mean_age = mean(age),
            sd_age = sd(age))

t.test(age~cluster_2, data = df_corr_frontal, alternative = "two.sided")#
wilcox.test(age~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)#
#effsize
df_corr_frontal%>%
  ungroup()%>%
  wilcox_effsize(age~cluster_2)# small

# years of education
df_corr_frontal%>%
  group_by(participant_id)%>%
  ggplot(aes(years_of_education))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()

t.test(years_of_education~cluster_2, data = df_corr_frontal, alternative = "two.sided")# 0.82
wilcox.test(years_of_education~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.85
#effsize
df_corr_frontal%>%
  ungroup()%>%
  wilcox_effsize(years_of_education~cluster_2)# small

# FACIT
df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(facit_f_FS))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# c2 is very skew

shapiro.test(shapiro_df_self-reportedCD$facit_f_FS)
shapiro.test(shapiro_df_no_self-reportedCDPCS$facit_f_FS)
leveneTest(facit_f_FS~cluster_2,data = df_corr_frontal)# not significant
wilcox.test(facit_f_FS~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.0027
#effsize
df_corr_frontal%>%
  ungroup()%>%
  wilcox_effsize(facit_f_FS~cluster_2)# moderate

t.test(facit_f_FS~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# significant p = 0.0031

df_corr_frontal <- df_corr_frontal%>%
  mutate(cluster_2 = as.factor(cluster_2))# otherwise wilcoxon_test from coin does not work

# in order to get the z value
result <- coin::wilcox_test(data = df_corr_frontal,facit_f_FS~cluster_2, comparisons = list(c('c1','c2')), alternative = 'two.sided')


# HADS
df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(hads_d_total_score))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# c2 is very skew, c1 also a little

shapiro.test(shapiro_df_self-reportedCD$hads_d_total_score)
shapiro.test(shapiro_df_no_self-reportedCDPCS$hads_d_total_score)
leveneTest(hads_d_total_score~cluster_2,data = df_corr_frontal)# not significant
t.test(hads_d_total_score~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# p = 0.095
wilcox.test(hads_d_total_score~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.0276
#effsize
df_corr_frontal%>%
  ungroup()%>%
  wilcox_effsize(hads_d_total_score~cluster_2)# moderate

# in order to get the z value
result <- coin::wilcox_test(data = df_corr_frontal,hads_d_total_score~cluster_2, comparisons = list(c('c1','c2')), alternative = 'two.sided')


# TMT A
df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(tmt_a_time))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# looks okay

shapiro.test(shapiro_df_self-reportedCD$tmt_a_time)
shapiro.test(shapiro_df_no_self-reportedCDPCS$tmt_a_time)
leveneTest(tmt_a_time~cluster_2,data = df_corr_frontal)# not significant
t.test(tmt_a_time~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# p = 0.059
wilcox.test(tmt_a_time~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.038
#effsize
df_corr_frontal%>%
  ungroup()%>%
  wilcox_effsize(tmt_a_time~cluster_2)# moderate

# in order to get the z value
result <- coin::wilcox_test(data = df_corr_frontal,tmt_a_time~cluster_2, comparisons = list(c('c1','c2')), alternative = 'two.sided')


# TMT B-A
df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(tmt_diff))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# both skew

shapiro.test(shapiro_df_self-reportedCD$tmt_diff)
shapiro.test(shapiro_df_no_self-reportedCDPCS$tmt_diff)
leveneTest(tmt_diff~cluster_2,data = df_corr_frontal)# not significant
t.test(tmt_diff~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# 0.2434
wilcox.test(tmt_diff~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.2768
#effsize
df_corr_frontal%>%
  ungroup()%>%
  wilcox_effsize(tmt_diff~cluster_2)# small
# in order to get the z value
result <- coin::wilcox_test(data = df_corr_frontal,tmt_diff~cluster_2, comparisons = list(c('c1','c2')), alternative = 'two.sided')


# MOCA
df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(moca))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# both skew

shapiro.test(shapiro_df_self-reportedCD$moca)
shapiro.test(shapiro_df_no_self-reportedCDPCS$moca)
leveneTest(moca~cluster_2,data = df_corr_frontal)# not significant
t.test(moca~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)
wilcox.test(moca~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.2768
#effsize
df_corr_frontal%>%
  ungroup()%>%
  cohens_d(moca ~ cluster_2)

# number of epochs
df_corr_frontal%>%
  group_by(cluster_2)%>%
  summarise(mean_epoch = mean(number_epochs),
            sd_epoch = sd(number_epochs))

t.test(number_epochs~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# 0.4701
#effsize
df_corr_frontal%>%
  ungroup()%>%
  cohens_d(number_epochs~cluster_2)# small

df_corr_frontal%>%
  group_by(cluster_2)%>%
  summarise(max_epoch = max(number_epochs),
            min_epoch = min(number_epochs))

# do they correlate with the power?
df_corr_frontal%>%
  ggplot(aes(x = number_epochs, y = mean_delta_power, color = cluster_2))+
  geom_point()
cor.test(df_corr_frontal$mean_delta_power,df_corr_frontal$number_epochs)

df_corr_central%>%
  ggplot(aes(x = number_epochs, y = mean_beta_power, color = cluster_2))+
  geom_point()
cor.test(df_corr_central$mean_delta_power,df_corr_central$number_epochs)

# number of epochs correlates with fatigue score!
table_power_5%>%
  ggplot(aes(x = number_epochs, y = facit_f_FS))+
  geom_point()
cor.test(table_power_5$number_epochs, table_power_5$facit_f_FS)

# summarize values into one table
table_behav <- df_corr_frontal%>%
  group_by(cluster_2)%>%
  summarise(mean_facit = mean(facit_f_FS, na.rm = T),
            sd_facit = sd(facit_f_FS, na.rm = T),
            mean_hads = mean(hads_d_total_score, na.rm = T),
            sd_hads = sd(hads_d_total_score, na.rm = T),
            mean_tmta = mean(tmt_a_time),
            sd_tmta = sd(tmt_a_time),
            mean_tmtb_a = mean(tmt_diff),
            sd_tmtb_a = sd(tmt_diff),
            mean_y_o = mean(years_of_education),
            sd_y_o = sd(years_of_education),
            mean_epoc = mean(number_epochs),
            sd_epoc = sd(number_epochs),
            mean_moca = mean(moca, na.rm = T),
            sd_moca = sd(moca, na.rm = T))

# good channels
channel_artefacts <- table_power_5%>%
  group_by(cluster_2)%>%
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
# relative delta power frontal
df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power, color = cluster_2))+
  geom_boxplot()# two outliers in the c2 cluster_2


# outlier removal inside the participant with +/- 3SD
table_delta_filtered <-table_power_5%>%
  group_by(participant_id) %>%
  mutate(mean_rel_delta = mean(rel_delta),
         sd_rel_delta = sd(rel_delta),
         lower_bound = mean_rel_delta - 3 * sd_rel_delta,
         upper_bound = mean_rel_delta + 3 * sd_rel_delta) %>%
  filter(rel_delta >= lower_bound & rel_delta <= upper_bound) %>%
  ungroup()

# last step: remove negative values
table_delta_filtered$rel_delta <- ifelse(
  table_delta_filtered$rel_delta < 0, 0, table_delta_filtered$rel_delta)

# visualize the filtered data
table_delta_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = rel_delta, color = cluster_2))+
  geom_boxplot(outlier.colour = 'black')+
  geom_jitter()

# select only frontal channels
table_delta_frontal_filtered <- table_delta_filtered%>%
  filter(table_delta_filtered$channel %in% frontal_channels)

df_corr_frontal_filtered <- table_delta_frontal_filtered%>%
  group_by(participant_id,cluster_2,tmt_a_time,facit_f_FS, tmt_diff,age,years_of_education,moca)%>%
  summarise(mean_delta_power = mean(rel_delta))

df_corr_frontal_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power, color = cluster_2))+
  geom_boxplot(outlier.colour = 'black')+
  geom_jitter()# 4 outliers in c2

# additional filtering (across cluster_2)
table_delta_filtered_group <- table_delta_filtered%>%
  group_by(cluster_2)%>%
  mutate(mean_rel_delta = mean(rel_delta),
         sd_rel_delta = sd(rel_delta),
         lower_bound = mean_rel_delta - 3 * sd_rel_delta,
         upper_bound = mean_rel_delta + 3 * sd_rel_delta) %>%
  filter(rel_delta >= lower_bound & rel_delta <= upper_bound) %>%
  ungroup()

table_frontal_filtered_group <- table_delta_filtered_group%>%
  filter(table_delta_filtered_group$channel %in% frontal_channels)

df_corr_frontal_filtered_group <- table_frontal_filtered_group%>%
  group_by(participant_id,cluster_2,tmt_a_time,facit_f_FS, tmt_diff,age,moca,hads_d_total_score)%>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_delta_power = mean(rel_delta),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

df_corr_frontal_filtered_group%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power, color = cluster_2))+
  geom_boxplot(outlier.colour = 'black')+
  geom_jitter()# 2 outliers in c2 and 1 in c1


# outliers delta absolute
table_delta_filtered_abs <-table_power_5%>%
  group_by(participant_id) %>%
  mutate(mean_abs_delta = mean(abs_delta),
         sd_abs_delta = sd(abs_delta),
         lower_bound = mean_abs_delta - 3 * sd_abs_delta,
         upper_bound = mean_abs_delta + 3 * sd_abs_delta) %>%
  filter(abs_delta >= lower_bound & abs_delta <= upper_bound) %>%
  ungroup()

table_delta_frontal_filtered_abs <- table_delta_filtered_abs%>%
  filter(table_delta_filtered_abs$channel %in% frontal_channels)

df_corr_frontal_filtered_abs <- table_delta_frontal_filtered_abs%>%
  group_by(participant_id,cluster_2,tmt_a_time,facit_f_FS, tmt_diff,age,years_of_education)%>%
  summarise(mean_delta_power_abs = mean(abs_delta))

df_corr_frontal_filtered_abs%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power_abs, color = cluster_2))+
  geom_boxplot(outlier.colour = 'black')+
  geom_jitter()

kruskal.test(mean_delta_power_abs~cluster_2, data = df_corr_frontal_filtered_abs)

## ----------- 5.2 relative beta power central ----------------------------
df_corr_central%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_beta_power, color = cluster_2))+
  geom_boxplot()# one outlier in c2, one in the c1 cluster_2

# sd +- 3 for beta
table_beta_filtered <-table_power_5%>%
  group_by(participant_id) %>%
  mutate(mean_rel_beta = mean(rel_beta),
         sd_rel_beta = sd(rel_beta),
         lower_bound = mean_rel_beta - 3 * sd_rel_beta,
         upper_bound = mean_rel_beta + 3 * sd_rel_beta) %>%
  filter(rel_beta >= lower_bound & rel_beta <= upper_bound) %>%
  ungroup()

table_central_filtered <- table_beta_filtered%>%
  filter(table_beta_filtered$channel %in% central_channels)

df_corr_central_filtered <- table_central_filtered%>%
  group_by(participant_id,cluster_2,tmt_a_time,facit_f_FS, tmt_diff,age, moca,hads_d_total_score)%>%
  summarise(mean_beta_power = mean(rel_beta))

df_corr_central_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_beta_power, color = cluster_2))+
  geom_boxplot(outlier.colour = 'black')+
  geom_jitter()# one outlier in c2 cluster_2 and one in c1 cluster_2

# additional filtering (across cluster_2)
table_beta_filtered_group <- table_beta_filtered%>%
  group_by(cluster_2)%>%
  mutate(mean_rel_beta = mean(rel_beta),
         sd_rel_beta = sd(rel_beta),
         lower_bound = mean_rel_beta - 3 * sd_rel_beta,
         upper_bound = mean_rel_beta + 3 * sd_rel_beta) %>%
  filter(rel_beta >= lower_bound & rel_beta <= upper_bound) %>%
  ungroup()

table_central_filtered_group <- table_beta_filtered_group%>%
  filter(table_beta_filtered_group$channel %in% central_channels)

df_corr_central_filtered_group <- table_central_filtered_group%>%
  group_by(participant_id,cluster_2,tmt_a_time,facit_f_FS, tmt_diff,age, moca, hads_d_total_score)%>%
  summarise(mean_beta_power = mean(rel_beta))

df_corr_central_filtered_group%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_beta_power, color = cluster_2))+
  geom_boxplot(outlier.colour = 'black')+
  geom_jitter()# 1 outlier in c2 and 2 in c1

## ----------- 5.3 aperiodic components -------------------------------------------
# +- 3 sd for aperiodic exponent
table_ape_filtered <-table_power_5%>%
  group_by(participant_id) %>%
  mutate(mean_ape = mean(aperiodic_exponent),
         sd_ape = sd(aperiodic_exponent),
         lower_bound = mean_ape - 3 * sd_ape,
         upper_bound = mean_ape + 3 * sd_ape) %>%
  filter(aperiodic_exponent >= lower_bound & aperiodic_exponent <= upper_bound) %>%
  ungroup()


df_corr_ape <- table_ape_filtered%>%
  group_by(participant_id,cluster_2,tmt_a_time,facit_f_FS, tmt_diff,age,moca,hads_d_total_score)%>%
  summarise(mean_aperiodic_exponent = mean(aperiodic_exponent))

df_corr_ape%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_aperiodic_exponent, color = cluster_2))+
  geom_boxplot()+
  geom_jitter()

# +- 3 SD for aperiodic offset
table_apo_filtered <-table_power_5%>%
  group_by(participant_id) %>%
  mutate(mean_apo = mean(aperiodic_offset),
         sd_apo = sd(aperiodic_offset),
         lower_bound = mean_apo - 3 * sd_apo,
         upper_bound = mean_apo + 3 * sd_apo) %>%
  filter(aperiodic_offset >= lower_bound & aperiodic_offset <= upper_bound) %>%
  ungroup()

df_corr_apo <- table_apo_filtered%>%
  group_by(participant_id,cluster_2,tmt_a_time,facit_f_FS, tmt_diff,age,moca,hads_d_total_score)%>%
  summarise(mean_aperiodic_offset = mean(aperiodic_offset))

df_corr_apo%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_aperiodic_offset, color = cluster_2))+
  geom_boxplot()+
  geom_jitter()

#-------- 6. export tables for topoplots ---------------------
# beta power
export_beta_c1 <- table_beta_filtered_group%>%
  filter(cluster_2 == 'c1')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_rel_beta = mean(rel_beta))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

export_beta_c2 <- table_beta_filtered_group%>%
  filter(cluster_2 == 'c2')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_rel_beta = mean(rel_beta))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

# delta power
export_delta_c1 <- table_delta_filtered_group%>%
  filter(cluster_2 == 'c1')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_rel_delta = mean(rel_delta))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

export_delta_c2 <- table_delta_filtered_group%>%
  filter(cluster_2 == 'c2')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_rel_delta = mean(rel_delta))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

# save in folder
write.table(export_beta_c1, file = "export_beta_c1.txt", row.names = FALSE, col.names = FALSE)
write.table(export_beta_c2, file = "export_beta_c2.txt", row.names = FALSE, col.names = FALSE)
write.table(export_delta_c1, file = "export_delta_c1.txt", row.names = FALSE, col.names = FALSE)
write.table(export_delta_c2, file = "export_delta_c2.txt", row.names = FALSE, col.names = FALSE)

# have a look at min/max values for visualisation purposes
export_beta_c2%>%
  summarise(min = min(mean_rel_beta),
            max = max(mean_rel_beta))

export_beta_c1%>%
  summarise(min = min(mean_rel_beta),
            max = max(mean_rel_beta))

export_delta_c2%>%
  summarise(min = min(mean_rel_delta),
            max = max(mean_rel_delta))

export_delta_c1%>%
  summarise(min = min(mean_rel_delta),
            max = max(mean_rel_delta))

# now the same for aperiodic exponent
export_ape_c1 <- table_ape_filtered%>%
  filter(cluster_2 == 'c1')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_ape = mean(aperiodic_exponent))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

export_ape_c2 <- table_ape_filtered%>%
  filter(cluster_2 == 'c2')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_ape = mean(aperiodic_exponent))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

export_apo_c1 <- table_apo_filtered%>%
  filter(cluster_2 == 'c1')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_apo = mean(aperiodic_offset))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

export_apo_c2 <- table_apo_filtered%>%
  filter(cluster_2 == 'c2')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_apo = mean(aperiodic_offset))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

write.table(export_ape_c1, file = "export_ape_c1.txt", row.names = FALSE, col.names = FALSE)
write.table(export_ape_c2, file = "export_ape_c2.txt", row.names = FALSE, col.names = FALSE)
write.table(export_apo_c1, file = "export_apo_c1.txt", row.names = FALSE, col.names = FALSE)
write.table(export_apo_c2, file = "export_apo_c2.txt", row.names = FALSE, col.names = FALSE)

export_ape_c1%>%
  summarise(min = min(mean_ape),
            max = max(mean_ape))

export_ape_c2%>%
  summarise(min = min(mean_ape),
            max = max(mean_ape))

export_apo_c1%>%
  summarise(min = min(mean_apo),
            max = max(mean_apo))

export_apo_c2%>%
  summarise(min = min(mean_apo),
            max = max(mean_apo))

# now the same with the r squared
export_r_c2 <- table_power_5%>%
  filter(cluster_2 == 'c2')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_r = mean(r_squared, na.rm = T))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

export_r_c1 <- table_power_5%>%
  filter(cluster_2 == 'c1')%>% 
  mutate(channel = as.numeric(channel)) %>%
  group_by(channel)%>%
  summarise(mean_r = mean(r_squared, na.rm = T))%>%
  arrange(channel)%>%
  mutate(channel = replace(channel, is.na(channel), "Gnd"))  

write.table(export_r_c1, file = "export_r_c1.txt", row.names = FALSE, col.names = FALSE)
write.table(export_r_c2, file = "export_r_c2.txt", row.names = FALSE, col.names = FALSE)

export_r_c1%>%
  summarise(min = min(mean_r),
            max = max(mean_r))

export_r_c2%>%
  summarise(min = min(mean_r),
            max = max(mean_r))

#--------- 7. check requirements-----------------------------
# I need data sets per cluster_2 in order to check the normality requirement separately
shapiro_df_c1 <- df_corr_frontal_filtered_group%>%
  filter(cluster_2 == '1')

shapiro_df_c2 <- df_corr_frontal_filtered_group%>%
  filter(cluster_2 == '2')

# normality
df_corr_frontal_filtered_group%>%
  ggplot(aes(x = mean_delta_power))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# looks a bit weird but a similar kind of weird

shapiro.test(shapiro_df_c1$mean_delta_power)# 0.02
shapiro.test(shapiro_df_c2$mean_delta_power)# 0.0388

# normality beta
shapiro_df_c1 <- df_corr_central_filtered_group%>%
  filter(cluster_2 == 'c1')
shapiro_df_c2 <- df_corr_central_filtered_group%>%
  filter(cluster_2 == 'c2')

df_corr_central_filtered%>%
  ggplot(aes(x = mean_beta_power))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')# looks really skew (bot equally skew in both groups, a bit worse in c1 maybe) -> maybe use nonparametric stats

shapiro.test(shapiro_df_c1$mean_beta_power)# 8.953e-05
shapiro.test(shapiro_df_c2$mean_beta_power)# 0.001104

# normality aperiodic offset
shapiro_df_c1<- df_corr_apo%>%
  filter(cluster_2 == 'c1')
shapiro_df_c2 <- df_corr_apo%>%
  filter(cluster_2 == 'c2')

df_corr_apo%>%
  ggplot(aes(x = mean_aperiodic_offset))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# looks quite normally distributed

shapiro.test(shapiro_df_c1$mean_aperiodic_offset)# 0.3911
shapiro.test(shapiro_df_c2$mean_aperiodic_offset)# 0.4375

# normality aperiodic exponent
shapiro_df_c1 <- df_corr_ape%>%
  filter(cluster_2 == 'c1')
shapiro_df_c2 <- df_corr_ape%>%
  filter(cluster_2 == 'c2')

df_corr_ape%>%
  ggplot(aes(x = mean_aperiodic_exponent))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')# looks different between the groups

shapiro.test(shapiro_df_c1$mean_aperiodic_exponent)# 0.01124
shapiro.test(shapiro_df_c2$mean_aperiodic_exponent)# 0.5865

# variance
leveneTest(mean_delta_power~cluster_2,data = df_corr_frontal_filtered_group)# not significant
leveneTest(mean_beta_power~cluster_2,data = df_corr_central_filtered_group)# not significant
leveneTest(mean_aperiodic_offset~cluster_2,data = df_corr_apo)# 0.0451
leveneTest(mean_aperiodic_exponent~cluster_2,data = df_corr_ape)# not significant
leveneTest(mean_beta1_power~cluster_2,data = df_corr_central1_filtered)# not significant
leveneTest(mean_beta2_power~cluster_2,data = df_corr_central2_filtered)# not significant


# conclusion: variances are not that big of a problem, normality is though! with the aperiodic offset we have normality in both groups
# but then there is no equal variances in that case
# => use NONPARAMETRIC Tests for beta, delta and the exponent + offset?

# ----- 8. boxplots and stats -------------------
# Define custom colors
color_palette <- c("c1" = '#02CAF5',
                   "c2" = "#F59541")

##---- 8.1 aperiodic exponent general ------------
# mean
df_corr_ape%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_aperiodic_exponent, color = cluster_2))+
  geom_boxplot(size = 0.75,outlier.colour = 'black', width=0.5)+
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2)+
  geom_signif(comparisons = list(c("c1","c2")),map_signif_level = function(p) sprintf("p = %.2g", p), test = 'wilcox.test', color = 'black')+
  labs(y = 'mean aperiodic exponent')+
  scale_color_manual(values = color_palette) +
  theme_classic()+
  guides(color = FALSE)+
  theme(
    text = element_text(size = 14)  # Adjust the size here
  )


wilcox.test(mean_aperiodic_exponent~cluster_2, data = df_corr_ape, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.6057

df_corr_ape <- df_corr_ape%>%
  mutate(cluster_2 = as.factor(cluster_2))# otherwise wilcoxon_test from coin does not work

# in order to get the z value
result <- coin::wilcox_test(data = df_corr_ape,mean_aperiodic_exponent~cluster_2, comparisons = list(c('c1','c2')), alternative = 'two.sided')

# get the effsize
df_corr_ape%>%
  ungroup()%>% # apparently you have to ungroup here, otherwise, wilcox_effsize does not work
  wilcox_effsize(mean_aperiodic_exponent~cluster_2)

##-------8.2 aperiodic offset general-----------
# mean
df_corr_apo%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_aperiodic_offset, color = cluster_2))+
  geom_boxplot(size = 0.75,outlier.colour = 'NA', width=0.5)+
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2)+
  geom_signif(comparisons = list(c("c1","c2")),map_signif_level = function(p) sprintf("p = %.2g", p), test = 'wilcox.test', color = 'black')+
  labs(y = 'mean aperiodic offset')+
  scale_color_manual(values = color_palette) +
  theme_classic()+
  guides(color = FALSE)+
  theme(
    text = element_text(size = 14)  # Adjust the size here
  )

wilcox.test(mean_aperiodic_offset~cluster_2, data = df_corr_apo, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)
df_corr_apo <- df_corr_apo%>%
  mutate(cluster_2 = as.factor(cluster_2))# otherwise wilcoxon_test from coin does not work
result <- coin::wilcox_test(data = df_corr_apo,mean_aperiodic_offset~cluster_2, comparisons = list(c('c1','c2')), alternative = 'two.sided')
# get the effsize
df_corr_apo%>%
  ungroup()%>% # apparently you have to ungroup here, otherwise, wilcox_effsize does not work
  wilcox_effsize(mean_aperiodic_offset~cluster_2)

# but be aware that the c2 cluster_2 is a bit younger
# exponent
df_corr_ape%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = age, y = mean_aperiodic_exponent, color = cluster_2))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE)+
  scale_color_manual(values = color_palette) +
  theme_classic() +
  theme(legend.position = c(0.20, 0.15))+
  stat_cor(aes(color = "Correlation: "),method = "pearson", label.x = 60, label.y = 1.1,hjust=0)+
  labs(y = 'mean aperiodic exponent')

cor.test(df_corr_ape$age,df_corr_ape$mean_aperiodic_exponent)# r -.61, p 6.83e-06

# offset
df_corr_apo%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = age, y = mean_aperiodic_offset, color = cluster_2))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE)+
  scale_color_manual(values = color_palette) +
  theme_classic() +
  theme(legend.position = c(0.20, 0.15))+
  stat_cor(aes(color = "Correlation: "),method = "pearson", label.x = 60, label.y = 0.8,hjust=0)+
  labs(y = 'mean aperiodic offset')

cor.test(df_corr_apo$age,df_corr_apo$mean_aperiodic_offset)# significant (p = 0.036) r = -0.31

##------ 8.3 rel and absolute delta frontal---------------
df_corr_frontal_filtered_group%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power, color = cluster_2))+
  geom_boxplot(size = 0.75,outlier.colour = 'NA', width=0.5)+
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2)+
  geom_signif(comparisons = list(c("c1","c2")),map_signif_level = TRUE, color = 'black')+
  labs(y = 'mean delta power [μV^2] in frontal ROI')+
  scale_color_manual(values = color_palette) +
  theme_classic()+
  guides(color = FALSE)+
  theme(
    text = element_text(size = 14)  # Adjust the size here
  )

# in order to get the W statistics
wilcox.test(mean_delta_power~cluster_2, data = df_corr_frontal_filtered_group, 
            alternative = 'less',
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)

df_corr_frontal_filtered_group <-df_corr_frontal_filtered_group%>%
  mutate(cluster_2 = as.factor(cluster_2))# otherwise wilcoxon_test from coin does not work

# in order to get the z value
result <- coin::wilcox_test(data = df_corr_frontal_filtered_group,mean_delta_power~cluster_2, comparisons = list(c('c1','c2')), alternative = 'less')

# get the effsize
df_corr_frontal_filtered_group%>%
  ungroup()%>%
  wilcox_effsize(mean_delta_power~cluster_2)

# absolute delta
df_corr_frontal_filtered_abs%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power_abs))+
  geom_boxplot()+
  geom_jitter(width = 0.3, height = 0, alpha = 0.1)

wilcox.test(mean_delta_power_abs~cluster_2, data = df_corr_frontal_filtered_abs, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)
t.test(mean_delta_power_abs~cluster_2, data = df_corr_frontal_filtered_abs, alternative = "less", paired = FALSE)

##----- 8.4 mean beta -----------------
df_corr_central_filtered_group%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_beta_power, color = cluster_2))+
  geom_boxplot(size = 0.75,outlier.colour = NA, width=0.5)+
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2)+   
  geom_signif(comparisons = list(c("c1","c2")),map_signif_level = TRUE, color = 'black')+
  labs(y = 'mean beta power [μV^2] in central ROI')+
  scale_color_manual(values = color_palette) +
  theme_classic()+
  guides(color = FALSE)+
  theme(
    text = element_text(size = 14)  # Adjust the size here
  )


wilcox.test(mean_beta_power~cluster_2, data = df_corr_central_filtered_group, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)

df_corr_central_filtered_group <-df_corr_central_filtered_group%>%
  mutate(cluster_2 = as.factor(cluster_2))# otherwise wilcoxon_test from coin does not work

# in order to get the z value
result <- coin::wilcox_test(data = df_corr_central_filtered_group,mean_beta_power~cluster_2, comparisons = list(c('c1','c2')), alternative = 'two.sided')

# get effsize
df_corr_central_filtered_group%>%
  ungroup()%>%
  wilcox_effsize(mean_beta_power~cluster_2)

## --------- 8.5 tables of all EEG values ----
df_corr_frontal_filtered_group%>%
  group_by(cluster_2)%>%
  summarise(mean_delta = mean(mean_delta_power),
            sd_delta = sd(mean_delta_power))

df_corr_central_filtered_group%>%
  group_by(cluster_2)%>%
  summarise(mean_beta = mean(mean_beta_power),
            sd_beta = sd(mean_beta_power))

df_corr_ape%>%
  group_by(cluster_2)%>%
  summarise(mean_ape = mean(mean_aperiodic_exponent),
            sd_ape = sd(mean_aperiodic_exponent))

df_corr_apo%>%
  group_by(cluster_2)%>%
  summarise(mean_apo = mean(mean_aperiodic_offset),
            sd_apo = sd(mean_aperiodic_offset))

#------ 9. plot behavioral data and corr test ------
## ------- 9.1 just behavioral data ---------------
# TMT A
df_corr_frontal_filtered%>%
  ggplot(aes(x = cluster_2, y = tmt_a_time))+
  geom_boxplot()+
  geom_jitter(width = 0.3, height = 0, alpha = 0.1)

# FACIT
df_corr_frontal_filtered_group%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = facit_f_FS, color = cluster_2))+
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
  ggplot(aes(x = cluster_2, y = tmt_diff))+
  geom_boxplot()+
  geom_jitter(width = 0.3, height = 0, alpha = 0.1)

# moca
df_corr_frontal_filtered%>%
  ggplot(aes(x = cluster_2, y = moca))+
  geom_boxplot()+
  geom_jitter(width = 0.3, height = 0, alpha = 0.1)

wilcox.test(moca~cluster_2, data = df_corr_frontal_filtered, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)

# FACIT and HADS-D
p8<- df_corr_frontal_filtered_group%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  ggplot(aes(x = hads_d_total_score,y = facit_f_FS, color = cluster_2))+
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
  group_by(cluster_2)%>%
  ggplot(aes(x = mean_delta_power,y = tmt_a_time, color = cluster_2))+
  geom_point()

cor.test(df_corr_frontal_filtered_group$mean_delta_power,df_corr_frontal_filtered_group$tmt_a_time, method = 'spearman', exact = FALSE)

df_corr_frontal_filtered_group%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = tmt_diff,y = mean_delta_power,color = cluster_2))+
  geom_point()

cor.test(df_corr_frontal_filtered_group$mean_delta_power,df_corr_frontal_filtered_group$tmt_diff, method = 'spearman', exact = FALSE)


### ------ 9.2.2 rel delta and moca ---------------
df_corr_frontal_filtered_group%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = moca,y = mean_delta_power,color = cluster_2))+
  geom_point()

cor.test(df_corr_frontal_filtered$mean_delta_power,df_corr_frontal_filtered$moca, method = 'spearman', exact = FALSE)

###-------- 9.2.3 relative and absolute delta and FACIT score ------------------------
p1<- df_corr_frontal_filtered_group%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  ggplot(aes(x = mean_delta_power,y = facit_f_FS, color = cluster_2))+
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
  filter(cluster_2 == 'c1')%>%
  ggplot(aes(x = mean_delta_power,y = facit_f_FS))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) +
  theme_classic() +
  theme(legend.position = c(0.25, 0.15))

ggMarginal(p5, type = "densigram")


test <- df_corr_frontal_filtered_group%>%
  filter(cluster_2 == 'c1')

cor.test(test$mean_delta_power,test$facit_f_FS) # 

p6 <- df_corr_frontal_filtered_group%>%
  filter(cluster_2 == 'c2')%>%
  ggplot(aes(x = mean_delta_power,y = facit_f_FS))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) +
  theme_classic() +
  theme(legend.position = c(0.25, 0.15))

ggMarginal(p6, type = "densigram")

test2 <- df_corr_frontal_filtered_group%>%
  filter(cluster_2 == 'c2')

cor.test(test2$mean_delta_power,test2$facit_f_FS) 


# absolute delta power
p1<- df_corr_frontal_filtered_abs%>%
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  ggplot(aes(x = mean_delta_power_abs,y = facit_f_FS, color = cluster_2))+
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
  mutate(cluster_2 = fct_recode(cluster_2,
                            "c1" = "c1",
                            "c2" = "c2"))%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = mean_delta_power,y = hads_d_total_score, color = cluster_2))+
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
  group_by(cluster_2)%>%
  ggplot(aes(x = mean_beta_power,y = tmt_a_time, color = cluster_2))+
  geom_point()

cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$tmt_a_time, method = 'spearman', exact = FALSE)


df_corr_central_filtered_group%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = mean_beta_power,y = tmt_diff,color = cluster_2))+
  geom_point()

cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$tmt_diff, method = 'spearman', exact = FALSE)

###---- 9.2.6 relative beta power and with FACIT score --------------------
df_corr_central_filtered_group%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = mean_beta_power,y = facit_f_FS,color = cluster_2))+
  geom_point()

cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$facit_f_FS, method = 'spearman', exact = FALSE)
#hads
cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$hads_d_total_score, method = 'spearman', exact = FALSE)
#moca
cor.test(df_corr_central_filtered_group$mean_beta_power,df_corr_central_filtered_group$moca, method = 'spearman', exact = FALSE)

# beta 1
df_corr_central1_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = mean_beta1_power,y = facit_f_FS,color = cluster_2))+
  geom_point()

cor.test(df_corr_central1_filtered$mean_beta1_power,df_corr_central1_filtered$facit_f_FS, method = 'spearman', exact = FALSE)

# beta 2
df_corr_central2_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = mean_beta2_power,y = facit_f_FS,color = cluster_2))+
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
