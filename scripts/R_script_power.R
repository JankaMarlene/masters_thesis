## R Script for power analysis
# content
# 1. load packages
# 2. load data
# 3. demographics
# 4. checking requirements
# 5. have a look at the data (plots)

#------------ 1. load packages------------------
library(tidyverse)
library(carData)
library(car)
library(readr)
library(ggdist)
library(ggExtra)# displaying distributions next to plots
library(ggsignif)# displaying stats in plots
library(ggplot2)
library(ggpubr)
library(coin)# need this for z value of wilcox test
library(effsize)# for cohens d
library(rstatix)# for wilcox test
library(dplyr)

#--------------- 2. load data--------------------
# load csv file that I created in MATLAB (has ID, channel, aperiodic offset, aperidic exponent, abs and rel delta and beta power)

table_power_5 <- read_csv("data/analysis_power/table_power_final_5.csv")

number_of_epochs_5 <- read_csv("data/analysis_power/number_of_epochs_5.csv")

# this would be the amount of people where more than half of the data is good
number_of_epochs%>%
  filter(number_epochs > 37.5)# here 70

number_of_epochs%>%
  filter(number_epochs > 50)# here 61

number_of_epochs%>%
  filter(number_epochs < 51)# these subjects have to be excluded

number_of_epochs_5%>%
  filter(number_epochs > 37.5)# here 62

number_of_epochs_5%>%
  filter(number_epochs < 37.5)# these subjects have to be excluded

# I am using the 5 second data for the power analysis!

# modify table (f.ex. add tmt b-a)
table_power_5 <- table_power_5%>%
  mutate(facit_f_FS = as.numeric(facit_f_FS),
         tmt_b_minus_a = tmt_b_time-tmt_a_time)

table_power_5 <- merge(table_power_5, number_of_epochs_5)

#test <- table_power%>%
# group_by(participant_id,group,number_epochs)%>%
# summarise(mean_delta_power = mean(rel_delta))%>%
# filter(number_epochs > 37.5)%>%
# count()

test_table <- table_power_5%>%
  group_by(participant_id,group,tmt_a_time,facit_f_FS, tmt_b_minus_a,age,years_of_education,number_epochs,cluster_2)%>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_beta_power = mean(rel_beta),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

test_table%>%
  group_by(cluster_2)%>%
  summarise(mean_age = mean(age),
            sd_age = sd(age),
            mean_epochs = mean(number_epochs))# sind immerhin fast identisch vom Alter her
t.test(age~cluster_2, data = test_table, alternative = "two.sided")

# Define the channel names you want to select (for delta)
frontal_channels <- c('22','105','11','40','75','39','49','82','48','19','112','25','94','93','83','92','95','96','21','50','10','59','26')

# Filter rows with the specified channel names
table_power_frontal <- table_power_5%>%
  filter(table_power_5$channel %in% frontal_channels)

# Define the channel names you want to select (for beta)
central_channels <- c('85','34','65','37','90','66','1','68','3','67','2','70','74','76','81','34','37','42','86','43','87','44','88','45','89','46','77','5','78','6','7','79','8','80','71','35','72','36','73')

# Filter rows with the specified channel names
table_power_central <- table_power_5%>%
  filter(table_power_5$channel %in% central_channels)

#-------3. summarise mean -----------------
df_corr_frontal <- table_power_frontal%>%
  group_by(participant_id,group,tmt_a_time,facit_f_FS, tmt_b_minus_a,age,years_of_education,sex,hads_d_total_score, number_epochs, cluster_2)%>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_beta_power = mean(rel_beta),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

df_corr_central <- table_power_central%>%
  group_by(participant_id,group,tmt_a_time,facit_f_FS, tmt_b_minus_a,hads_d_total_score, number_epochs, cluster_2)%>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_beta_power = mean(rel_beta),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

#------- 4. demographics-----------------
# age
df_corr_frontal%>%
  group_by(participant_id)%>%
  ggplot(aes(age))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()

# years of education
df_corr_frontal%>%
  group_by(participant_id)%>%
  ggplot(aes(years_of_education))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()

t.test(years_of_education~cluster_2, data = df_corr_frontal, alternative = "two.sided")
wilcox.test(years_of_education~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# without PCS hat eine schiefe Verteilung

df_corr_frontal%>%
  group_by(cluster_2)%>%
  count()

df_corr_frontal%>%
  group_by(cluster_2,sex)%>%
  count()

df_corr_frontal%>%
  group_by(cluster_2)%>%
  summarise(mean_age = mean(age),
            sd_age = sd(age))

t.test(age~cluster_2, data = df_corr_frontal, alternative = "two.sided")
wilcox.test(age~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.69

table_behav <- df_corr_frontal%>%
  group_by(cluster_2)%>%
  summarise(mean_facit = mean(facit_f_FS, na.rm = T),
            sd_facit = sd(facit_f_FS, na.rm = T),
            mean_hads = mean(hads_d_total_score, na.rm = T),
            sd_hads = sd(hads_d_total_score, na.rm = T),
            mean_tmta = mean(tmt_a_time),
            sd_tmta = sd(tmt_a_time),
            mean_tmtb_a = mean(tmt_b_minus_a),
            sd_tmtb_a = sd(tmt_b_minus_a),
            mean_y_o = mean(years_of_education),
            sd_y_o = sd(years_of_education),
            mean_epoc = mean(number_epochs),
            sd_epoc = sd(number_epochs))

df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(facit_f_FS))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# eine Gruppe ist schief!

leveneTest(facit_f_FS~cluster_2,data = df_corr_frontal)# nicht significant

# wilcox.test
wilcox.test(facit_f_FS~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# 0.002
t.test(facit_f_FS~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# significant p = 0.001083

df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(hads_d_total_score))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# eine Gruppe ist schief

leveneTest(hads_d_total_score~cluster_2,data = df_corr_frontal)# not significant

t.test(hads_d_total_score~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# significant p = 0.001083
wilcox.test(hads_d_total_score~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# without PCS hat eine schiefe Verteilung


df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(tmt_a_time))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# sieht okay aus

leveneTest(tmt_a_time~cluster_2,data = df_corr_frontal)# not significant

t.test(tmt_a_time~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# significant p = 0.001083
wilcox.test(tmt_a_time~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)

df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(tmt_b_minus_a))+
  geom_histogram(color = "black",
                 fill = "white", bins = sqrt(100))+
  facet_wrap(~cluster_2,scales = 'free')+
  theme_classic()# beide leicht schief (gleich)

leveneTest(tmt_b_minus_a~cluster_2,data = df_corr_frontal)# not significant
t.test(tmt_b_minus_a~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)# significant p = 0.001083
wilcox.test(tmt_b_minus_a~cluster_2, data = df_corr_frontal, 
            exact = FALSE, 
            correct = FALSE, 
            conf.int = FALSE)# without PCS hat eine schiefe Verteilung

# number of epochs
df_corr_frontal%>%
  group_by(cluster_2)%>%
  summarise(mean_epoch = mean(number_epochs),
            sd_age = sd(number_epochs))


t.test(number_epochs~cluster_2, data = df_corr_frontal, alternative = "two.sided", paired = FALSE)

df_corr_frontal%>%
  ggplot(aes(x = number_epochs, y = mean_delta_power))+
  geom_point()

cor.test(df_corr_frontal$mean_delta_power,df_corr_frontal$number_epochs)

df_corr_central%>%
  ggplot(aes(x = number_epochs, y = mean_beta_power))+
  geom_point()
cor.test(df_corr_central$mean_delta_power,df_corr_central$number_epochs)

#------ 5. exclude outliers--------
# how exactly?

# have a look at the whole data set (boxplots)
table_power_5%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = group, y = rel_delta, color = group))+
  geom_boxplot() 

# per group
table_power_5%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = group, y = rel_beta, color = group))+
  geom_boxplot() 


# just the frontal channels
table_power_frontal%>%
  group_by(participant_id, cluster_2)%>%
  ggplot(aes(x = participant_id, y = rel_delta, color = group))+
  geom_boxplot()#+
#geom_jitter(width = 0.2, height = 0, alpha = 0.5)

# just per group
df_corr_frontal%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = group, y = mean_delta_power, color = group))+
  geom_boxplot()


# just per group (beta)
df_corr_central%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = group, y = mean_beta_power, color = group))+
  geom_boxplot()

# outliers delta relative
# Group data by participant_id and filter outliers

# power values under 0 are clearly wrong -> have to be excluded
table_power_5 <- table_power_5%>%
  filter(rel_delta > 0)


table_delta_filtered <-table_power_5%>%
  group_by(participant_id) %>%
  mutate(mean_rel_delta = mean(rel_delta),
         sd_rel_delta = sd(rel_delta),
         lower_bound = mean_rel_delta - 3 * sd_rel_delta,
         upper_bound = mean_rel_delta + 3 * sd_rel_delta) %>%
  filter(rel_delta >= lower_bound & rel_delta <= upper_bound) %>%
  ungroup()

test_missing <- table_delta_filtered%>%
  group_by(channel,cluster_2)%>%
  count()
# # excluding negative values
#table_delta_filtered_clean <- table_delta_filtered%>%
#  filter(rel_delta >= 0)

table_delta_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = rel_delta))+
  geom_boxplot()+
  geom_jitter()


table_delta_frontal_filtered <- table_delta_filtered%>%
  filter(table_delta_filtered$channel %in% frontal_channels)


test_missing <- table_delta_frontal_filtered%>%
  group_by(channel,cluster_2)%>%
  count()

df_corr_frontal_filtered <- table_delta_frontal_filtered%>%
  group_by(participant_id,group,tmt_a_time,facit_f_FS, tmt_b_minus_a,age,years_of_education,moca,cluster_2)%>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_beta_power = mean(rel_beta),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

df_corr_frontal_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power, color = cluster_2))+
  geom_boxplot()+
  geom_jitter()

# additional filtering (across group)
table_delta_filtered_cluster_2 <- table_delta_filtered%>%
  group_by(cluster_2)%>%
  mutate(mean_rel_delta = mean(rel_delta),
         sd_rel_delta = sd(rel_delta),
         lower_bound = mean_rel_delta - 3 * sd_rel_delta,
         upper_bound = mean_rel_delta + 3 * sd_rel_delta) %>%
  filter(rel_delta >= lower_bound & rel_delta <= upper_bound) %>%
  ungroup()

table_frontal_filtered_cluster_2 <- table_delta_filtered_cluster_2%>%
  filter(table_delta_filtered_cluster_2$channel %in% frontal_channels)

df_corr_frontal_filtered_cluster_2 <- table_frontal_filtered_cluster_2%>%
  group_by(participant_id,group,tmt_a_time,facit_f_FS, tmt_b_minus_a,age,moca,hads_d_total_score,cluster_2)%>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_delta_power = mean(rel_delta),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

df_corr_frontal_filtered_cluster_2%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power, color = cluster_2))+
  geom_boxplot()+
  geom_jitter()

df_corr_delta_filtered_cluster_2 <- table_delta_filtered_cluster_2%>%
  group_by(participant_id,group,tmt_a_time,facit_f_FS, tmt_b_minus_a,age, cluster_2)%>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_delta_power = mean(rel_delta),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

df_corr_delta_filtered_cluster_2%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power, color = cluster_2))+
  geom_boxplot()+
  geom_jitter()

t.test(mean_delta_power~cluster_2, data = df_corr_delta_filtered_cluster_2, alternative = 'two.sided')

# outliers delta absolute
table_delta_filtered_abs <-table_power_5%>%
  group_by(participant_id) %>%
  mutate(mean_abs_delta = mean(abs_delta),
         sd_abs_delta = sd(abs_delta),
         lower_bound = mean_abs_delta - 3 * sd_abs_delta,
         upper_bound = mean_abs_delta + 3 * sd_abs_delta) %>%
  filter(abs_delta >= lower_bound & abs_delta <= upper_bound) %>%
  ungroup()

table_delta_filtered_abs%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = abs_delta))+
  geom_boxplot()+
  geom_jitter()

table_delta_frontal_filtered_abs <- table_delta_filtered_abs%>%
  filter(table_delta_filtered_abs$channel %in% frontal_channels)

df_corr_frontal_filtered_abs <- table_delta_frontal_filtered_abs%>%
  group_by(participant_id,group,tmt_a_time,facit_f_FS, tmt_b_minus_a,age,years_of_education, cluster_2)%>%
  summarise(mean_delta_power_abs = mean(abs_delta))

df_corr_frontal_filtered_abs%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_delta_power_abs, color = cluster_2))+
  geom_boxplot()+
  geom_jitter()

t.test(mean_delta_power_abs~cluster_2, data = df_corr_frontal_filtered_abs, alternative = 'less')


# sd +- 3 for beta
table_beta_filtered <-table_power_5%>%
  group_by(participant_id) %>%
  mutate(mean_rel_beta = mean(rel_beta),
         sd_rel_beta = sd(rel_beta),
         lower_bound = mean_rel_beta - 3 * sd_rel_beta,
         upper_bound = mean_rel_beta + 3 * sd_rel_beta) %>%
  filter(rel_beta >= lower_bound & rel_beta <= upper_bound) %>%
  ungroup()

table_beta_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = rel_beta))+
  geom_boxplot()+
  geom_jitter()


table_central_filtered <- table_beta_filtered%>%
  filter(table_beta_filtered$channel %in% central_channels)

df_corr_central_filtered <- table_central_filtered%>%
  group_by(participant_id,group,tmt_a_time,facit_f_FS, tmt_b_minus_a,age, moca,hads_d_total_score,cluster_2)%>%
  summarise(mean_delta_power = mean(rel_delta),
            mean_beta_power = mean(rel_beta),
            mean_aperiodic_exponent = mean(aperiodic_exponent))

df_corr_central_filtered%>%
  group_by(cluster_2)%>%
  ggplot(aes(x = cluster_2, y = mean_beta_power, color = cluster_2))+
  geom_boxplot()+
  geom_jitter()

# additional filtering (across group)
table_beta_filtered_cluster_2 <- table_beta_filtered%>%
  group_by(cluster_2)%>%
  mutate(mean_rel_beta = mean(rel_beta),
         sd_rel_beta = sd(rel_beta),
         lower_bound = mean_rel_beta - 3 * sd_rel_beta,
         upper_bound = mean_rel_beta + 3 * sd_rel_beta) %>%
  filter(rel_beta >= lower_bound & rel_beta <= upper_bound) %>%
  ungroup()
