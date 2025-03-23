%% Script to visualize the power data (topoplots and power spectrum)
% overview
% 0. Preliminaries
% 1. topoplots
    % 1.1 power
    % 1.2 aperiodic components
    % 1.3 r squared
% 2. plot the whole power spectrum
% 3. permutation test
%% 0. Preliminaries

clear all
close all
clc

proj_dir = fullfile(pwd);
indir = fullfile(proj_dir,'data\analysis_power');
eeglab;close;% initialize EEGLAB, you will need it for the plots

% add source folder for functions, toolboxes, etc.
addpath(fullfile(pwd,"source"))

roi_delta = [21,102,11,37,72,36,46,79,45,19,109,24,91,90,80,89,92,93,20,47,10,56,25]; % frontal ROI (channel indices are not always the number of the actual electrode!)
roi_beta = [82,31,62,34,87,63,1,65,3,64,2,67,71,73,78,31,34,39,83,40,84,41,85,42,86,43,74,5,75,6,7,76,8,77,68,32,69,33,70]; % put channel indices here

%% 1. topoplots
%% 1.1 power
% take a data vector and plot it
% for this to work you need to load in one dataset in order to have the
% structure of the layout
% load('C:\Users\jankj\OneDrive\Desktop\prep_power_5\sub-AN03NU_prep_p_5')

data_beta_c1 = importdata('data\analysis_power\export_beta_c1.txt');
data_beta_c2 = importdata('data\analysis_power\export_beta_c2.txt');
data_delta_c1 = importdata('data\analysis_power\export_delta_c1.txt');
data_delta_c2 = importdata('data\analysis_power\export_delta_c2.txt');

% you need to load one preprocessed data set for this in order to get
% EEG_epoched_5.chanlocs

%Beta C1
  f =  figure;
topoplot(data_beta_c1.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_beta,'o','w',3,1})
hc=colorbar;
caxis([0.346 2.97])
xlabel(hc,'beta Power [μV^2]');
%title ('cluster 1 ');
set(findobj(gca,'type','patch'),'facecolor', '#F59541'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 17);
%save_fig(f,'C:\Users\jankj\OneDrive\Desktop\masters_thesis\plots\final\','beta_C1', 'fontsize',17, 'figsize', [0 0 10 6.5]);
saveas(f, 'C:\Users\jankj\OneDrive\Desktop\masters_thesis\plots\final\beta_C1.png');
exportgraphics(f, 'C:\Users\jankj\OneDrive\Desktop\masters_thesis\plots\final\beta_C1.png', 'Resolution', 300);

%Beta C2
 f = figure;
topoplot(data_beta_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_beta,'o','w',3,1})
hc=colorbar;
caxis([0.346 2.97])
xlabel(hc,'beta Power [μV^2]');
%title ('ohne PCS ');
set(findobj(gca,'type','patch'),'facecolor', '#02CAF5'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 17);
save_fig(f,'C:\Users\jankj\OneDrive\Desktop\masters_thesis\plots\final\','beta_c2', 'fontsize',17, 'figsize', [0 0 10 6.5]);

%Delta C1
f = figure;
topoplot(data_delta_c1.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_delta,'o','w',3,1})
hc=colorbar;
caxis([0.172 1.92])
xlabel(hc,'delta Power [μV^2]');
%title ('with PCS ');
set(findobj(gca,'type','patch'),'facecolor', '#F59541'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 17);
save_fig(f,'C:\Users\jankj\OneDrive\Desktop\masters_thesis\plots\final\','delta_c1', 'fontsize',17, 'figsize', [0 0 10 6.5]);

%Delta C2
f = figure;
topoplot(data_delta_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_delta,'o','w',3,1});
hc=colorbar;
caxis([0.172 1.92]);
xlabel(hc,'delta Power [μV^2]');
%title ('without PCS ');
set(findobj(gca,'type','patch'),'facecolor', '#02CAF5'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 17);
save_fig(f,'C:\Users\jankj\OneDrive\Desktop\masters_thesis\plots\final\','delta_c2', 'fontsize',17, 'figsize', [0 0 10 6.5]);

%% 1.2 topoplots aperiodic components
data_ape_c1 = importdata('data\analysis_power\export_ape_c1.txt');
data_ape_c2 = importdata('data\analysis_power\export_ape_c2.txt');
data_apo_c1 = importdata('data\analysis_power\export_apo_c1.txt');
data_apo_c2 = importdata('data\analysis_power\export_apo_c2.txt');

% significant channel (below 5 %): 68, 86
roi_sig = [65,83];
% unter 10 % 1,2,5,6,7,35,43,65,69,71,72,78,79,80,86
roi_nearly_sig = [1,2,5,6,7,32,40,62,66,68,69,75,76,77,83];

figure;
topoplot(data_ape_c1.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2',{roi_sig,'o','w',3,1});
hc=colorbar;
caxis([0.648 1.16]);
xlabel(hc,'aperiodic exponent');
title ('cluster 1 ');
set(findobj(gca,'type','patch'),'facecolor', '#F59541'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 14);

figure;
topoplot(data_ape_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2',{roi_sig,'o','w',3,1});
hc=colorbar;
caxis([0.605 1.16]);
xlabel(hc,'aperiodic exponent');
title ('cluster 2 ');
set(findobj(gca,'type','patch'),'facecolor', '#02CAF5'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 14);

figure;
topoplot(data_apo_c1.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on');
hc=colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc,'aperiodic offset');
title ('cluster 1 ');
set(findobj(gca,'type','patch'),'facecolor', '#F59541'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 16);

figure;
topoplot(data_apo_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on');
hc=colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc,'aperiodic offset');
title ('cluster 2 ');
set(findobj(gca,'type','patch'),'facecolor', '#02CAF5'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 16);

   
%% 1.3 topoplot r squared
data_r_c1 = importdata('data\analysis_power\export_r_c1.txt');
data_r_c2 = importdata('data\analysis_power\export_r_c2.txt');


figure;
topoplot(data_r_c1.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on');
hc=colorbar;
caxis([0.833 0.959]);
xlabel(hc,'r squared');
title ('cluster 1 ');

figure;
topoplot(data_r_c2.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on');
hc=colorbar;
caxis([0.833 0.959]);
xlabel(hc,'R^2');
title ('cluster 2 ');
%set(findobj(gca,'type','patch'),'facecolor', '#FFFFFF'); % Change [0.5, 0.5, 0.5] to your desired color

% load the age matched data
data_behav = readtable("C:\Users\jankj\OneDrive\Desktop\masters_thesis\data\merged_data_all.tsv", "FileType","text",'Delimiter', '\t');

%% 3. permutation test
% add group to every individual data set
for i = 1:length(oscillatory)
    participant_id = oscillatory{i}.id;
    if ismember(participant_id, c1)
        oscillatory{i}.group = 'c1';
    elseif ismember(participant_id, c2)
        oscillatory{i}.group = 'c2';
    else
        oscillatory{i}.group = 'irrelevant';
    end
end

% Initialize empty arrays for P and W groups
    oscillatory_c1 = struct([]);
    oscillatory_c2 = struct([]);
% Loop through the structure array and separate based on group
for i = 1:length(oscillatory)
    if strcmp(oscillatory{i}.group, 'c1')
        oscillatory_c1 = [oscillatory_c1, oscillatory(i)];
    elseif strcmp(oscillatory{i}.group, 'c2')
        oscillatory_c2 = [oscillatory_c2, oscillatory(i)];
    end
end

% load the layout
elec = fullfile(proj_dir,'source\BC-128-pass-lay.mat');  
cfg = [];
cfg.elec = elec;
layout = ft_prepare_layout(cfg);
    
% prepare neighbours
channels_to_exclude = {'31', '32'};
all_channels = layout.label; % Get all channel labels from the layout
channels_to_include = setdiff(all_channels, channels_to_exclude);

    
cfg = []; 
cfg.method = 'distance'; % how should the neighbors be selected?
cfg.channel = channels_to_include;
%cfg.neighbourdist = 3; % I have no Idea what range this has, just make sure, that you get meaningful neighbors
cfg.elec = elec; % Here we need the 3d-positions!
        
neigh = ft_prepare_neighbours(cfg); % between 5 and 10 neighbors is a good value, always good to check!

% now the actual permutation test with clusters
cfg = [];
%cfg.latency          = 'all';
cfg.frequency        = [0.6 4];% [0.5 4] for delta [14 30] for beta
cfg.avgoverfreq      = 'no';
cfg.method           = 'montecarlo';
cfg.statistic        = 'ft_statfun_indepsamplesT';
cfg.parameter        = 'powspctrm';
cfg.correctm         = 'cluster';
cfg.clusteralpha     = 0.05;
cfg.clusterstatistic = 'maxsum';
cfg.minnbchan        = 2;
cfg.tail             = 0;
cfg.clustertail      = 0;
cfg.alpha            = 0.05;
cfg.numrandomization = 1000;
% prepare_neighbours determines what sensors may form clusters
cfg.neighbours       = neigh;

design = zeros(1,length(oscillatory_c1) + length(oscillatory_c2));
design(1,1:length(oscillatory_c1)) = 1;
design(1,(length(oscillatory_c1)+1):(length(oscillatory_c1)+length(oscillatory_c2))) = 2;

cfg.design           = design;
cfg.ivar             = 1;

[stat_delta] = ft_freqstatistics(cfg, oscillatory_c2{:},oscillatory_c1{:});
        
% get the topography of stat
plot_stat_delta = mean(stat_delta.stat,2);

f = figure;
topoplot(plot_stat_delta,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_delta,'o','w',3,1});
hc=colorbar;
caxis([-1.3711 1.3711]);
xlabel(hc,'\it{t} - value');
%title ('without PCS ');
set(gca, 'FontSize', 15);
set(findobj(gca,'type','patch'),'facecolor', '#FFFFFF'); % Change [0.5, 0.5, 0.5] to your desired color
save_fig(f,'C:\Users\jankj\OneDrive\Desktop\masters_thesis\plots\final\','delta_perm', 'fontsize',17, 'figsize', [0 0 10 6.5]);


% now beta
% now the actual permutation test with clusters
cfg = [];
%cfg.latency          = 'all';
cfg.frequency        = [14 30];% [0.5 4] for delta [14 30] for beta
cfg.avgoverfreq      = 'no';
cfg.method           = 'montecarlo';
cfg.statistic        = 'ft_statfun_indepsamplesT';
cfg.parameter        = 'powspctrm';
cfg.correctm         = 'cluster';
cfg.clusteralpha     = 0.05;
cfg.clusterstatistic = 'maxsum';
cfg.minnbchan        = 2;
cfg.tail             = 0;
cfg.clustertail      = 0;
cfg.alpha            = 0.025;
cfg.numrandomization = 1000;
% prepare_neighbours determines what sensors may form clusters
cfg.neighbours       = neigh;

design = zeros(1,length(oscillatory_c1) + length(oscillatory_c2));
design(1,1:length(oscillatory_c1)) = 1;
design(1,(length(oscillatory_c1)+1):(length(oscillatory_c1)+length(oscillatory_c2))) = 2;

cfg.design           = design;
cfg.ivar             = 1;

[stat_beta] = ft_freqstatistics(cfg, oscillatory_c2{:},oscillatory_c1{:});

plot_stat_beta = mean(stat_beta.stat,2);
        
% get the topography of stat
f = figure;
topoplot(plot_stat_beta,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_beta,'o','w',3,1});
hc=colorbar;
caxis([-1.3711 1.3711]);
xlabel(hc,'\it{t} - value');
%title ('without PCS ');
set(gca, 'FontSize', 15);
set(findobj(gca,'type','patch'),'facecolor', '#FFFFFF'); % Change [0.5, 0.5, 0.5] to your desired color
save_fig(f,'C:\Users\jankj\OneDrive\Desktop\masters_thesis\plots\final\','beta_perm', 'fontsize',17, 'figsize', [0 0 10 6.5]);