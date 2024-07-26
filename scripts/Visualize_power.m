%% Script to visualize the power data (topoplots and power spectrum)
clear all
close all
clc

%% topoplots
% take a data vector and plot it
% this is EEGLAB
% initialize EEGLAB
proj_dir = fullfile(pwd);
indir = fullfile(proj_dir,'data\analysis_power');
eeglab;close;

roi_delta = [21,102,11,37,72,36,46,79,45,19,109,24,91,90,80,89,92,93,20,47,10,56,25]; % frontal ROI (channel indices are not always the number of the actual electrode!)
roi_beta = [82,31,62,34,87,63,1,65,3,64,2,67,71,73,78,31,34,39,83,40,84,41,85,42,86,43,74,5,75,6,7,76,8,77,68,32,69,33,70]; % put channel indices here

% for this to work you need to load in one dataset in order to have the
% structure of the layout

data_beta_c1 = importdata('data\analysis_power\export_beta_c1.txt');
data_beta_c2 = importdata('data\analysis_power\export_beta_c2.txt');
data_delta_c1 = importdata('data\analysis_power\export_delta_c1.txt');
data_delta_c2 = importdata('data\analysis_power\export_delta_c2.txt');

% Notes: - To change the plot map masking ring to a new figure background color,
%            >> set(findobj(gca,'type','patch'),'facecolor',get(gcf,'color'))


% you need to load one preprocessed data set for this in order to get
% EEG_epoched_5.chanlocs

    figure;
topoplot(data_beta_pcs.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on','emarker2', {roi_beta,'o','w',3,1})
hc=colorbar;
caxis([0.328 2.22])
xlabel(hc,'beta Power [μV^2]');
title ('mit PCS ');
%set(findobj(gca,'type','patch'),'facecolor', '#D94867'); % Change [0.5, 0.5, 0.5] to your desired color

 figure;
topoplot(data_beta_c.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on','emarker2', {roi_beta,'o','w',3,1})
hc=colorbar;
caxis([0.328 2.22])
xlabel(hc,'beta Power [μV^2]');
title ('ohne PCS ');
%set(findobj(gca,'type','patch'),'facecolor', '#D9B348'); % Change [0.5, 0.5, 0.5] to your desired color


figure;
topoplot(data_delta_pcs.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on','emarker2', {roi_delta,'o','w',3,1})
hc=colorbar;
caxis([0.292 1.65])
xlabel(hc,'delta Power [μV^2]');
title ('mit PCS ');
%set(findobj(gca,'type','patch'),'facecolor', '#9D5C99'); % Change [0.5, 0.5, 0.5] to your desired color


 figure;
topoplot(data_delta_c.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on','emarker2', {roi_delta,'o','w',3,1});
hc=colorbar;
caxis([0.292 1.65]);
xlabel(hc,'delta Power [μV^2]');
title ('ohne PCS ');
%set(findobj(gca,'type','patch'),'facecolor', '#D9B348'); % Change [0.5, 0.5, 0.5] to your desired color

% now for the aperiodic exponent
data_ape_pcs = importdata('data\analysis_power\export_ape_pcs.txt');
data_ape_c = importdata('data\analysis_power\export_ape_c.txt');
data_apo_pcs = importdata('data\analysis_power\export_apo_pcs.txt');
data_apo_c = importdata('data\analysis_power\export_apo_c.txt');

figure;
topoplot(data_ape_pcs.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on');
hc=colorbar;
caxis([0.605 1.15]);
xlabel(hc,'aperiodic exponent');
title ('mit PCS ');

figure;
topoplot(data_ape_c.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on');
hc=colorbar;
caxis([0.605 1.15]);
xlabel(hc,'aperiodic exponent');
title ('ohne PCS ');

figure;
topoplot(data_apo_pcs.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on');
hc=colorbar;
caxis([0 0.62]);
xlabel(hc,'aperiodic offset');
title ('mit PCS ');

figure;
topoplot(data_apo_c.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on');
hc=colorbar;
caxis([0 0.62]);
xlabel(hc,'aperiodic offset');
title ('ohne PCS ');

% plot the whole spectrum!
% for this I need the groups again
% load the age matched data
data_behav = readtable("C:\Users\Lara Godbersen\Documents\GitHub\Masters-thesis\data\PuG\matched_participants_conn.tsv", "FileType","text",'Delimiter', '\t');

% find out who is in which group
withPCS = table2array(data_behav(1:23,1));
withoutPCS = table2array(data_behav(24:46,1));

% trying to calculate a mean of the matrices
M_pcs = zeros(126,31);
M_c = zeros(126,31);
M_rest = zeros(126,31);

for i = 1:length(oscillatory)
    participant_id =  oscillatory{i}.id;
    
    if ismember(participant_id,withPCS) == 1
   M_pcs =  M_pcs + oscillatory{i}.powspctrm;
    elseif ismember(participant_id,withoutPCS) == 1
   M_c = M_c + oscillatory{i}.powspctrm;
    else
        M_rest = M_rest + oscillatory{i}.powspctrm;
    end
end

avg_power_pcs = M_pcs/23;
avg_power_c = M_c/23;

v = 58;
 hold on;
    plot((original{1,1}.freq),avg_power_pcs,'k');
    plot((original{1,1}.freq),avg_power_c,'b');
    plot((fractal{v,1}.freq), mean(fractal{v,1}.powspctrm),'b');
    plot((oscillatory{v,1}.freq), mean(oscillatory{v,1}.powspctrm),'r');

    plot((original{1,1}.freq),avg_power_c,'k');
    
% now average over ROIs!
roi_delta = [21,104,11,39,74,38,48,81,47,19,111,24,93,92,82,91,94,95,20,49,10,58,25]; % frontal ROI (channel indices are not always the number of the actual electrode!)
roi_beta = [84,33,64,36,89,65,1,67,3,66,2,69,73,75,80,33,36,41,85,42,86,43,87,44,88,45,76,5,77,6,7,78,8,79,70,34,71,35,72]; % put channel indices here

frontal_roi_c = mean(avg_power_c(roi_delta,:));
central_roi_c = mean(avg_power_c(roi_beta,:));

frontal_roi_pcs = mean(avg_power_pcs(roi_delta,:));
central_roi_pcs = mean(avg_power_pcs(roi_beta,:));

hold on;
    plot((original{1,1}.freq),frontal_roi_c,'k');
    plot((original{1,1}.freq),frontal_roi_pcs,'b');
    
hold on;
    plot((original{1,1}.freq),central_roi_c,'k');
    plot((original{1,1}.freq),central_roi_pcs,'b');
    
% plot the spectral parameterization as an example for the Thesis
v = 35;
 hold on;
    plot(log((original{v,1}.freq)),log(mean(original{v,1}.powspctrm)),'k');
    plot(log((fractal{v,1}.freq)), log(mean(fractal{v,1}.powspctrm)),'b');
    plot(log((oscillatory{v,1}.freq)), log(mean(oscillatory{v,1}.powspctrm)),'r');
    
% Example RGB color code (replace with your desired color)
rgbColor = '#BA4BF5'; % This is a teal color, for example
% % Compute the logarithm of the frequency and power spectrum
logFreq = log(original{v,1}.freq);
logPower = log(mean(original{v,1}.powspctrm));

% Plot the data with the desired modifications
plot(logFreq, logPower, 'Color', rgbColor, 'LineWidth', 2);
% Add axis labels
xlabel('log(frequency)');
ylabel('log(power)');
% Set the font size for the axis ticks
set(gca, 'FontSize', 14);

% Example RGB color code (replace with your desired color)
rgbColor = '#6547F5'; % This is a teal color, for example
logFreq = log(fractal{v,1}.freq);
logPower = log(mean(fractal{v,1}.powspctrm));
% Plot the data with the desired modifications
plot(logFreq, logPower, 'Color', rgbColor, 'LineWidth', 2);
% Add axis labels
xlabel('log(frequency)');
ylabel('log(power)');
% Set the font size for the axis ticks
set(gca, 'FontSize', 14);

% Example RGB color code (replace with your desired color)
rgbColor = '#F54B46'; % This is a teal color, for example
logFreq = log(oscillatory{v,1}.freq);
logPower = log(mean(oscillatory{v,1}.powspctrm));
% Plot the data with the desired modifications
plot(logFreq, logPower, 'Color', rgbColor, 'LineWidth', 2);
% Add axis labels
xlabel('log(frequency)');
ylabel('log(power)');
% Set the font size for the axis ticks
set(gca, 'FontSize', 14);

    
    
% plot the squared r
data_r_pcs = importdata('data\analysis_power\export_r_pcs.txt');
data_r_c = importdata('data\analysis_power\export_r_c.txt');


figure;
topoplot(data_r_pcs.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on');
hc=colorbar;
caxis([0.802 0.963]);
xlabel(hc,'r squared');
title ('mit PCS ');

figure;
topoplot(data_r_c.data,EEG_epoched_5.chanlocs,'colormap',cividis,'electrodes','on');
hc=colorbar;
caxis([0.802 0.963]);
xlabel(hc,'R^2');
set(findobj(gca,'type','patch'),'facecolor', '#FFFFFF'); % Change [0.5, 0.5, 0.5] to your desired color




%% permutation test (try)
for i = 1:length(oscillatory)
    participant_id =  oscillatory{i}.id;
    
    if ismember(participant_id,withPCS) == 1
   oscillatory{i}.group = 'withPCS';
    elseif ismember(participant_id,withoutPCS) == 1
   oscillatory{i}.group = 'withoutPCS'
    else
       oscillatory{i}.group = 'irrelevant';
    end
end


% Initialize empty arrays for P and W groups
    oscillatory_P = struct([]);
    oscillatory_W = struct([]);
% Loop through the structure array and separate based on group

for i = 1:length(oscillatory)
    if strcmp(oscillatory{i}.group, 'withPCS')
        oscillatory_P = [oscillatory_P, oscillatory{i}];
    elseif strcmp(oscillatory{i}.group, 'withoutPCS')
        oscillatory_W = [oscillatory_W, oscillatory{i}];
    end
end

% Initialize empty cell arrays for P and W groups
oscillatory_P = {};
oscillatory_W = {};

% Loop through the cell array and separate based on group
for i = 1:length(oscillatory)
    if strcmp(oscillatory{i}.group, 'withPCS')
        oscillatory_P = [oscillatory_P, oscillatory(i)];
    elseif strcmp(oscillatory{i}.group, 'withoutPCS')
        oscillatory_W = [oscillatory_W, oscillatory(i)];
    end
end

% layout
 elec = fullfile(proj_dir,'Src\BC-128-pass-lay.mat');  
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

cfg = [];
%cfg.latency          = 'all';
cfg.frequency        = [1 30];% get f.ex. delta here
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
cfg.numrandomization = 500;
% prepare_neighbours determines what sensors may form clusters
cfg.neighbours       = neigh;

design = zeros(1,length(oscillatory_W) + length(oscillatory_P));
design(1,1:length(oscillatory_W)) = 1;
design(1,(length(oscillatory_W)+1):(length(oscillatory_W)+length(oscillatory_P))) = 2;

cfg.design           = design;
cfg.ivar             = 1;

[stat] = ft_freqstatistics(cfg, oscillatory_W{:}, oscillatory_P{:});

cfg = [];
        cfg.layout = layout;
        cfg.parameter = 'stat';
        cfg.maskparameter = 'mask';

        ft_multiplotER(cfg,stat);
        