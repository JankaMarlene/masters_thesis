%% Script to visualize the power data (topoplots and power spectrum)
% overview
% 0. Preliminaries
% 1. topoplots
    % 1.1 power
    % 1.2 aperiodic components
    % 1.3 r squared
% 2. plot the whole power spectrum
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

% Define the output folder (already exists)
output_folder = 'plots\final\cluster2'; 

%% 1. topoplots
%% 1.1 power
% take a data vector and plot it
% for this to work you need to load in one dataset in order to have the
% structure of the layout
% load('C:\Users\jankj\OneDrive\Desktop\prep_power_5\sub-AN03NU_prep_p_5')

data_beta_c1 = importdata('data\analysis_power\cluster2\export_beta_c1.txt');
data_beta_c2 = importdata('data\analysis_power\cluster2\export_beta_c2.txt');
data_delta_c1 = importdata('data\analysis_power\cluster2\export_delta_c1.txt');
data_delta_c2 = importdata('data\analysis_power\cluster2\export_delta_c2.txt');

% you need to load one preprocessed data set for this in order to get
% EEG_epoched_5.chanlocs

%Beta C1
  fig =  figure;
topoplot(data_beta_c1.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_beta,'o','w',3,1})
hc=colorbar;
caxis([0.346 2.97])
xlabel(hc,'beta Power [μV^2]');
%title ('cluster 1 ');
set(findobj(gca,'type','patch'),'facecolor', '#F59541'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 17);
% Define the file name for saving the figure (as PNG)
file_name = fullfile(output_folder, 'beta_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location
exportgraphics(fig, file_name, 'Resolution', 300);

%Beta C2
 fig = figure;
topoplot(data_beta_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_beta,'o','w',3,1})
hc=colorbar;
caxis([0.346 2.97])
xlabel(hc,'beta Power [μV^2]');
%title ('ohne PCS ');
set(findobj(gca,'type','patch'),'facecolor', '#02CAF5'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

%Delta C1
fig = figure;
topoplot(data_delta_c1.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_delta,'o','w',3,1})
hc=colorbar;
caxis([0.172 1.92])
xlabel(hc,'delta Power [μV^2]');
%title ('with PCS ');
set(findobj(gca,'type','patch'),'facecolor', '#F59541'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

%Delta C2
fig = figure;
topoplot(data_delta_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2', {roi_delta,'o','w',3,1});
hc=colorbar;
caxis([0.172 1.92]);
xlabel(hc,'delta Power [μV^2]');
%title ('without PCS ');
set(findobj(gca,'type','patch'),'facecolor', '#02CAF5'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

%% 1.2 topoplots aperiodic components
data_ape_c1 = importdata('data\analysis_power\cluster2\export_ape_c1.txt');
data_ape_c2 = importdata('data\analysis_power\cluster2\export_ape_c2.txt');
data_apo_c1 = importdata('data\analysis_power\cluster2\export_apo_c1.txt');
data_apo_c2 = importdata('data\analysis_power\cluster2\export_apo_c2.txt');

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
file_name = fullfile(output_folder, 'Ape_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

figure;
topoplot(data_ape_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on','emarker2',{roi_sig,'o','w',3,1});
hc=colorbar;
caxis([0.605 1.16]);
xlabel(hc,'aperiodic exponent');
title ('cluster 2 ');
set(findobj(gca,'type','patch'),'facecolor', '#02CAF5'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

figure;
topoplot(data_apo_c1.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on');
hc=colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc,'aperiodic offset');
title ('cluster 1 ');
set(findobj(gca,'type','patch'),'facecolor', '#F59541'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

figure;
topoplot(data_apo_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on');
hc=colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc,'aperiodic offset');
title ('cluster 2 ');
set(findobj(gca,'type','patch'),'facecolor', '#02CAF5'); % Change [0.5, 0.5, 0.5] to your desired color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

%% 1.3 topoplot r squared
data_r_c1 = importdata('data\analysis_power\cluster2\export_r_c1.txt');
data_r_c2 = importdata('data\analysis_power\cluster2\export_r_c2.txt');

figure;
topoplot(data_r_c1.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on');
hc=colorbar;
caxis([0.833 0.959]);
xlabel(hc,'r squared');
title ('cluster 1 ');
file_name = fullfile(output_folder, 'r_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

figure;
topoplot(data_r_c2.data,EEG_epoched_5.chanlocs,'colormap',viridis,'electrodes','on');
hc=colorbar;
caxis([0.833 0.959]);
xlabel(hc,'R^2');
title ('cluster 2 ');
file_name = fullfile(output_folder, 'r_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location
%set(findobj(gca,'type','patch'),'facecolor', '#FFFFFF'); % Change [0.5, 0.5, 0.5] to your desired color

