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
eeglab; close; % initialize EEGLAB, you will need it for the plots

% add source folder for functions, toolboxes, etc.
addpath(fullfile(pwd,"source"))

% Define the ROIs for delta and beta power
roi_delta = [21,102,11,37,72,36,46,79,45,19,109,24,91,90,80,89,92,93,20,47,10,56,25]; % frontal ROI (channel indices are not always the number of the actual electrode!)
roi_beta = [82,31,62,34,87,63,1,65,3,64,2,67,71,73,78,31,34,39,83,40,84,41,85,42,86,43,74,5,75,6,7,76,8,77,68,32,69,33,70]; % central ROI

%% 1. topoplots
%% 1.1 power
% Load the data for cluster 4
data_beta_c1 = importdata('data\analysis_power\cluster4\export_beta_c1.txt');
data_beta_c2 = importdata('data\analysis_power\cluster4\export_beta_c2.txt');
data_beta_c3 = importdata('data\analysis_power\cluster4\export_beta_c3.txt');
data_beta_c4 = importdata('data\analysis_power\cluster4\export_beta_c4.txt');
data_delta_c1 = importdata('data\analysis_power\cluster4\export_delta_c1.txt');
data_delta_c2 = importdata('data\analysis_power\cluster4\export_delta_c2.txt');
data_delta_c3 = importdata('data\analysis_power\cluster4\export_delta_c3.txt');
data_delta_c4 = importdata('data\analysis_power\cluster4\export_delta_c4.txt');

% Define the output folder (already exists)
output_folder = 'plots\final\cluster4'; 

% Beta C1
fig = figure;
topoplot(data_beta_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1})
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); 
set(gca, 'FontSize', 17);
% Define the file name for saving the figure (as PNG)
file_name = fullfile(output_folder, 'beta_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Beta C2
fig = figure;
topoplot(data_beta_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5');
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Beta C3
fig = figure;
topoplot(data_beta_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5');
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_C3.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location


% Beta C4
fig = figure;
topoplot(data_beta_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#FF5F5F');
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_C4.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Delta C1
fig = figure;
topoplot(data_delta_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541');
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Delta C2
fig = figure;
topoplot(data_delta_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5');
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Delta C3
fig = figure;
topoplot(data_delta_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5');
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_C3.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Delta C4
fig = figure;
topoplot(data_delta_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#FF5F5F');
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_C4.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location
%% 1.2 topoplots aperiodic components

data_ape_c1 = importdata('data\analysis_power\cluster4\export_ape_c1.txt');
data_ape_c2 = importdata('data\analysis_power\cluster4\export_ape_c2.txt');
data_ape_c3 = importdata('data\analysis_power\cluster4\export_ape_c3.txt');
data_ape_c4 = importdata('data\analysis_power\cluster4\export_ape_c4.txt');
data_apo_c1 = importdata('data\analysis_power\cluster4\export_apo_c1.txt');
data_apo_c2 = importdata('data\analysis_power\cluster4\export_apo_c2.txt');
data_apo_c3 = importdata('data\analysis_power\cluster4\export_apo_c3.txt');
data_apo_c4 = importdata('data\analysis_power\cluster4\export_apo_c4.txt');

% significant channels (below 5 %): 68, 86
roi_sig = [65,83];
% under 10 %: 1, 2, 5, 6, 7, 35, 43, 65, 69, 71, 72, 78, 79, 80, 86
roi_nearly_sig = [1, 2, 5, 6, 7, 32, 40, 62, 66, 68, 69, 75, 76, 77, 83];

% Plot for Aperiodic Exponent - C1
figure;
topoplot(data_ape_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('cluster 1');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541');
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for Aperiodic Exponent - C2
figure;
topoplot(data_ape_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.605 1.16]);
xlabel(hc, 'aperiodic exponent');
title('cluster 2');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5');
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for Aperiodic Exponent - C3
figure;
topoplot(data_ape_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.605 1.16]);
xlabel(hc, 'aperiodic exponent');
title('cluster 3');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5');
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_C3.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for Aperiodic Exponent - C4
figure;
topoplot(data_ape_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.605 1.16]);
xlabel(hc, 'aperiodic exponent');
title('cluster 4');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#FF5F5F');
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_C4.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for Aperiodic Offset - C1
figure;
topoplot(data_apo_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('cluster 1');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541');
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for Aperiodic Offset - C2
figure;
topoplot(data_apo_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('cluster 2');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5');
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for Aperiodic Offset - C3
figure;
topoplot(data_apo_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('cluster 3');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5');
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_C3.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for Aperiodic Offset - C4
figure;
topoplot(data_apo_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('cluster 4');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#FF5F5F');
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_C4.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

%% 1.3 topoplot r squared
data_r_c1 = importdata('data\analysis_power\cluster4\export_r_c1.txt');
data_r_c2 = importdata('data\analysis_power\cluster4\export_r_c2.txt');
data_r_c3 = importdata('data\analysis_power\cluster4\export_r_c3.txt');
data_r_c4 = importdata('data\analysis_power\cluster4\export_r_c4.txt');

% Plot for R squared - C1
figure;
topoplot(data_r_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('cluster 1');
file_name = fullfile(output_folder, 'r_C1.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for R squared - C2
figure;
topoplot(data_r_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('cluster 2');
file_name = fullfile(output_folder, 'r_C2.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for R squared - C3
figure;
topoplot(data_r_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('cluster 3');
file_name = fullfile(output_folder, 'r_C3.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location

% Plot for R squared - C4
figure;
topoplot(data_r_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('cluster 4');
file_name = fullfile(output_folder, 'r_C4.png');
saveas(fig, file_name); % Save the figure in PNG format at the specified location
