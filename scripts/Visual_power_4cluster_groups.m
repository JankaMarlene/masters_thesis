%% Script to visualize the power data (topoplots and power spectrum)
%% The original code was sourced from the GitHub repository LGodbersen/Masters-thesis and subsequently modified to accommodate my dataset. (https://github.com/LGodbersen/Masters-thesis)
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
indir = fullfile(proj_dir, 'data\analysis_power');
eeglab; close; % initialize EEGLAB, you will need it for the plots

% add source folder for functions, toolboxes, etc.
addpath(fullfile(pwd, "source"))

roi_delta = [21, 102, 11, 37, 72, 36, 46, 79, 45, 19, 109, 24, 91, 90, 80, 89, 92, 93, 20, 47, 10, 56, 25]; % frontal ROI (channel indices are not always the number of the actual electrode!)
roi_beta = [82, 31, 62, 34, 87, 63, 1, 65, 3, 64, 2, 67, 71, 73, 78, 31, 34, 39, 83, 40, 84, 41, 85, 42, 86, 43, 74, 5, 75, 6, 7, 76, 8, 77, 68, 32, 69, 33, 70]; % put channel indices here

% Define the output folder (already exists)
output_folder = 'plots\final\cluster4_groups'; 

%% 1. topoplots
%% 1.1 power
% take a data vector and plot it

% Import the group-specific data for cluster4_groups
data_beta_selfCD_c1 = importdata('data\analysis_power\cluster4_groups\export_beta_selfCD_c1.txt');
data_beta_noCD_c1 = importdata('data\analysis_power\cluster4_groups\export_beta_noCD_c1.txt');
data_beta_selfCD_c2 = importdata('data\analysis_power\cluster4_groups\export_beta_selfCD_c2.txt');
data_beta_noCD_c2 = importdata('data\analysis_power\cluster4_groups\export_beta_noCD_c2.txt');
data_beta_selfCD_c3 = importdata('data\analysis_power\cluster4_groups\export_beta_selfCD_c3.txt');
data_beta_noCD_c3 = importdata('data\analysis_power\cluster4_groups\export_beta_noCD_c3.txt');
data_beta_selfCD_c4 = importdata('data\analysis_power\cluster4_groups\export_beta_selfCD_c4.txt');
data_beta_noCD_c4 = importdata('data\analysis_power\cluster4_groups\export_beta_noCD_c4.txt');

% Import delta power data
data_delta_selfCD_c1 = importdata('data\analysis_power\cluster4_groups\export_delta_selfCD_c1.txt');
data_delta_noCD_c1 = importdata('data\analysis_power\cluster4_groups\export_delta_noCD_c1.txt');
data_delta_selfCD_c2 = importdata('data\analysis_power\cluster4_groups\export_delta_selfCD_c2.txt');
data_delta_noCD_c2 = importdata('data\analysis_power\cluster4_groups\export_delta_noCD_c2.txt');
data_delta_selfCD_c3 = importdata('data\analysis_power\cluster4_groups\export_delta_selfCD_c3.txt');
data_delta_noCD_c3 = importdata('data\analysis_power\cluster4_groups\export_delta_noCD_c3.txt');
data_delta_selfCD_c4 = importdata('data\analysis_power\cluster4_groups\export_delta_selfCD_c4.txt');
data_delta_noCD_c4 = importdata('data\analysis_power\cluster4_groups\export_delta_noCD_c4.txt');

%% --- Beta SelfCD Cluster 1
fig = figure;
topoplot(data_beta_selfCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Color for SelfCD Cluster 1
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_selfCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- Beta NoCD Cluster 1
fig = figure;
topoplot(data_beta_noCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Color for NoCD Cluster 1
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_noCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- Beta SelfCD Cluster 2
fig = figure;
topoplot(data_beta_selfCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Color for SelfCD Cluster 2
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_selfCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- Beta NoCD Cluster 2
fig = figure;
topoplot(data_beta_noCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Color for NoCD Cluster 2
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_noCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- Beta SelfCD Cluster 3
fig = figure;
topoplot(data_beta_selfCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Color for SelfCD Cluster 3
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_selfCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- Beta NoCD Cluster 3
fig = figure;
topoplot(data_beta_noCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Color for NoCD Cluster 3
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_noCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- Beta SelfCD Cluster 4
fig = figure;
topoplot(data_beta_selfCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Color for SelfCD Cluster 4
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_selfCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% --- Beta NoCD Cluster 4
fig = figure;
topoplot(data_beta_noCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_beta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.346 2.97]);
xlabel(hc, 'beta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Color for NoCD Cluster 4
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'beta_noCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% --- Delta SelfCD Cluster 1
fig = figure;
topoplot(data_delta_selfCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Color for SelfCD Cluster 1
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_selfCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- Delta NoCD Cluster 1
fig = figure;
topoplot(data_delta_noCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Color for NoCD Cluster 1
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_noCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- Delta SelfCD Cluster 2
fig = figure;
topoplot(data_delta_selfCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Color for SelfCD Cluster 2
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_selfCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- Delta NoCD Cluster 2
fig = figure;
topoplot(data_delta_noCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Color for NoCD Cluster 2
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_noCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- Delta SelfCD Cluster 3
fig = figure;
topoplot(data_delta_selfCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Color for SelfCD Cluster 3
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_selfCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- Delta NoCD Cluster 3
fig = figure;
topoplot(data_delta_noCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Color for NoCD Cluster 3
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_noCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- Delta SelfCD Cluster 4
fig = figure;
topoplot(data_delta_selfCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Color for SelfCD Cluster 4
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_selfCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% --- Delta NoCD Cluster 4
fig = figure;
topoplot(data_delta_noCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_delta, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.172 1.92]);
xlabel(hc, 'delta Power [μV^2]');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Color for NoCD Cluster 4
set(gca, 'FontSize', 17);
file_name = fullfile(output_folder, 'delta_noCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% Aperiodic Exponent (Ape) and Aperiodic Offset (Apo) for each cluster and group

% Import the Ape and Apo data for both clusters and groups (similar to beta and delta import)
data_ape_selfCD_c1 = importdata('data\analysis_power\cluster4_groups\export_ape_selfCD_c1.txt');
data_ape_noCD_c1 = importdata('data\analysis_power\cluster4_groups\export_ape_noCD_c1.txt');
data_ape_selfCD_c2 = importdata('data\analysis_power\cluster4_groups\export_ape_selfCD_c2.txt');
data_ape_noCD_c2 = importdata('data\analysis_power\cluster4_groups\export_ape_noCD_c2.txt');
data_ape_selfCD_c3 = importdata('data\analysis_power\cluster4_groups\export_ape_selfCD_c3.txt');
data_ape_noCD_c3 = importdata('data\analysis_power\cluster4_groups\export_ape_noCD_c3.txt');
data_ape_selfCD_c4 = importdata('data\analysis_power\cluster4_groups\export_ape_selfCD_c4.txt');
data_ape_noCD_c4 = importdata('data\analysis_power\cluster4_groups\export_ape_noCD_c4.txt');

data_apo_selfCD_c1 = importdata('data\analysis_power\cluster4_groups\export_apo_selfCD_c1.txt');
data_apo_noCD_c1 = importdata('data\analysis_power\cluster4_groups\export_apo_noCD_c1.txt');
data_apo_selfCD_c2 = importdata('data\analysis_power\cluster4_groups\export_apo_selfCD_c2.txt');
data_apo_noCD_c2 = importdata('data\analysis_power\cluster4_groups\export_apo_noCD_c2.txt');
data_apo_selfCD_c3 = importdata('data\analysis_power\cluster4_groups\export_apo_selfCD_c3.txt');
data_apo_noCD_c3 = importdata('data\analysis_power\cluster4_groups\export_apo_noCD_c3.txt');
data_apo_selfCD_c4 = importdata('data\analysis_power\cluster4_groups\export_apo_selfCD_c4.txt');
data_apo_noCD_c4 = importdata('data\analysis_power\cluster4_groups\export_apo_noCD_c4.txt');

% significant channel (below 5 %): 68, 86
roi_sig = [65,83];
% unter 10 % 1,2,5,6,7,35,43,65,69,71,72,78,79,80,86
roi_nearly_sig = [1,2,5,6,7,32,40,62,66,68,69,75,76,77,83];

%% --- Aperiodic Exponent SelfCD Cluster 1
fig = figure;
topoplot(data_ape_selfCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('self-reported CD cluster 1');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_selfCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Exponent NoCD Cluster 1
fig = figure;
topoplot(data_ape_noCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('no self-reported CD cluster 1');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Set the color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_noCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Exponent SelfCD Cluster 2
fig = figure;
topoplot(data_ape_selfCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('self-reported CD cluster 2');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_selfCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Exponent NoCD Cluster 2
fig = figure;
topoplot(data_ape_noCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('no self-reported CD cluster 2');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Set the color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_noCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Exponent SelfCD Cluster 3
fig = figure;
topoplot(data_ape_selfCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('self-reported CD cluster 3');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_selfCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Exponent NoCD Cluster 3
fig = figure;
topoplot(data_ape_noCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('no self-reported CD cluster 3');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Set the color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_noCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Exponent SelfCD Cluster 4
fig = figure;
topoplot(data_ape_selfCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('self-reported CD cluster 4');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_selfCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Exponent NoCD Cluster 4
fig = figure;
topoplot(data_ape_noCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on', 'emarker2', {roi_sig, 'o', 'w', 3, 1});
hc = colorbar;
caxis([0.648 1.16]);
xlabel(hc, 'aperiodic exponent');
title('no self-reported CD cluster 4');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Set the color
set(gca, 'FontSize', 14);
file_name = fullfile(output_folder, 'Ape_noCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Offset SelfCD Cluster 1
fig = figure;
topoplot(data_apo_selfCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('self-reported CD cluster 1');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_selfCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Offset NoCD Cluster 1
fig = figure;
topoplot(data_apo_noCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('no self-reported CD cluster 1');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_noCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Offset SelfCD Cluster 2
fig = figure;
topoplot(data_apo_selfCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('self-reported CD cluster 2');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_selfCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Offset NoCD Cluster 2
fig = figure;
topoplot(data_apo_noCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('no self-reported CD cluster 2');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_noCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Offset SelfCD Cluster 3
fig = figure;
topoplot(data_apo_selfCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('self-reported CD cluster 3');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_selfCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Offset NoCD Cluster 3
fig = figure;
topoplot(data_apo_noCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('no self-reported CD cluster 3');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_noCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Offset SelfCD Cluster 4
fig = figure;
topoplot(data_apo_selfCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('self-reported CD cluster 4');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_selfCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% --- Aperiodic Offset NoCD Cluster 4
fig = figure;
topoplot(data_apo_noCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([-0.630 -0.0323]);
xlabel(hc, 'aperiodic offset');
title('no self-reported CD cluster 4');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'Apo_noCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% 1.3 topoplot r squared
data_r_selfCD_c1 = importdata('data\analysis_power\cluster4_groups\export_r_selfCD_c1.txt');
data_r_noCD_c1 = importdata('data\analysis_power\cluster4_groups\export_r_noCD_c1.txt');
data_r_selfCD_c2 = importdata('data\analysis_power\cluster4_groups\export_r_selfCD_c2.txt');
data_r_noCD_c2 = importdata('data\analysis_power\cluster4_groups\export_r_noCD_c2.txt');
data_r_selfCD_c3 = importdata('data\analysis_power\cluster4_groups\export_r_selfCD_c3.txt');
data_r_noCD_c3 = importdata('data\analysis_power\cluster4_groups\export_r_noCD_c3.txt');
data_r_selfCD_c4 = importdata('data\analysis_power\cluster4_groups\export_r_selfCD_c4.txt');
data_r_noCD_c4 = importdata('data\analysis_power\cluster4_groups\export_r_noCD_c4.txt');

%% --- R Squared SelfCD Cluster 1
fig = figure;
topoplot(data_r_selfCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('self-reported CD cluster 1');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'r_selfCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- R Squared NoCD Cluster 1
fig = figure;
topoplot(data_r_noCD_c1.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('no self-reported CD cluster 1');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'r_noCD_C1.png');
saveas(fig, file_name); % Save as PNG

%% --- R Squared SelfCD Cluster 2
fig = figure;
topoplot(data_r_selfCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('self-reported CD cluster 2');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'r_selfCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- R Squared NoCD Cluster 2
fig = figure;
topoplot(data_r_noCD_c2.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('no self-reported CD cluster 2');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'r_noCD_C2.png');
saveas(fig, file_name); % Save as PNG

%% --- R Squared SelfCD Cluster 3
fig = figure;
topoplot(data_r_selfCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('self-reported CD cluster 3');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'r_selfCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- R Squared NoCD Cluster 3
fig = figure;
topoplot(data_r_noCD_c3.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('no self-reported CD cluster 3');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#AA42F5'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'r_noCD_C3.png');
saveas(fig, file_name); % Save as PNG

%% --- R Squared SelfCD Cluster 4
fig = figure;
topoplot(data_r_selfCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('self-reported CD cluster 4');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#F59541'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'r_selfCD_C4.png');
saveas(fig, file_name); % Save as PNG

%% --- R Squared NoCD Cluster 4
fig = figure;
topoplot(data_r_noCD_c4.data, EEG_epoched_5.chanlocs, 'colormap', viridis, 'electrodes', 'on');
hc = colorbar;
caxis([0.833 0.959]);
xlabel(hc, 'r squared');
title('no self-reported CD cluster 4');
set(findobj(gca, 'type', 'patch'), 'facecolor', '#02CAF5'); % Set the color
set(gca, 'FontSize', 16);
file_name = fullfile(output_folder, 'r_noCD_C4.png');
saveas(fig, file_name); % Save as PNG
