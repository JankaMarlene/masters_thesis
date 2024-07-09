%% 1. Script to read in and do some general preprocessing to the data
% 0. Preliminaries
% 1. Define the current dataset
    % 1.1. Read in Markers and define trials
    % 1.2. Save the trial-definition
    % 1.3. Then define the entire data set
% 2. Read in the continuous data and apply filters (Preprocessing)
    % 2.1. High pass filter
    % 2.2. Resampling
    % 2.3 Low pass filter
% 3. Safe the data for further processing

%% 0.Preliminaries
clear
close all
clc

% initialize fieldtrip
addpath('C:\Users\jankj\OneDrive\Desktop\masters_thesis\scripts\fieldtrip-20240504\fieldtrip-20240504'); %initialize FieldTrip
%addpath('C:\Users\User\Documents\MATLAB\toolboxes\fieldtrip-20230503') % Julius FT
ft_defaults;

%% 1. Define the current dataset

proj_dir = fullfile(pwd); % automatically get path of script location, and parent dir
addpath(genpath(proj_dir)); % add dir to project to Matlab path
% indir = fullfile(proj_dir,'data\raw');% path to folder with BIDS datasets
indir = fullfile('W:\bids_projects\epoc_cn');% path to folder with BIDS datasets on Server
outdir = fullfile(proj_dir,'data\prep'); % path to prep ft data
indat = dir(indir); % content of that folder
indat = indat(startsWith({indat.name}, 'sub-')); % only keep folders that start with 'sub-' (i.e. the subjects)

