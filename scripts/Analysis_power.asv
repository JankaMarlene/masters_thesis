%% Power Analysis Script for my Master's thesis
% 0. Preliminaries
% 1. Spectral Parameterization
% 2. create table with all relevant values
% 3. adding group membership and other behavioral data
% 4. save combined table as .csv

%% 0.Preliminaries
clear
close all
clc

% initialize fieldtrip
ft_defaults;
% initialize EEGLAB
eeglab;close;
% add source folder for functions, toolboxes, etc.
addpath(fullfile(pwd,"source"))
% needs MATLABs Optimization Toolbox as well! (downloaded as add on)

% set paths
proj_dir = fullfile(pwd); % automatically get path of script location, and parent dir
indir = fullfile(proj_dir,'data\prep_power_5');% path to folder with BIDS datasets
outdir = fullfile(proj_dir,'data\analysis_power'); % I'll change that later (after preparing poster)
indat = dir(indir); % content of that folder
indat = indat(startsWith({indat.name}, 'sub-')); % only keep folders that start with 'sub-' (i.e. the subjects)

fractal = cell(length(indat),1);
original = cell(length(indat),1);
oscillatory = cell(length(indat),1);


for s = 1:length(indat) % loop over all subjects   
%% 1. Spectral Parameterization
% is originally Python Code, but was implemented in the Brainstorm Toolbox
% but not in EEGLAB -> convert back to FieldTrip
load(fullfile(indir,indat(s).name));
data_prep = eeglab2fieldtrip(EEG_epoched_5, 'raw');% the 'raw' specification seems to be important

% https://www.fieldtriptoolbox.org/example/fooof/
% compute the fractal and original spectra
    cfg               = [];
    cfg.foi        = 0.3 :0.2: 30;% changed to 0.5 for better fit
    cfg.method        = 'mtmfft';
    cfg.taper         = 'hanning';
    cfg.pad           = 'nextpow2';
    cfg.output        = 'fooof_aperiodic'; % there is also fooof_peaks and just fooof
    fractal{s} = ft_freqanalysis(cfg, data_prep); % this contains info on the aperiodic component already -> I do not need fooof option additionally
    cfg.output        = 'pow';
    original{s} = ft_freqanalysis(cfg, data_prep);% contains the absolute power
    
    fractal{s}.id = extractBefore(indat(s).name,'_');% attach ID

    % subtract the fractal component from the power spectrum
    cfg               = [];
    cfg.parameter     = 'powspctrm';
    cfg.operation     = 'x2-x1';
    oscillatory{s} = ft_math(cfg, fractal{s}, original{s});% contains the relative power
    
    oscillatory{s}.id = extractBefore(indat(s).name,'_');% attach ID
end

% save oscillatory, original and fractal for Visualize_power (Script)
save(fullfile(outdir,"power_results_final_5.mat"),"oscillatory", "original", "fractal");

    %% 2. create the table with all the relevant values
    % f.ex. ID, channel name (label), aperiodic component (both offset and exponent), absolute delta,
    % relative delta, absolute beta, relative beta, group membership(later)
 
% Initialize an empty table
bigTable = table();

   for s = 1:length(oscillatory)
    % subject ID
    clear patient_id
    patient_id = oscillatory{s,1}.id;
    
     % aperiodic component (needs to be computed before the loop ->
     % separate the two values)
    clear aperiodic
    aperiodic = cell(126,1);
    for row = 1:126
        aperiodic{row,1} = fractal{s,1}.fooofparams(row).aperiodic_params;
    end
    
    aperiodic_offset = cell(126,1);
    aperiodic_exponent = cell(126,1);
    
    for row = 1:126
        aperiodic_offset{row,1} = aperiodic{row}(1);
    end
    
    for row = 1:126
        aperiodic_exponent{row,1} = aperiodic{row}(2);
    end
    
    % getting the r2 as a measure for fooof fit into the table
    for row = 1:126
        r_squared{row,1} = fractal{s,1}.fooofparams(row).r_squared;
    end
 
    % create empty cell
    cell_info = cell(126,13); 
    
for row = 1:126
   for col = 1
      cell_info{row,col} = patient_id;% VPCode
   end 
   for col = 2
       cell_info{row,col} = oscillatory{s,1}.label(row);% channel labels
   end
   for col = 3
       cell_info{row,col} = aperiodic_offset{row};% first value in fooofparams.aperiodic_parameters
   end
   for col = 4
       cell_info{row,col} = aperiodic_exponent{row};% second value in fooofparams.aperiodic_parameters
   end
   for col = 5
       cell_info{row,col} = sum(original{s,1}.powspctrm(row,4:19),2);% relevant frequencies for absoulte delta power
   end
   for col = 6
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,4:19),2);% relevant frequencies for relative delta power
   end
   for col = 7
       cell_info{row,col} = sum(original{s,1}.powspctrm(row,70:149),2);% relevant frequencies for absoulte beta power
   end
   for col = 8
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,70:149),2);% relevant frequencies for relative beta power
   end 
   for col = 9
       cell_info{row,col} = r_squared{row};% first value in fooofparams.aperiodic_parameters
   end
   for col = 10
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,20:39),2);% relevant frequencies for relative theta power
   end
   for col = 11
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,40:69),2);% relevant frequencies for relative alpha power
   end
   for col = 12
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,70:99),2);% relevant frequencies for relative low beta power
   end
   for col = 13
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,100:149),2);% relevant frequencies for relative high beta power
   end
end

% create the table names
VarNames = ["participant_id" "channel" "aperiodic_offset" "aperiodic_exponent" "abs_delta" "rel_delta" "abs_beta" "rel_beta" "r_squared" "rel_theta" "rel_alpha" "rel_beta1" "rel_beta2"];


% gather data in a temporary table
currTable = table(cell_info(:,1),cell_info(:,2),cell_info(:,3),cell_info(:,4),cell_info(:,5),cell_info(:,6),cell_info(:,7),cell_info(:,8),cell_info(:,9), cell_info(:,10),cell_info(:,11),cell_info(:,12),cell_info(:,13), 'VariableNames',VarNames);
    
% Vertically concatenate the new table to the big table
bigTable = vertcat(bigTable, currTable);

   end

   %% 3. adding group membership and other behavioral data
data_behav = readtable("C:\Users\jankj\OneDrive\Desktop\masters_thesis\data\merged_data_all.tsv", "FileType","text",'Delimiter', '\t');


% combine the tables
bigTable_combined = outerjoin(bigTable, data_behav,'MergeKeys',true);

   %% 4. saving big table as .csv
% export table as csv -> be able to import it in R
csvFile = 'table_power_final_5.csv';
writetable(bigTable_combined, fullfile(outdir,csvFile));
