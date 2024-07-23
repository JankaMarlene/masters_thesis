%% Power Analysis Script for my Master's thesis
% 0. Preliminaries
% 1. Spectral Parameterization
% 2. some topoplots (just to look)
% 3. create table with all relevant values
% 4. adding group membership and other behavioral data
% 5. save combined table as .csv
% 6. fitting knee
% 7. create a big table with the resulst of the python MATLAB wrapper
% 8. add group membership & data_behav
% 9. save


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
data_prep = eeglab2fieldtrip(EEG_epoched_5, 'preprocessing');% muss ggf. auch epoched_4 oder epoched_5 geändert werden

% needs MATLABs Optimization Toolbox
% https://www.fieldtriptoolbox.org/example/fooof/

% compute the fractal and original spectra
    cfg               = [];
    cfg.foilim           = [0.5 30];% changed to 0.5 for better fit
    cfg.method        = 'mtmfft';
    cfg.taper         = 'hanning';
    cfg.pad           = 'nextpow2';
    cfg.output        = 'fooof_aperiodic'; % there is also fooof_peaks and just fooof
    fractal{s} = ft_freqanalysis(cfg, data_prep); % this contains info an the aperiodic component already -> I do not need fooof option additionally
    cfg.output        = 'pow';
    original{s} = ft_freqanalysis(cfg, data_prep);% contains the absolute power
    
    fractal{s}.id = extractBefore(indat(s).name,'_');
    
%     % plot fractal
%     figure
%     semilogy(fractal.freq, fractal.powspctrm(1,:), 'b-');
%     % plot original
%     figure
%     semilogy(original.freq, original.powspctrm(1,:), 'b-');

    % subtract the fractal component from the power spectrum
    cfg               = [];
    cfg.parameter     = 'powspctrm';
    cfg.operation     = 'x2-x1';
    oscillatory{s} = ft_math(cfg, fractal{s}, original{s});% contains the relative power
    
    oscillatory{s}.id = extractBefore(indat(s).name,'_');
%     %plot oscillatory
%     figure
%     semilogy(oscillatory.freq, oscillatory.powspctrm(1,:), 'b-');
end

% save oscillatory, original and fractal for PuG
save(fullfile(outdir,"power_results_final_5.mat"),"oscillatory", "original", "fractal");


   
    %% 2. Having a look at the topoplots
       
%     % prepare layout
%     elec = fullfile(proj_dir,'Src\BC-126-pass-lay.mat');  
%     cfg = [];
%     cfg.elec = elec;
%     layout = ft_prepare_layout(cfg);
%     
%     % plot relative power
%     cfg              = [];
%     cfg.xlim         = [16 24];    % we only plot the beta band
%     cfg.zlim         = 'maxabs';
%     cfg.marker       = 'on';
%     cfg.colorbar     = 'yes';
%     cfg.layout       = layout;
% 
%     figure;
%     ft_topoplotTFR(cfg, oscillatory);
%     title('beta band');
%     
%         % compare it to a topoplot of absolute power 
%     cfg              = [];
%     cfg.xlim         = [16 24]; % we only plot the beta band
%     cfg.zlim         = 'maxabs';
%     cfg.marker       = 'on';
%     cfg.colorbar     = 'yes';
%     cfg.layout       = layout;
% 
%     figure;
%     ft_topoplotTFR(cfg, original);
%     title('beta band');

%     % plot the individual spectra 
%     figure;
% 

% v = 21;
%  hold on;
%     plot((original{v,1}.freq),mean(original{v,1}.powspctrm),'k');
%     plot((fractal{v,1}.freq), mean(fractal{v,1}.powspctrm),'b');
%     plot((oscillatory{v,1}.freq), mean(oscillatory{v,1}.powspctrm),'r');

% % Plot the first subplot
% subplot(1, 7, 1);
% plot(oscillatory{55,1}.freq, oscillatory{55,1}.powspctrm,'k');
% plot(original{55,1}.freq, original{55,1}.powspctrm,'k');
% title('Plot 1');
% plot(fractal{55,1}.freq, fractal{55,1}.powspctrm,'k');
% title('Plot 1');
% % Plot the second subplot
% subplot(1, 7, 2);
% plot(oscillatory{2,1}.freq, oscillatory{2,1}.powspctrm,'k');
% title('Plot 2');
% % Plot the third subplot
% subplot(1, 7, 3);
% plot(oscillatory{3,1}.freq, oscillatory{3,1}.powspctrm,'k');
% title('Plot 3');
% % Plot the fourth subplot
% subplot(1, 7, 4);
% plot(oscillatory{4,1}.freq, oscillatory{4,1}.powspctrm,'k');
% title('Plot 4');
% % Plot the fifth subplot
% subplot(1, 7, 5);
% plot(oscillatory{5,1}.freq, oscillatory{5,1}.powspctrm,'k');
% title('Plot 5');
% % Plot the sixth subplot
% subplot(1, 7, 6);
% plot(oscillatory{6,1}.freq, oscillatory{6,1}.powspctrm,'k');
% title('Plot 6');
% % Plot the seventh subplot
% subplot(1, 7, 7);
% plot(oscillatory{7,1}.freq, oscillatory{7,1}.powspctrm,'k');
% title('Plot 7');

    %% 3. create the table with all the relevant values
    % f.ex. ID, channel name (label), aperiodic component (both offset and exponent), absolute delta,
    % relative delta, absolute beta, relative beta, group membership(later)
 
% Initialize an empty table
bigTable = table();

   for s = 1:length(oscillatory) % das hier über oscillatory machen
    % subject ID
    clear patient_id
    patient_id = oscillatory{s,1}.id; % und hier dann die ID aus fractal/oscillatorx
    
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
    cell_info = cell(126,11); 
    
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
       cell_info{row,col} = sum(original{s,1}.powspctrm(row,2:4),2);% relevant frequencies for absoulte delta power
   end
   for col = 6
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,2:4),2);% relevant frequencies for relative delta power
   end
   for col = 7
       cell_info{row,col} = sum(original{s,1}.powspctrm(row,15:30),2);% relevant frequencies for absoulte beta power
   end
   for col = 8
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,15:30),2);% relevant frequencies for relative beta power
   end 
   for col = 9
       cell_info{row,col} = r_squared{row};% first value in fooofparams.aperiodic_parameters
   end
   for col = 10
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,5:8),2);% relevant frequencies for relative theta power
   end
   for col = 11
       cell_info{row,col} = sum(oscillatory{s,1}.powspctrm(row,9:14),2);% relevant frequencies for relative alpha power
   end
end 

% create the table names
VarNames = ["participant_id" "channel" "aperiodic_offset" "aperiodic_exponent" "abs_delta" "rel_delta" "abs_beta" "rel_beta" "r_squared" "rel_theta" "rel_alpha"];

% gather data in a temporary table
currTable = table(cell_info(:,1),cell_info(:,2),cell_info(:,3),cell_info(:,4),cell_info(:,5),cell_info(:,6),cell_info(:,7),cell_info(:,8),cell_info(:,9), cell_info(:,10),cell_info(:,11), 'VariableNames',VarNames);
    
% Vertically concatenate the new table to the big table
bigTable = vertcat(bigTable, currTable);

   end

   %% 4. adding group membership and other behavioral data
data_behav = readtable("C:\Users\jankj\OneDrive\Desktop\masters_thesis\data\merged_data_all.tsv", "FileType","text",'Delimiter', '\t');


% combine the tables
bigTable_combined = outerjoin(bigTable, data_behav,'MergeKeys',true);


   %% 5. saving big table as .csv
% export table as csv -> be able to import it in R
csvFile = 'table_power_final_5.csv';
writetable(bigTable_combined, fullfile(outdir,csvFile));
