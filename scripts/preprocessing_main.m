%% 2. Main Preprocessing Script
%% The original code was sourced from the GitHub repository LGodbersen/Masters-thesis and subsequently modified to accommodate my dataset. (https://github.com/LGodbersen/Masters-thesis)
% 0. Preliminaries
% 1. read in data and convert into EEGLAB structure
% 2. exclude big artifacts 
% 3. Common average reference
% 4. ICA 
% 5. ICLabel (automatic component rejection)
% 6. Additional artifact removal
% 7. Interpolate bad channels: Repair the Cleaned and ICA-Corrected Data
% 8. Re-Reference 
% 9. Extract epochs 
% 10. save data in prep_power
% 13. find out how many epochs survived

%% 0.Preliminaries
clear
close all
clc

% add source folder for functions, toolboxes, etc.
addpath(fullfile(pwd,"source"))
% initialize fieldtrip
ft_defaults;
% initialize eeglab
addpath('C:\Users\jankj\OneDrive\Desktop\eeglab2024.0')
eeglab; close; % add paths to EEGLAB (was in the loop before -> inefficient)

% set paths
proj_dir = fullfile(pwd); % automatically get path of script location, and parent dir
indir = fullfile(proj_dir,'data\prep');% path to folder with BIDS datasets
outdir = fullfile(proj_dir,'data\icaweights'); % path to prep ft data
indat = dir(indir); % content of that folder
indat = indat(startsWith({indat.name}, 'sub-')); % only keep folders that start with 'sub-' (i.e. the subjects)

%% run loop over participants
for s = 53:length(indat) % loop over all subjects

    %% 1. read in data and convert into EEGLAB structure
    load(fullfile(indir,indat(s).name)); % load read in data sets
    
    tmp_id = extractBefore(indat(s).name,'_'); % get the participant ID
    data_clean.label = extractAfter(data_clean.label,'_'); % remove letters from the channel names so that they match the .loc file
    
    % convert to EEGLAB structure and prepare Layout
    EEG = fieldtrip2eeglab(data_clean);
    EEG.srate = 250;% it had 1000 still in the hdr
    EEG = pop_chanedit(EEG, 'load',{fullfile(proj_dir,'source\BC-128-pass-lay.loc'),'filetype','autodetect'});
    EEG = eeg_checkset(EEG);
    
    %% 2. Clean data ( exclude big artifacts)
    EEG_clean = pop_clean_rawdata(EEG, 'FlatlineCriterion',5,'ChannelCriterion',0.85,'Highpass','off','BurstCriterion',100,'WindowCriterion',0.4,'BurstRejection','on','Distance','Euclidian','WindowCriterionTolerances',[-Inf 7] );
    %plot histogram to understand the dirstribution of the values
    %histogram(EEG.data)
    %histogram(EEG_clean.data)
   
    %% 3. apply CAR
    % After cleaning the data, it is best to re-reference the data to
    % the average across channels to remove the influence of the
    % reference
    
    EEG_ref = pop_reref(EEG_clean, []);
     
     %% 4. ICA EEGLAB
     EEG_ica = pop_runica(EEG_ref, 'icatype', 'runica', 'extended',1,'interrupt','on','pca',EEG_ref.nbchan-1);
     outdir = fullfile(proj_dir,'data\icaweights'); % path to prep ft data
     save(fullfile(outdir,[tmp_id + "_comp_EEGLAB.mat"]),"EEG_ica"); % save icaweights in case you change something later in the pipeline
     
     %% 5. ICLABEL (automatic component rejection)
     EEG_ica_label = pop_iclabel(EEG_ica, 'default');
     EEG_ica_comp = pop_icflag(EEG_ica_label, [0 0;0.8 1; 0.5 1; 0 0; 0 0; 0 0; 0 0]); % see function help message
     rejected_comps = find(EEG_ica_comp.reject.gcompreject > 0);
     EEG_ica_comp = pop_subcomp(EEG_ica_comp, rejected_comps);
     EEG_ica_comp  = pop_select(EEG_ica_comp, 'rmchannel',{'31', '32'}); % remove the two EOG channels '31' and '32'
     EEG_ica_clean = eeg_checkset(EEG_ica_comp);
     
    %% 6. Additional artifact removal
    std_check = std(EEG_ica_clean.data, 0, 2);
    std_hist = mean(std_check);
    mean_check = mean(EEG_ica_clean.data,2);
    mean_hist = mean(mean_check);
    % create a threshold from the data
    threshold_max = mean_hist + 2.5*std_hist;
    threshold_min = mean_hist - 2.5*std_hist; 
    % Find channels that exceed the threshold
    channels_to_reject_max = find(std_check > threshold_max);
    channels_to_reject_min = find(std_check < threshold_min);
    
    % Reject the identified channels
    EEG_chan_clean = pop_select(EEG_ica_clean, 'rmchannel', channels_to_reject_max);
    EEG_chan_clean = pop_select(EEG_chan_clean, 'rmchannel', channels_to_reject_min);
    
    %% 7. Interpolate bad channels
    % found an approach here: https://gist.github.com/disbeat/6c484ca61eafd64f071a8c80a36a9211  
    % get a list of existent chanloc names in the EEG structure
    chans_eeg = [];
    for i=1:length(EEG_chan_clean.chanlocs)
        chans_eeg = [ chans_eeg {EEG_chan_clean.chanlocs(i).labels} ];
    end
    % make a list of indexes from the provided chanlocs that are not in the EEG structure
    idxs = [];
    chanlocs = EEG.chanlocs;
    clear i
    for i=1:length(chanlocs)
        if isempty(find(ismember(chans_eeg, chanlocs(i).labels) == 1, 1))
            idxs = [idxs i];
        end
    end
    
% remove EOG channels
remove_EOG = [30, 31]; % channel 31 and 32 are in the place 30 and 31 because nr. 20 is missing

% Using logical indexing
idxs = idxs(~ismember(idxs,remove_EOG));

    % call EEGLAB pop_interp method
    EEG_interp = pop_interp(EEG_chan_clean, chanlocs(idxs));

    % get current EEG chanlocs names
    chans_eeg = cell(1, length(EEG_interp.chanlocs));
    for c=1:length(EEG_interp.chanlocs)
        chans_eeg{c} = EEG_interp.chanlocs(c).labels;
    end

    % remove the EOG channels from chanlocs also
    % Labels to remove
labels_to_remove = {'31', '32'}; 

% Find indices of labels to remove
indices_to_remove = ismember({chanlocs.labels}, labels_to_remove);

% Remove rows with specified labels
chanlocs(indices_to_remove) = [];

    % find order idxs
    idxs = nan(1, length(chanlocs));
    for c=1:length(chanlocs)
        idxs(c) = find(ismember(chans_eeg, chanlocs(c).labels) == 1, 1);
    end
    
    % reorder data and chanlocs based on indexes
    EEG_interp.data(:,:) = EEG_interp.data(idxs,:);
    EEG_interp.chanlocs = EEG_interp.chanlocs(idxs);

    % updata icachansind for a correct match to the channels used in ica
    indcomps = nan(1, length(EEG_interp.icachansind));
    for compidx = 1:length(EEG_interp.icachansind)
        indcomps(compidx) = find(EEG_interp.icachansind(compidx) == idxs);
    end

    %checking if the layout has changed
    %figure; topoplot([],EEG_interp.chanlocs,'style','blank','electrodes','labelpoint','chaninfo',EEG_interp.chaninfo);% looks normal
    %% 8. Re-Reference
    EEG_final = pop_reref( EEG_interp, []);
    
    %% 9. Epoch the data   
    % Define epoch length in seconds
   epoch_length_seconds = 5;
   EEG_epoched_5 = eeg_regepochs(EEG_final, 'recurrence', epoch_length_seconds, 'extractepochs', 'on');
   
   %    % sanity check for how many epochs it actually are
%    % Get the sampling rate
% sampling_rate = EEG_final.srate;
% 
% % Calculate expected number of data points per epoch
% expected_data_points_per_epoch = epoch_length_seconds * sampling_rate;
% 
% % Get the time vector for the epochs
% epoch_time_vector = EEG_epoched_5.times;
% 
% % Calculate the actual epoch length in seconds
% actual_epoch_length = length(epoch_time_vector) / sampling_rate;

    %% 10. save data in prep_power
    outdir = fullfile(proj_dir,'data\prep_power_5');
    save(fullfile(outdir,[tmp_id + "_prep_p_5.mat"]),"EEG_epoched_5");
    
end

clear
close all
clc

%% 11. find out how many epochs survived
% set paths
proj_dir = fullfile(pwd); % automatically get path of script location, and parent dir
indir = fullfile(proj_dir,'data\prep_power_5');% 
outdir = fullfile(proj_dir,'data\analysis_power'); % 
indat = dir(indir); % content of that folder
indat = indat(startsWith({indat.name}, 'sub-')); % only keep folders that start with 'sub-' (i.e. the subjects)

n_epochs = table();

for m = 1:length(indat)
load(fullfile(indir,indat(m).name));
    
    tmp_id = extractBefore(indat(m).name,'_');
    cell_info = cell(1,3); 
   for row = 1
   for col = 1
      cell_info{row,col} = tmp_id;% VPCode
   end 
   for col = 2
       cell_info{row,col} = length(EEG_epoched_5.epoch);% number of epochs that survived
   end
   for col = 3
       cell_info{row,col} = 60 - length(EEG_epoched_5.epoch); % number of 'missing' epochs
   end
   end
   % create table names
   VarNames = ["participant_id" "number_epochs" "missing_epochs"];

   currTable = table(cell_info(:,1),cell_info(:,2),cell_info(:,3),'VariableNames',VarNames);
    
    n_epochs = vertcat(n_epochs, currTable);
  
end

% save

csvFile = 'number_of_epochs_5.csv';
writetable(n_epochs, fullfile(pwd,'data','analysis_power',csvFile));