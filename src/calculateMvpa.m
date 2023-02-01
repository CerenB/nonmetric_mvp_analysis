function accu = calculateMvpa(opt, roiSource)

% main function which loops through masks and subjects to calculate the
% decoding accuracies for given conditions.
% dependant on SPM and CosMoMvpa toolboxes
% the output is compatible for R visualisation, it gives .csv file as well
% as .mat file 

  % get the smoothing parameter for 4D map
  funcFWHM = opt.funcFWHM;

  % choose masks to be used
  opt = chooseMask(opt, roiSource);

  %% set output folder/name
  savefileMat = fullfile(opt.pathOutput, ...
                         [opt.taskName, ...
                          'Decoding_', ...
                          roiSource, ...
                          '_s', num2str(funcFWHM), ...
                          '_ratio', num2str(opt.mvpa.ratioToKeep), ...
                          '_', datestr(now, 'yyyymmddHHMM'), '.mat']);

  savefileCsv = fullfile(opt.pathOutput, ...
                         [opt.taskName, ...
                          'Decoding_', ...
                          roiSource, ...
                          '_s', num2str(funcFWHM), ...
                          '_ratio', num2str(opt.mvpa.ratioToKeep ), ...
                          '_', datestr(now, 'yyyymmddHHMM'), '.csv']);

  % anything to omit/midful about?
  if opt.omit.subject.do == 1
      omitSubject = [26,32];
      omitMask = opt.omit.subject.mask;
  end

  %% let's get going!

  % set structure array for keeping the results
  accu = struct( ...
                'subID', [], ...
                'mask', [], ...
                'accuracy', [], ...
                'prediction', [], ...
                'maskVoxNb', [], ...
                'choosenVoxNb', [], ...
                'image', [], ...
                'ffxSmooth', [], ...
                'roiSource', [], ...
                'decodingCondition', [], ...
                'permutation', [], ...
                'imagePath', []);

  count = 1;

  for iSub = 1:numel(opt.subjects)

    % get FFX path
    subID = opt.subjects{iSub};
    ffxDir = getFFXdir(subID, funcFWHM, opt);

    % get subject folder name
    subFolder = ['sub-', subID];

    
    subType = 'badTapper';
    subTypeLabel = 0;
    % save good/bad tapper info
    if ismember(str2double(subID),opt.tapper.good)
        subType = 'goodTapper';
        subTypeLabel = 1;
    end
    
    for iImage = 1:length(opt.mvpa.map4D)

      for iMask = 1:length(opt.maskName)

        % choose the mask
        mask = fullfile(opt.maskPath, opt.maskName{iMask});
        if strcmp(roiSource, 'freesurfer') || strcmp(roiSource, 'hmat')
          mask = fullfile(opt.maskPath, subFolder, [opt.maskBaseName, opt.maskName{iMask}]);
        end

        % display the used mask
        disp(opt.maskName{iMask});
        
        % 4D image
        imageName = ['4D_', opt.mvpa.map4D{iImage}, '_', num2str(funcFWHM), '.nii'];
        image = fullfile(ffxDir, imageName);

        % load cosmo input
        ds = cosmo_fmri_dataset(image, 'mask', mask);

        % Getting rid off zeros
        zeroMask = all(ds.samples == 0, 1);
        ds = cosmo_slice(ds, ~zeroMask, 2);

        % set cosmo structure
        ds = setCosmoStructure(opt, ds, opt.mvpa.condLabelNb, opt.mvpa.condLabelName);

        % slice the ds according to your targers (choose your
        % train-test conditions
        ds = cosmo_slice(ds, ds.sa.targets == 1 | ds.sa.targets == 2);

        % remove constant features
        ds = cosmo_remove_useless_data(ds);

        % calculate the mask size
        maskVoxel = size(ds.samples, 2);

        % partitioning, for test and training : cross validation
        partitions = cosmo_nfold_partitioner(ds);
        
        % use the ratios, instead of the voxel number:
        opt.mvpa.feature_selection_ratio_to_keep = opt.mvpa.ratioToKeep;
        
        % if SMA, double the voxel number
        if strcmpi(opt.maskLabel{iMask}, 'sma')
           opt.mvpa.feature_selection_ratio_to_keep = 2 * opt.mvpa.ratioToKeep;
        end

        % ROI mvp-analysis
        if opt.omit.subject.do && contains(opt.maskName{iMask}, omitMask) && ...
                ismember(str2double(subID), omitSubject)
            pred = 'NA';
            accuracy = 'NA';
            if opt.mvpa.permutate  == 1
                acc0 = 'NA';
            end
        else
            [pred, accuracy] = cosmo_crossvalidate(ds, ...
                                   @cosmo_classify_meta_feature_selection, ...
                                   partitions, opt.mvpa);
                               
            if opt.mvpa.permutate  == 1
                [acc0] = permuteAccuracy(ds,accuracy, partitions, opt.mvpa);
                accu(count).permutation = acc0';
            end                   
        end
        
        

        %% store output
        accu(count).subID = subID;
        accu(count).mask = opt.maskLabel{iMask};
        accu(count).maskVoxNb = maskVoxel;
%         accu(count).choosenVoxNb = opt.mvpa.feature_selection_ratio_to_keep;
        accu(count).choosenVoxNb = opt.mvpa.ratioToKeep;
        accu(count).image = opt.mvpa.map4D{iImage};
        accu(count).ffxSmooth = funcFWHM;
        accu(count).accuracy = accuracy;
        accu(count).prediction = pred;
        accu(count).imagePath = image;
        accu(count).roiSource = roiSource;
        accu(count).decodingCondition = opt.mvpa.decodingCondition;
        accu(count).subType = subType;
        accu(count).subTypeLabel = subTypeLabel;

        % increase the counter and allons y!
        count = count + 1;

        fprintf(['Sub'  subID ' - area: ' opt.maskLabel{iMask} ...
                 ', accuracy: ' num2str(accuracy) '\n\n\n']);

      end
    end
  end
  %% save output

  % mat file
  save(savefileMat, 'accu');

  % csv but with important info for plotting
  csvAccu = rmfield(accu, 'permutation');
  csvAccu = rmfield(csvAccu, 'prediction');
  csvAccu = rmfield(csvAccu, 'imagePath');
  writetable(struct2table(csvAccu), savefileCsv);

end

function ds = setCosmoStructure(opt, ds, condLabelNb, condLabelName)
  % sets up the target, chunk, labels by stimuli condition labels, runs,
  % number labels.

  % design info from opt
  nbRun = opt.mvpa.nbRun;
  betasPerCondition = opt.mvpa.nbTrialRepetition;

  % chunk (runs), target (condition), labels (condition names)
  conditionPerRun = length(condLabelNb);
  betasPerRun = betasPerCondition * conditionPerRun;

  chunks = repmat((1:nbRun)', 1, betasPerRun);
  chunks = chunks(:);

  targets = repmat(condLabelNb', 1, nbRun)';
  targets = targets(:);
  targets = repmat(targets, betasPerCondition, 1);

  condLabelName = repmat(condLabelName', 1, nbRun)';
  condLabelName = condLabelName(:);
  condLabelName = repmat(condLabelName, betasPerCondition, 1);

  % assign our 4D image design into cosmo ds git
  ds.sa.targets = targets;
  ds.sa.chunks = chunks;
  ds.sa.labels = condLabelName;

  % figure; imagesc(ds.sa.chunks);

end
