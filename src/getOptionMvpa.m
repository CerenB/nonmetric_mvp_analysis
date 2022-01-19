% (C) Copyright 2019 CPP BIDS SPM-pipeline developpers

function opt = getOptionMvpa()
  % returns a structure that contains the options chosen by the user to run
  % decoding with cosmo-mvpa.

  if nargin < 1
    opt = [];
  end

  % suject to run in each group
  opt.subjects = {'001','002', '003', '004', '005', '006',...
                  '007', '008', '009', '010', '011', ...
                  '012', '013', '014','015', '016', '017', ...
                  '018', '019', '020', '021', '023'};


    
  % Uncomment the lines below to run preprocessing
  % - don't use realign and unwarp
  opt.realign.useUnwarp = true;

  % we stay in native space (that of the T1)
  opt.space = 'individual'; % 'individual', 'MNI'

  % The directory where the data are located
  opt.dataDir = fullfile(fileparts(mfilename('fullpath')), ...
                         '..', '..', '..', 'raw');
  opt.derivativesDir = fullfile(opt.dataDir, '..', 'derivatives', 'cpp_spm');

  opt.pathOutput = fullfile(opt.dataDir, '..', 'derivatives', 'cosmoMvpa');

  % multivariate
  opt.model.file = fullfile(fileparts(mfilename('fullpath')), '..', ...
                            'model', 'model-RhythmBlockDecoding1_smdl.json');

  % task to analyze
  opt.taskName = 'RhythmBlock';

  opt.parallelize.do = false;
  opt.parallelize.nbWorkers = 1;
  opt.parallelize.killOnExit = true;

  %% DO NOT TOUCH
  opt = checkOptions(opt);
  saveOptions(opt);
  % we cannot save opt with opt.mvpa, it crashes

  %% mvpa options
  
  % set cosmo mvpa structure
  opt.mvpa.condLabelNb = [1 2];
  opt.mvpa.condLabelName = {'simple', 'complex'};
  opt.mvpa.decodingCondition = 'simpleVscomplex';
  
  % define the 4D maps to be used
  opt.funcFWHM = 2;

  % take the most responsive xx nb of voxels
  opt.mvpa.ratioToKeep = 300; % 100 150 250 350 420

  % set which type of ffx results you want to use
  opt.mvpa.map4D = {'beta', 't_maps'};

  % design info
  opt.mvpa.nbRun = 9;
  opt.mvpa.nbTrialRepetition = 1;

  % cosmo options
  opt.mvpa.tool = 'cosmo';
  % opt.mvpa.normalization = 'zscore';
  opt.mvpa.child_classifier = @cosmo_classify_libsvm;
  opt.mvpa.feature_selector = @cosmo_anova_feature_selector;

  % permute the accuracies ?
  opt.mvpa.permutate = 1;

end
