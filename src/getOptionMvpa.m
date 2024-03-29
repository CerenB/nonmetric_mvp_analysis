% (C) Copyright 2019 CPP BIDS SPM-pipeline developpers

function opt = getOptionMvpa()
  % returns a structure that contains the options chosen by the user to run
  % decoding with cosmo-mvpa.

  if nargin < 1
    opt = [];
  end

  % suject to run in each group
  opt.subjects = {'013', '014', '015', '016', '017', ...
                  '018', '019', '020', '021', '023', ...
                  '024', '025', '026','027', '028', ...
                  '029', '030', '031', '032', '033'}; 


  % Uncomment the lines below to run preprocessing
  % - don't use realign and unwarp
  opt.realign.useUnwarp = true;

  % we stay in native space (that of the T1)
  opt.space = 'MNI'; % 'individual', 'MNI'

  % The directory where the data are located
  opt.dataDir = fullfile(fileparts(mfilename('fullpath')), ...
                         '..', '..', '..', 'nonmetric_raw');
                     
  opt.dir.derivatives = fullfile(opt.dataDir, '..', ...
                                  'nonmetric_derivatives_cpp_spm');
  opt.dir.stats = fullfile(opt.dataDir, '..', ...
                                  'nonmetric_derivatives_cpp_spm-stats');                          
  opt.dir.raw = opt.dataDir;

  opt.pathOutput = fullfile(opt.dir.raw, '..', 'derivatives', 'cosmoMvpa');

  % multivariate
  opt.model.file = fullfile(fileparts(mfilename('fullpath')), '..', ...
                            'model', 'model-NonmetricDecoding1_smdl.json');

  % task to analyze
  opt.taskName = 'Nonmetric';

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
  opt.mvpa.condLabelName = {'simple', 'nonmetric'};
  opt.mvpa.decodingCondition = 'simpleVsnonmetric';
  
  % define the 4D maps to be used
  opt.funcFWHM = 2;

  % take the most responsive xx nb of voxels
  opt.mvpa.ratioToKeep = 120; % 100 150 250 350 420

  % set which type of ffx results you want to use
  opt.mvpa.map4D = {'beta', 't_maps'};

  % design info
  opt.mvpa.nbRun = 9;
  opt.mvpa.nbTrialRepetition = 1;

  % cosmo options
  opt.mvpa.tool = 'cosmo';
  opt.mvpa.normalization = 'zscore';
  opt.mvpa.child_classifier = @cosmo_classify_libsvm;
  opt.mvpa.feature_selector = @cosmo_anova_feature_selector;

  % permute the accuracies ?
  opt.mvpa.permutate = 1;

end
