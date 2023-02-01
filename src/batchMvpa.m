clear ;
clc;

%% set paths
  % spm
  warning('off');
  addpath(genpath('/Users/battal/Documents/MATLAB/spm12'));
  % cosmo
  cosmo = '~/Documents/MATLAB/CoSMoMVPA';
  addpath(genpath(cosmo));
  cosmo_warning('once');

  % libsvm
  libsvm = '~/Documents/MATLAB/libsvm';
  addpath(genpath(libsvm));
  % verify it worked.
  cosmo_check_external('libsvm'); % should not give an error
  
  % add cpp repo
  run ../lib/CPP_SPM/initCppSpm.m;
  
  % add mini-helper functions
  addpath(genpath(fullfile(pwd, 'subfun')));
  
  % load your options
  opt = getOptionMvpa();

  %% run mvpa 
  
  % tapping info
  opt.tapper.do = 1;
  opt.tapper.good = [1,2,5,7,8,9,10,12,14,15,16,21,25,27,28,30,32,33];

  % use parcels or NS masks?
  roiSource = 'contrastSTGOnly'; % 'freesurfer', 'neurosynth', 'hmat'
  opt.omit.subject.do = 0;
  opt.omit.subject.mask = 'cerebellum';
  opt.omit.subject.ID = [26,32];
  accuracy = calculateMvpa(opt, roiSource);
  
  
  
  
  