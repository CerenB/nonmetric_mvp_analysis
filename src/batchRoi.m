clear ;
clc;

%% set paths
  % spm
  warning('off');
  addpath(genpath('/Users/battal/Documents/MATLAB/spm12'));
  
  % add cpp repo
  run ../lib/CPP_SPM/initCppSpm.m;
   
  % load your options
  opt = getOptionBlockMvpa();

  %% make Freesurfer based ROIs
  
    % roi path
  parcelPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                        '..', '..', '..','RhythmCateg_ROI', 'freesurfer');
  % which parcels to use?
  useAudParcel = 0;
  
  % make rois
  % action 1 = makes individual/separate parcels and create binary masks
  % action 2 = combines the left hemisphere masks into 1 (e.g. left-basal
  % ganglia, or left-auditory cortex).
  % action 3 = reslice the masks by using spm func to the functional (4D.nii) image
  % action 4 = smooths the masks for auditory parcels
  action = 3;
  info = parcel2mask(action, parcelPath, opt, useAudParcel);
  
  
  useAudParcel = 1;
  action = 3;
  info = parcel2mask(action, parcelPath, opt, useAudParcel);

  action = 4;
  opt.maskFWHM = 2;
  info = parcel2mask(action, parcelPath, opt, useAudParcel);
  
  %% make HMAT based ROIs
  opt.dir.roi = fullfile(fileparts(mfilename('fullpath')),  ...
      '..', '..', '..', '..', 'RhythmCateg_ROI', 'hmat');
  
  % dont start from copying the raw masks
  opt.copyRawToDerivatives.do = true;
  
  % reslice the mask into mni  func space
  opt.reslice.do = true;
  
  % reslice mask to normalised func space
  % (only once is enough for MNI space roi)
  opt.resliceFunc.do = false;
  
  % transform space from "mni func image" to "individual func image"
  opt.inversTransform.do = true;
  
  % count the mask voxel
  opt.countVoxel.do = true;
  
  opt = hmat2mask(opt);
  

  
  
  
  
  
  
  
  
  