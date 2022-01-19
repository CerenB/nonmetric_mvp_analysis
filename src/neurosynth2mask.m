function info = neurosynth2mask

  % this small function makes neurosynth masks in the same space (and resolution)
  % as the 4D beta/tmaps.
  % it is using cpp-spm function to set and access to the subject input, and
  % using nifti_toolbox to read/save/reslice the .nii files
  % spm
  %% add paths
  % spm - for now
  warning('off');
  addpath(genpath('/Users/battal/Documents/MATLAB/spm12'));

  % add cpp-spm
  cppSPM = '/Users/battal/Documents/GitHub/CPPLab/CPP_SPM';
  addpath(genpath(fullfile(cppSPM, 'src')));
  addpath(genpath(fullfile(cppSPM, 'lib')));

  % roi path
  roiPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                     '..', '..', 'RhythmCateg_ROI', 'neurosynth', 'functional');

  roiPathRaw = fullfile(roiPath, 'raw');

  roiPathDerivatives = fullfile(roiPath, 'derivatives');

  opt = getOptionBlockMvpa();

  checkDependencies();

  %% inputs
  % which 4D map to use as template?
  % first define the smoothing
  funcFWHM = 0;

  % extract subject info
  [~, opt, group] = setUpWorkflow(opt, 'get ready for MVPA');

  % take only 1 subject 4D map as template (MNI space)
  subID = group(1).subNumber{1};

  ffxDir = getFFXdir(subID, funcFWHM, opt);

  % prefix to be used to make realign, reslice masks
  prefix = 'r';

  % an example below is reslicing the functional ROI from Neurosynth
  % according to the 4D.nii file resolution
  % roiName = 'auditory_FDR_0.01.nii';
  roiName = 'premotor_FDR_0.01.nii';
  % roiName = 'sma_FDR_0.01.nii';
  roi = fullfile(roiPathRaw, roiName);

  % z-score, defines the size of the mask
  % threshold = 3:9; % aud
  % threshold = 7:11; % sma
  threshold = 3:9; % premotor

  %% inclusive mask, load already make .nii mask (in marsbar)
  useInclusiveMask = 1;

  % InclMaskName = 'inclusiveMask_audL_r26--48_-20_6.nii';
  % InclMaskName = 'inclusiveMask_audR_r26-52_-10_6.nii';

  % InclMaskName = 'inclusiveMask_sma_r20-0_-4_58.nii';
  % InclMaskName = 'inclusiveMask_premotorR_r20-50_0_40.nii';
  InclMaskName = 'inclusiveMask_premotorL_r20--54_0_34.nii';
  InclMask = fullfile(roiPath, '..', InclMaskName);

  % load mask
  maskInc = load_nii(InclMask);
  % save image as double
  maskInc.img = double(maskInc.img);

  % define the hemisphere of ROI ! ! ! you have to define it!
  hemisphere = 'left'; % left, right or both (for sma)

  %% realign + reslicing the masks
  count = 1;
  for iThres = threshold

    % load roi
    naiveRoi = load_nii(roi);

    % take out below threshold
    idx = find(naiveRoi.img < iThres);
    naiveRoi.img(idx) = 0; %#ok<FNDSB>

    % make binary mask for above threshold
    idx = find(naiveRoi.img >= iThres);
    naiveRoi.img(idx) = 1; %#ok<FNDSB>

    % calculate the voxel number
    voxelNbNaive = sum(naiveRoi.img(:));

    % save
    thresRoiName = ['nativeThres_', num2str(iThres), '_', roiName];
    thresRoi = fullfile(roiPathDerivatives, thresRoiName);
    save_nii(naiveRoi, thresRoi);

    %% realign and reslice the new-roi
    % so that it is in the same space as your 4D images
    matlabbatch = [];
    matlabbatch{1}.spm.spatial.realign.write.data = {
                                                     fullfile(ffxDir, '4D_t_maps_0.nii,1')
                                                     [thresRoi, ',1']
                                                    };
    matlabbatch{1}.spm.spatial.realign.write.roptions.which = [2 1];
    matlabbatch{1}.spm.spatial.realign.write.roptions.interp = 4;
    matlabbatch{1}.spm.spatial.realign.write.roptions.wrap = [0 0 0];
    matlabbatch{1}.spm.spatial.realign.write.roptions.mask = 1;
    matlabbatch{1}.spm.spatial.realign.write.roptions.prefix = 'r';
    spm_jobman('run', matlabbatch);

    delete(fullfile(ffxDir, ['r4D_t_maps*', '.nii']));
    delete(fullfile(ffxDir, ['mean4D_t_maps*', '.nii']));

    % call the realigned image
    realignRoiName = [prefix, thresRoiName];
    realignRoi = fullfile(roiPathDerivatives, realignRoiName);

    %% after reslicing, turn it again into binary mask
    % load roi
    %     mask = load_nii(resliceRealignRoi);
    mask = load_nii(realignRoi);

    % binarise
    mask.img(mask.img <= 0.1) = 0.0;
    mask.img(mask.img > 0) = 1.0;
    voxelNbBefore = sum(mask.img(:));

    % save
    binaryrRealignRoiName = ['bin_', realignRoiName];
    binaryrRealignRoi = fullfile(roiPathDerivatives, binaryrRealignRoiName);
    save_nii(mask, binaryrRealignRoi);

    %% use inclusive mask to select only SMA/premotor/...
    outputMaskName = [hemisphere, binaryrRealignRoiName];
    outputMask = fullfile(roiPathDerivatives, outputMaskName);

    if useInclusiveMask == 1
      % Create a template
      temp = mask;
      temp.fileprefix = 'temp';
      temp.img = [];

      % mask out the non-common areas
      temp.img = mask.img + maskInc.img;
      idx = find(temp.img < 2);
      mask.img(idx) = 0; %#ok<FNDSB>

      % tell me the end voxel number
      voxelNb = sum(mask.img(:));

      % save voxel info to struct
      info(count).ROIname = outputMaskName;
      info(count).VoxNb2mmNaive = voxelNbNaive;
      info(count).VoxNbNoMask = voxelNbBefore;
      info(count).VoxNbMasked = voxelNb;

      count = count + 1;
    end
    % save the sma/premotor-only mask
    save_nii(mask, outputMask);
  end

end
