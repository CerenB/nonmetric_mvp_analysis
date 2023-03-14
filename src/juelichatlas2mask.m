function [info, opt] = juelichatlas2mask

% this small function makes binary masks in the same space (and resolution)
% as the 4D beta/tmaps.
% it uses cpp-spm functions

%% add paths
% spm - for now
warning('off');
addpath(genpath('/Users/battal/Documents/MATLAB/spm12'));

% add cpp-spm (cpp-roi)
run ../lib/CPP_SPM/initCppSpm.m;


% roi path
  % we stay in native space (that of the T1)
  opt.space = 'MNI'; % 'individual', 'MNI'
  
opt = checkOptions(opt);

opt.dir.roi = fullfile(fileparts(mfilename('fullpath')), '..', ...
    '..', '..', '..', 'RhythmCateg_ROI', 'jubrainatlas', 'try');

roiPathDerivatives = fullfile(opt.dir.roi, 'derivatives');



  % The directory where the data are located
  opt.dataDir = fullfile(fileparts(mfilename('fullpath')), ...
                         '..', '..', '..', 'nonmetric_raw');
                     
  opt.dir.derivatives = fullfile(opt.dataDir, '..', ...
                                  'nonmetric_derivatives_cpp_spm');
  opt.dir.stats = fullfile(opt.dataDir, '..', ...
                                  'nonmetric_derivatives_cpp_spm-stats');                          
  opt.dir.raw = opt.dataDir;

  opt.pathOutput = fullfile(opt.dir.raw, '..', 'derivatives', 'cosmoMvpa');

  
  opt.dir.jobs = fullfile(opt.dir.stats, 'JOBS');
%% inputs
info = struct();

% reslice the images?
opt.reslice.do = 1;

% rename the masks?
opt.maskRename.do = 0;

% probability, defines the size of the mask
threshold = 0.7;

% extract dummy subject info
% iSub = 1;
% subID = opt.subjects{iSub};
%% create model file
% % path to a raw BIDS dataset
% bids_dir = opt.dir.input;
% % path where to output data
% output_dir = opt.dir.roi;
% 

%   % task to analyze
%   opt.taskName = 'Nonmetric';

% bidspm(bids_dir, output_dir, 'dataset', ...
%         'action', 'default_model', ...
%         'verbosity', 2, ...
%         'space', {'IXI549Space'}, ...
%         'options', struct([]), ...,
%         'ignore', {}, ...
%         'task', {opt.taskName})

%% folder of 4D file and masks
opt.model.file = fullfile(roiPathDerivatives, ...
                          'models', 'model-defaultNonmetric_smdl.json');
opt.pipeline.type = 'stats';                      
 
% The functional smoothing 
opt.fwhm.func = 2;

% space to analyse
opt.space = 'MNI';


% ffxDir = getFFXdir(subID, opt);
ffxDir = '../../../nonmetric_raw/../nonmetric_derivatives_cpp_spm-stats/sub-013/stats/task-Nonmetric_space-MNI_FWHM-2';
imageName = ['4D_', opt.mvpa.map4D{1}, '_', num2str(opt.funcFWHM), '.nii'];
dataImage = fullfile(ffxDir, imageName);

% define the paths, names of the probability rois
allMasks = dir(opt.dir.roi);
allMasks([allMasks.isdir]) = [];
allMasks = allMasks(~ismember({allMasks.name},{'.','..', '.DS_Store'}));
% roiNames = { 'lsts2','rsts2', 'lprimary1', 'rprimary1',...
%                 'lprimary2', 'rprimary2', 'lprimary3', 'rprimary3', ...
%                 'lstg1', 'rstg1', 'lstg2', 'rstg2', 'lstg3', 'rstg3', ...
%                 'lstg4', 'rstg4', 'lstg5', 'rstg5', 'ltpj1', 'rptj1'};
%             % 'lsts1', 'rsts1', 

count = 1;
for imask = 1:length(allMasks)

    maskImageName = allMasks(imask).name;
    inputImage = fullfile(opt.dir.roi, maskImageName);
    
    %% realign + reslicing the masks
    % rename the probab mask
    if opt.maskRename.do
        zMap = renameProbMaps(inputImage);
    else
        zMap = inputImage;
    end
    % reslice if needed
    if opt.reslice.do
        % If needed reslice probability map to have same resolution as the data image
        % resliceImg won't do anything if the 2 images have the same resolution
        zMap = resliceRoiImages(dataImage, zMap);
    end
    
    % threshold the probability and make a mask image
    roiName = thresholdToMask(zMap, threshold);
    
    % count mask voxels
    img = readImage(roiName);
    voxelNb = sum(img(img==1));
    
    % count zmap voxels
    img = readImage(zMap);
    voxelNbzMap = sum((img(:)>0.1));
    
    % save opt info with mask
  %  opt.maskName(imask) = maskImageName;
  %  opt.maskLabel(imask) = roiNames(imask);
    
    % save mask info to struct
    [~, info(count).roiName] = fileparts(roiName);
    info(count).VoxNbMasked = voxelNb;
    [~, info(count).zMapName] = fileparts(zMap);
    info(count).voxelNbzMap = voxelNbzMap;
    
    count = count +1;
end

save(fullfile(roiPathDerivatives,['maskInfo', datestr(now, 'yyyymmddHHMM')]), 'info');

end
%     %% combine masks?
% 
%       % Create a template
%       temp = mask;
%       temp.fileprefix = 'temp';
%       temp.img = [];
% 
%       % mask out the non-common areas
%       temp.img = mask.img + maskInc.img;
%       idx = find(temp.img < 2);
%       mask.img(idx) = 0; %#ok<FNDSB>
% 
%       % tell me the end voxel number
%       voxelNb = sum(mask.img(:));
% 
%       % save voxel info to struct
%       info(count).ROIname = outputMaskName;
%       info(count).VoxNb2mmNaive = voxelNbNaive;
%       info(count).VoxNbNoMask = voxelNbBefore;
%       info(count).VoxNbMasked = voxelNb;
% 
%       count = count + 1;
% 
%     % save the sma/premotor-only mask
%     save_nii(mask, outputMask);


