function opt = hmat2mask(opt)
% this is a script for converting HMAT masks into subject space mask
% alternative script is converting Freesurfer(FS) masks into MNI mask

% idea behind is this: use HMAT masks but limit them a bit with FS
% BA6 label. In FS, BA6 label contains all SMA, premotor, etc... We need to
% separate these regions in BA6 label by using HMAT masks. 
% so we should work in native space. 

% in this way, we would have auditory cortex, BG, and preSMA, SMA, premotor
% all in native subject space. 

% then we can use these masks to run decoding, and FFT. 

% GOAL
% 0. Insert 1x1x1mm masks into group folder with bids-valid name
% 1. First reslice the ROI according to ?THE NORMALIZED? functional image.
% 2. Then do the inverse transformation by using anat segmented iy*
% 3. To go back to func native space res, we reslice the ned product
% 4. and binarise it
% 5. lastly, we bidsify the output name
% 

% Note: Use nearest neighbour interpolation as well when doing the reverse transformation
% Note: inverse transformation only accepts space-MNI prefix files as input

% reslicing the roi according to the w- functional image (or Tmap.nii)


opt.roi.space = 'individual';
funcFWHM = opt.funcFWHM;


%% let's start
[BIDS, opt] = setUpWorkflow(opt, 'create ROI');

roiDerivativesFolder = 'derivatives';
%  spm_mkdir(fullfile(opt.dir.roi, roiDerivativesFolder));

spm_mkdir(fullfile(opt.dir.roi, roiDerivativesFolder, 'group'));

opt.jobsDir = fullfile(opt.dir.roi, roiDerivativesFolder, 'JOBS', opt.taskName);
  

%% copy from raw to derivatives
if opt.copyRawToDerivatives.do
    
    copyFromRawToDerivatives(opt,roiDerivativesFolder)
    
end

%% reslice the ROI with our normalised functional toy image
if opt.reslice.do
    % If needed reslice probability map to have same resolution as the data image
    % resliceImg won't do anything if the 2 images have the same resolution
    % if you read the data with spm_summarise,
    % then the 2 images do not need the same resolution.
    
    opt.space = 'MNI'; % make sure we are in normalised space
    
    funcMask = resliceToMNIFuncIndivSpace(opt,roiDerivativesFolder, funcFWHM);
    disp(funcMask);
end

if opt.resliceFunc.do
    
    opt.space = 'MNI'; % make sure we are in normalised space
    
    funcMask = resliceToMNIFuncGroupSpace(opt,roiDerivativesFolder, funcFWHM);
    disp(funcMask);
end

%% let's do the inverse transformation now
% now we need to be in individual subject space
if opt.inversTransform.do && any(strcmp(opt.roi.space, 'individual'))
    
    opt.space = 'individual'; % make sure we are in subject space
    
%     count = 1;
    opt.threshold = 0.05;
    
    for iSub = 1:numel(opt.subjects)
        
        subLabel = opt.subjects{iSub};
        subFolder = ['sub-', subLabel];
        
        printProcessingSubject(iSub, subLabel);
        
        roiList = spm_select('FPlist', ...
            fullfile(opt.dir.roi, roiDerivativesFolder, subFolder), ...
            '^space-MNI_label.*_mask.nii.*$');
        roiList = cellstr(roiList);
        
        %% inverse normalize
        % normalise mask according to functional image res
        [image, dataDir] = getAnatFilename(BIDS, subLabel, opt);

        deformation_field = spm_select('FPlist', dataDir, ['^iy_' image '$']);

        matlabbatch = {};
        for iROI = 1:size(roiList, 1)
            matlabbatch = setBatchNormalize(matlabbatch, ...
                                            {deformation_field}, ...
                                            nan(1, 3), ...
                                            roiList(iROI, :));
            matlabbatch{end}.spm.spatial.normalise.write.woptions.bb = nan(2, 3);
        end
        
        
        saveAndRunWorkflow(matlabbatch, 'inverseNormalize', opt, subLabel);
        
        %% reslice into func ind space and rename the files
        
        roiList = spm_select('FPlist', ...
            fullfile(opt.dir.roi, roiDerivativesFolder, subFolder), ...
            '^wspace.*_mask.nii.*$');
        roiList = cellstr(roiList);
        
        % find subj space image to get reslicing reference image
        [meanImage, meanDataDir] = getMeanFuncFilename(BIDS, subLabel, opt);
        dataImage = fullfile(meanDataDir, meanImage);
        
        for iROI = 1:size(roiList, 1)
            
            roiImage = roiList{iROI, 1};
            
            %reslice
            roiImage = resliceRoiImages(dataImage, roiImage);
            roiImage = removeSpmPrefix(roiImage, ...
                                         spm_get_defaults('realign.write.prefix'));
            
            % load the mask
            binariseImage(roiImage, opt.threshold);
 
            % rename
            p = bids.internal.parse_filename(spm_file(roiImage, 'filename'));
            
            entities = struct('task', opt.taskName, ...
                'hemi', p.entities.label(1), ...
                'space', 'individual', ...
                'label', p.entities.label(3:end), ...
                'desc', 'inverseTransform');
            
            nameStructure = struct('entities', entities, ...
                'suffix', 'mask', ...
                'ext', '.nii', ...
                'use_schema', false);      

            newName = bids.create_filename(nameStructure);
            
            movefile(roiImage, fullfile(opt.dir.roi, ...
                                        roiDerivativesFolder, ...
                                        subFolder, ...
                                        newName));
            
        end
       
        
    end
end

if opt.countVoxel.do
    info = struct();
    count = 1;
    
    for iSub = 1:numel(opt.subjects)
        
        subLabel = opt.subjects{iSub};
        subFolder = ['sub-', subLabel];
        
        roiList = spm_select('FPlist', ...
            fullfile(opt.dir.roi, roiDerivativesFolder, subFolder), ...
            ['^task-',opt.taskName,'.*inverseTransform_mask.nii.*$']);
        
        roiList = cellstr(roiList);
        
        for iROI = 1:size(roiList, 1)
            
            roiImage = roiList{iROI, 1};
            
            p = bids.internal.parse_filename(spm_file(roiImage, 'filename'));
            roiName = [p.entities.hemi, p.entities.label];
            
            % read how many voxels it has
            hdr = spm_vol(roiImage);
            img = spm_read_vols(hdr);
            
            voxelNb = sum(img(:));
            
            % save this info
            info(count).SUBname = subLabel;
            info(count).ROIname = roiName;
            info(count).ROInumVox = voxelNb;
            count = count + 1;
            
        end
    end
    
    opt.info = info;
    
end

end

function [funcMask] = resliceToMNIFuncIndivSpace(opt,roiDerivativesFolder,funcFWHM)

%read the recently renamed files in derivatives
maskAll = dir(fullfile(opt.dir.roi,roiDerivativesFolder, 'group', '*_mask.nii'));

    for iSub = 1:numel(opt.subjects)
        
        % get subject folder name
        subLabel = opt.subjects{iSub};
        subFolder = ['sub-', subLabel];
        
        % if sub folder doesn't exist, create it
        spm_mkdir(fullfile(opt.dir.roi,roiDerivativesFolder,subFolder));
        
        % get FFX path
        ffxDir = getFFXdir(subLabel, funcFWHM, opt);
        
        % toy data to be used for reslicing
        dataImage = fullfile(ffxDir, 'spmT_0001.nii');
        
        
        for iMask = 1:length(maskAll)
            
            % read the mask
            groupMask =  fullfile(opt.dir.roi,roiDerivativesFolder, 'group', ...
                                 maskAll(iMask).name);
            
            %copy it into subject specific folder
            funcMask = fullfile(opt.dir.roi,roiDerivativesFolder,subFolder, ...
                maskAll(iMask).name);
            copyfile(groupMask, funcMask);
            
            % reslice the mask in subject specific folder
            funcMask = resliceRoiImages(dataImage, funcMask);
            funcMask = removeSpmPrefix(funcMask, ...
                                       spm_get_defaults('realign.write.prefix'));
            
            
        end
    end

end


function funcMask = resliceToMNIFuncGroupSpace(opt, roiDerivativesFolder, funcFWHM)

spm_mkdir(fullfile(opt.dir.roi, roiDerivativesFolder, 'group', 'func'));

%read the recently renamed files in derivatives
maskAll = dir(fullfile(opt.dir.roi,roiDerivativesFolder, 'group', '*_mask.nii'));

% use one normalised subject
iSub = 1;

% get subject folder name
subLabel = opt.subjects{iSub};

% get FFX path
ffxDir = getFFXdir(subLabel, funcFWHM, opt);

% toy data to be used for reslicing into MNI func group space
dataImage = fullfile(ffxDir, 'spmT_0001.nii');


for iMask = 1:length(maskAll)
    
    % read the mask
    groupMask =  fullfile(opt.dir.roi,roiDerivativesFolder, 'group', ...
        maskAll(iMask).name);
    
    %copy it into group func folder
    funcMask = fullfile(opt.dir.roi,roiDerivativesFolder,'group', ...
        'func', maskAll(iMask).name);
    copyfile(groupMask, funcMask);
    
    % reslice the mask in subject specific folder
    funcMask = resliceRoiImages(dataImage, funcMask);
    funcMask = removeSpmPrefix(funcMask, ...
                               spm_get_defaults('realign.write.prefix'));

end


end

function copyFromRawToDerivatives(opt, roiDerivativesFolder)

    % first rename HMAT rois into more bids-friendly way
    maskAll = dir(fullfile(opt.dir.roi,'raw','*.nii'));
    
    for iFile = 1:numel(maskAll)
        % raw file
        rawMask = fullfile(opt.dir.roi,'raw', maskAll(iFile).name);
        
        %check if its bids-formatted already
        if ~contains(rawMask, 'space')
            % rename HMAT masks
            rawMask = renameHmat(rawMask);
        end
        
        % now it's bids-friendly, lets move them to derivatives
        groupMask = strrep(rawMask, 'raw', [roiDerivativesFolder,'/group']);
        copyfile(rawMask, groupMask);
    end

end

function binariseImage(imageName, threshold)

            hdr = spm_vol(imageName);
            img = spm_read_vols(hdr);
                                     
            %binarize
            img(img(:) > threshold) = 1;
            img(img(:) <= threshold) = 0;
     
            % save 
            spm_write_vol(hdr, img);

end