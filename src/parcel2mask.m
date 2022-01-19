function info = parcel2mask(action, parcelPath, opt, useAudParcel)

  % it takes the atlas defined parcels from FS output (in the subject's
  % anatomical native space)
  % selectively gets the only parcel-of-interests
  % then convert these parcels into subject's native functional resolution
  % and lastly binarise these masks to be used in further analysis e.g.
  % decoding/FFT/ ...

  % action 1 = makes individual/separate parcels and create binary masks
  % action 2 = combines the left hemisphere masks into 1 (e.g. left-basal
  % ganglia, or left-auditory cortex).
  % action 3 = reslice the masks by using spm func to the functional (4D.nii) image
  % action 4 = smooths the masks for auditory parcels

  % be careful, it has two options, with auditory cortex and with basal
  % ganglia (parcel numbers, labels, roi names differ)



  %% set info
  opt.binarise.do = true;

  % smooth info for ffxDir
  funcFWHM = opt.funcFWHM;

  % smooth the realigned/resliced mask?
  % only applied to AudCx, because BG does not need smoothing           
  prefixSmooth = ['s', num2str(opt.maskFWHM)];
  opt.threshold = 0.05;
  
  %% let's start
  if useAudParcel == 1
    % parcel codes in FS LookUp Table
    parcelCodes = [11136, 11175, 11133, 12136, 12175, 12133];

    % ROIs labels in FS LookUp table
    parcelLabels = {'lPT', 'ltransverse', 'lsup_transv', ...
                    'rPT', 'rtransverse', 'rsup_transv'};

    % concat parcel name
    concatParcelName = 'auditorycx.nii';

    % choose which masks to realign with functional image
    maskToAlign = {'lauditorycx.nii', 'rauditorycx.nii'};

  else
    parcelCodes = [11, 12, 13, 26, 50, 51, 52, 58];

    parcelLabels = {'lcaudate', 'lputamen', 'lpallidum', 'lna', ...
                    'rcaudate', 'rputamen', 'rpallidum', 'rna'};

    concatParcelName = 'basalganglia.nii';

    maskToAlign = {'lbasalganglia.nii', 'rbasalganglia.nii'};

  end

  % parcel numbers in 1 hemisphere
  parcelNb = length(parcelLabels) / 2;

  wholeParcelName = 'destrieux.nii';

  %% let's shave fun
  switch action

    case 1

      count = 1;
      for iSub = 1:length(opt.subjects)

        % get subject folder name
        subID = ['sub-', opt.subjects{iSub}];

        % loop for separate parcels
        for iParcel = 1:length(parcelCodes)

          % load the whole parcel
          parcel1 = load_untouch_nii(fullfile(parcelPath, subID, wholeParcelName));

          % assign zero to all the other parcels
          parcel1.img (parcel1.img ~= parcelCodes(iParcel)) = 0;

          % assign one to the selected parcel
          parcel1.img (parcel1.img == parcelCodes(iParcel)) = 1;

          % count the voxel number
          voxelNb = sum(parcel1.img(parcel1.img == 1));

          % save the new binary parcel/mask
          parcelOfInterest = fullfile(parcelPath, subID, ...
                                      [parcelLabels{iParcel}, '.nii']);
          save_untouch_nii(parcel1, parcelOfInterest);

          info(count).SUBname = subID;
          info(count).ROIname = parcelLabels{iParcel};
          info(count).ROIcode = parcelCodes(iParcel);
          info(count).ROInumVox = voxelNb;
          count = count + 1;
        end
      end

      save(fullfile(parcelPath, 'info'), 'info');

    case 2
      % now let's combine the masks in 1 hemisphere
      count = 1;
      for iSub = 1:length(opt.subjects)

        % get subject folder name
        subID = ['sub-', opt.subjects{iSub}];

        % loop for separate parcels
        for iParcel = 1:parcelNb:(length(parcelCodes))

          parcelImg = [];

          % load the parcels in 1 hemisphere
          for i = iParcel:iParcel + (parcelNb - 1)

            parcelName = [parcelLabels{i}, '.nii'];
            parcel = load_untouch_nii(fullfile(parcelPath, subID, parcelName));
            parcelImg = cat(4, parcelImg, parcel.img);

          end

          % open a template to save these parcels into 1 image
          temp = parcel;
          temp.fileprefix = 'parcel';
          temp.img = [];

          % sum all the parcels
          temp.img = squeeze(sum(parcelImg, 4));
          % make binary mask
          temp.img(temp.img == parcelNb) = 1;

          % save the new binary parcel/mask
          parcelOfInterest = fullfile(parcelPath, subID, ...
                                      [parcelName(1), concatParcelName]);
          save_untouch_nii(temp, parcelOfInterest);

          % calculate the voxel num
          voxelNb = sum(temp.img(temp.img == 1));

          % save voxel info into a struct
          info(count).SUBname = subID;
          info(count).ROIname = [parcelName(1), concatParcelName];
          info(count).ROInumVox = voxelNb;
          count = count + 1;
        end
      end

    case 3
      % let's realign and reslice the masks with functional image
      % assuming the binary masks are ready to go

      count = 1;
      for iSub = 1:length(opt.subjects)

        % get subject folder name
        % subID = opt.subjects{iSub};
        subID = ['sub-', opt.subjects{iSub}];

        % get ffx - 4D image folder
        ffxDir = getFFXdir(opt.subjects{iSub}, funcFWHM, opt);

        for iMask = 1:length(maskToAlign)

          % choose the mask to realign and reslice
          maskName = maskToAlign{iMask};
          maskPath = fullfile(parcelPath, subID);
          mask = fullfile(maskPath, maskName);
          image = fullfile(ffxDir, 'spmT_0001.nii'); % ['4D_beta_', num2str(funcFWHM),'.nii']

          %% reslice the new-roi
          % so that it is in the same resolution as your 4D images
          mask = resliceRoiImages(image, mask);
          
          % save bids name
          [p, voxelNb, img] = saveBids(mask, maskPath, opt);
          
          
          % save voxel info into a struct
          info(count).SUBname = subID;
          info(count).ROIname = p.entities.label; 
          info(count).ROIhemis = p.filename(2);
          info(count).ROInumVox = voxelNb;
          info(count).isProblemBinarise = sum(img(img ~= 1));
          count = count + 1;

        end
      end

    case 4
        % do we want to smooth the masks a bit for having a bigger ROIs?

        % mask pattern to find
        maskPattern = ['task-', opt.taskName, ...
                       '_hemi.*',concatParcelName(1:end-4)];
                   
      %% decimalise it before smoothing

      for iSub = 1:length(opt.subjects)

        % get subject folder name
        subID = ['sub-', opt.subjects{iSub}];
  

                        
        maskToDecimal = spm_select('FPlist', ...
                            fullfile(parcelPath, subID), ...
                            ['^',maskPattern,'_mask.nii.*$']);

        for iMask = 1:size(maskToDecimal,1)

          % choose the mask to realign and reslice
          mask = deblank(maskToDecimal(iMask,:));

          % load the mask
          hdr = spm_vol(mask);
          img = spm_read_vols(hdr);

          % decimalise the mask
          hdr.dt = [16,0];

          % rename
          p = bids.internal.parse_filename(spm_file(mask, 'filename'));
          p.suffix = 'decimalised';
          p.use_schema = false;
          newName = bids.create_filename(p);
          
          % save new image 
          hdr.fname = spm_file(hdr.fname, 'filename', newName);
          spm_write_vol(hdr, img);

        end
      end

      %% smooth it
        for iSub = 1:length(opt.subjects)
            
            % get subject folder name
            subID = ['sub-', opt.subjects{iSub}];
        
            % find decimalised files to smooth
            
            maskToSmooth = cellstr(spm_select('FPlist', ...
                            fullfile(parcelPath, subID), ...
                            ['^',maskPattern, '*_decimalised.nii.*$']));
                        
            matlabbatch = [];
            matlabbatch = setBatchSmoothing(matlabbatch, ...
                                      maskToSmooth, ...
                                      opt.maskFWHM, ...
                                      prefixSmooth);
            spm_jobman('run', matlabbatch);
            
        end

      %% threshold & binarise the image
      count = 1;
      for iSub = 1:length(opt.subjects)

        % get subject folder name
        subID = ['sub-', opt.subjects{iSub}];

        % load smoothed decimal masks to threshold
        maskToThres = cellstr(spm_select('FPlist', ...
                            fullfile(parcelPath, subID), ...
                            ['^', prefixSmooth, maskPattern, ...
                            '*_decimalised.nii.*$']));
                        
        for iMask = 1:size(maskToThres,1)

          % load the mask - be careful it's cell now
          mask = deblank(maskToThres{iMask,:});

          % load the mask
          hdr = spm_vol(mask);
          img = spm_read_vols(hdr);

          % rename bids friendly
          p = bids.internal.parse_filename(mask);
          p.filename = p.filename(3:end);
          p.suffix = 'mask';
          p.entities.desc = ['dec_', prefixSmooth,...
                            '_thres', num2str(opt.threshold * 100)];
          p.use_schema = false;
          
          % need to get rid off s1 prefix
          p.entities = renameStructField(p.entities,[prefixSmooth,'task'], 'task');
          
          % here is the new name
          newBinariseName = bids.create_filename(p);

          % calculate the voxel num
          voxelNb = sum(img(:) > opt.threshold);

          % binarise it
          img(img(:) > opt.threshold) = 1;
          img(img(:) <= opt.threshold) = 0;
        
          % save
          hdr.fname = spm_file(hdr.fname, 'filename', newBinariseName);
          spm_write_vol(hdr, img);
          
          info(count).SUBname = subID;
          info(count).ROIname = newBinariseName;
          info(count).ROInumVox = voxelNb;
          info(count).ROInumVoxAfter = sum(img(:));
          count = count + 1;
          
        end
        
        % let's delete other decimalised maps
        maskToDelete = spm_select('FPlist', ...
                            fullfile(parcelPath, subID), ...
                            '^*_decimalised.nii.*$');
                        
        for iFile = 1:size(maskToDelete,1)
            fileName = deblank(maskToDelete(iFile,:));
            delete(fileName);
        end
                            
           
      end
  end

  %
  % count = 1;
  % for i=1:length(info)
  %
  %     a(i)=info(i).ROInumVox;
  %
  %     if mod(i,2) == 0
  %
  %         b(count) = a(i) + a(i-1);
  %         count = count + 1;
  %
  %     end
  % end
  %
  % a=sort(a);
  % bar(a);
  % disp ('Min VoxNb');
  % min(a)
  % disp ('Max VoxNb');
  % max(a)
  %
  % disp ('Min both hemisphere voxNb');
  % min(b)
  % disp ('Max both hemisphere voxNb');
  % max(b)

end

function [p, voxelNb, img] = saveBids(mask, maskPath, opt)

        % read the resliced image - to calculate the vox number
          hdr = spm_vol(mask);
          img = spm_read_vols(hdr);

          % binarise image if it's asked
          if opt.binarise.do
            img = img > opt.threshold;
          end
          
          % count the vox nb
          voxelNb = sum(img(:));
          
          % rename it with bids friendly struct
          p = bids.internal.parse_filename(spm_file(mask, 'filename'));
            
          p.entities = struct('task', opt.taskName, ...
                              'hemi', p.filename(2), ...
                              'space', 'individual', ...
                              'label', ['FS', p.suffix(3:end)]);
  
          p.suffix = 'mask';
          p.use_schema = false;
            
          newName = bids.create_filename(p);

          % save with new name
          movefile(mask, ...
                 fullfile(maskPath, newName));
end
