function [opt] = chooseMask(opt, roiSource)

% this is a switch case, according to the action, we choose with ROIs to be
% used

% action 1: contrast - MNI
% action 2: neurosynth - MNI
% action 3: freesurfer parcels (individual) of auditory cx and basal ganglia
% action 4: HMAT individual masks

switch lower(roiSource)
    
    case 'contrast'
        
        opt.maskPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                                '..', '..', '..', 'RhythmCateg_ROI', roiSource);
        % masks to decode/use
        opt.maskName = {'allSounds_Mask_p001.nii'};
        
        % use in output roi name
        opt.maskLabel = {'AllSoundsContrast'};
        
        
    case 'neurosnyth'
        
        opt.maskPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                                '..', '..', '..', 'RhythmCateg_ROI', ...
                                'neurosynth', 'functional', 'derivatives');
        
        % masks to decode/use
        opt.maskName = {'leftbin_rnativeThres_7_auditory_FDR_0.01.nii', ...
            'rightbin_rnativeThres_6_auditory_FDR_0.01.nii', ...
            'rrthres_7sma_FDR_0.01.nii', ...
            'leftrrthres_5premotor_FDR_0.01.nii', ...
            'rightbin_rnativeThres_5_premotor_FDR_0.01.nii'};
        
        % use in output roi name
        opt.maskLabel = {'leftAud', 'rightAud', 'SMA', 'leftPremotor', 'rightPremotor'};
        
        % maskName = {'leftrrthres_7premotor_FDR_0.01.nii', ...
        %            'rightrrthres_7premotor_FDR_0.01.nii',...
        %            'rrthres_10sma_FDR_0.01.nii'};
        % maskLabel = {'leftPremotor','rightPremotor', 'SMA'};
        
    case 'freesurfer'
        
        opt.maskPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                                '..', '..', '..', 'RhythmCateg_ROI', roiSource);
        
        % task name to be added
        opt.maskBaseName = ['task-', opt.taskName, '_'];
        
        % masks to decode/use
        opt.maskName = {'hemi-r_space-individual_label-FSauditorycx_desc-decS1Thres5_mask.nii', ...
            'hemi-l_space-individual_label-FSauditorycx_desc-decS1Thres5_mask.nii', ...
            'hemi-l_space-individual_label-FSbasalganglia_mask.nii', ...
            'hemi-r_space-individual_label-FSbasalganglia_mask.nii'};
        
        % use in output roi name
        opt.maskLabel = {'leftAud', 'rightAud', 'leftBG', 'rightBG'};
        
    case 'hmat'
        
        opt.maskPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                                '..', '..', '..', 'RhythmCateg_ROI', ...
                                roiSource, 'derivatives');

        % task name to be added
        opt.maskBaseName = ['task-', opt.taskName, '_'];
        
        % masks to decode/use
        opt.maskName = {'hemi-l_space-individual_label-HMATPMd_desc-inverseTransform_mask.nii', ...
            'hemi-l_space-individual_label-HMATpreSMA_desc-inverseTransform_mask.nii', ...
            'hemi-l_space-individual_label-HMATSMA_desc-inverseTransform_mask.nii', ...
            'hemi-r_space-individual_label-HMATPMd_desc-inverseTransform_mask.nii', ...
            'hemi-r_space-individual_label-HMATpreSMA_desc-inverseTransform_mask.nii', ...
            'hemi-r_space-individual_label-HMATSMA_desc-inverseTransform_mask.nii'};
        
        % use in output roi name
        opt.maskLabel = {'leftPremotor', 'leftpreSMA', 'leftSMA', ...
            'rightPremotor', 'rightpreSMA', 'rightSMA'};
        
        
end


end