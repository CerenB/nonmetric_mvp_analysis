function [opt] = chooseMask(opt, roiSource)

% this is a switch case, according to the action, we choose with ROIs to be
% used

% action 1: contrast - MNI
% action 2: neurosynth - MNI
% action 3: freesurfer parcels (individual) of auditory cx and basal ganglia
% action 4: HMAT individual masks

switch lower(roiSource)
    
    
    case 'contraststgonly'
        
        opt.maskPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                                '..', '..', '..', 'RhythmCateg_ROI', roiSource);
        % masks to decode/use
        allMasks = dir(opt.maskPath);
        allMasks([allMasks.isdir]) = [];
        if allMasks(1).name =='.DS_Store'
            allMasks(1) = [];
        end
        
        for imask = 1:length(allMasks)
            opt.maskName(imask) = {allMasks(imask).name};
        end
        
        % use in output roi name
        opt.maskLabel = {'lSTG_10mm', 'lSTG_15mm', 'lSTG_20mm',  ...
                         'lSTG_nopitch_Block_p00001', 'lSTG_nopitch_Block_p0001', ...
                         'rSTG_nopitch_Block_p00001', 'rSTG_nopitch_Block_p0001', ...
                         'lSTG_nopitch_Nonmetric_p0001', 'rSTG_nopitch_Nonmetric_p0001', ...
                         'lSTG_pitch_Block_p005', 'lSTG_pitch_Block_p00001', ... 
                         'lSTG_pitch_Block_p0001', 'rSTG_pitch_Block_p005', ...
                         'rSTG_pitch_Block_p00001', 'lSTG_pitch_Block_p0001', ...
                         'lSTG_pitch_Nonmetric_p00001', 'lSTG_pitch_Nonmetric_p0001', ...
                         'rSTG_pitch_Nonmetric_p00001', 'rSTG_pitch_Nonmetric_p0001', ...
                         'rSTG_10mm', 'rSTG_15mm', 'rSTG_20mm'};
        
        
    case 'contrast'
        
        opt.maskPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                                '..', '..', '..', 'RhythmCateg_ROI', roiSource);
        % masks to decode/use
        opt.maskName = {'cerebellum_sphere-10mm_x-29_y--60_z--28.nii', ...
                        'lpreM_sphere-10mm_x--49_y--5_z-47.nii', ...
                        'rpreM_sphere-10mm_x-52_y-0_z-44.nii', ...
                        'lSTG_sphere-10mm_x--42_y--31_z-11.nii', ...
                        'rSTG_sphere-10mm_x-55_y--5_z-0.nii', ...
                        'sma_sphere-15mm_x--3_y-0_z-63.nii'
                        };
        
        % use in output roi name
        opt.maskLabel = {'cerebellum', 'lpreM', 'rpreM',  'lSTG', 'rSTG', 'SMA'};
        
%         opt.maskPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
%                                 '..', '..', '..', 'RhythmCateg_ROI', roiSource);
%         % masks to decode/use
%         opt.maskName = {'allSounds_Mask_p001.nii'};
%         
%         % use in output roi name
%         opt.maskLabel = {'AllSoundsContrast'};

        
        
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
        
    case 'jubrainatlas'
        
         opt.maskPath = fullfile(fileparts(mfilename('fullpath')), '..', ...
                                '..', '..', '..', 'RhythmCateg_ROI', ...
                                roiSource, 'derivatives');
                            
         % masks to decode/use
        allMasks = dir(opt.maskPath);
        allMasks([allMasks.isdir]) = [];
        if allMasks(1).name =='.DS_Store'
            allMasks(1) = [];
        end
        
        for imask = 1:length(allMasks)
            opt.maskName(imask) = {allMasks(imask).name};
        end
        
        % use in output roi name
        opt.maskLabel = {};
        
        
end


end