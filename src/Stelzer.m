function Stelzer

%%%%%%%%%%%%%%%%%%%%%
% notes

%%%%%%%%%%%%%%%%%%%%%
group = 'SC';

NrPermutations=100000;

mainPath = '/Users/cerenbattal/Cerens_files/fMRI/Processed/Spatiotopy/';
inputPath = fullfile(mainPath,'MVPA/results/Spheres/ConcatHemisphere/');
inputFileName = 'AllAccuracy_concathemisphere_LocVV5_LocPT_220vx_beta_permute1_20210510.mat';
outputPath = fullfile(inputPath, 'permutation');



load(fullfile(inputPath,inputFileName));

Allcondition = {'Motion4','LeftvsRight','UpvsDown','HorizontalvsVertical',...
                'Static4','SLeftvsRight','SUpvsDown','SHorizontalvsVertical'}; 

%Across-Axis
%'LeftvsUp','LeftvsDown','RightvsUp','RightvsDown',...
%    'sLeftvsUp','sLeftvsDown','sRightvsUp','sRightvsDown'

% Cross-Decoding
%'All','LeftvsRight','UpvsDown','HorizontalvsVertical'

% Normal Decoding
%'Motion4','LeftvsRight','UpvsDown','Static4','HorizontalvsVertical'
% 'LeftvsRight','UpvsDown','SLeftvsRight','SUpvsDown'

% Normal Decoding with Static conditions
%'SLeftvsRight','SUpvsDown','SHorizontalvsVertical'

% Motion vs Static Decoding:
%'LeftwardM>S','RightwardM>S','UpwardM>S','DownwardM>S','HorizontalvsVerticalM>S'

AllRoi = {'LR_locPT_6mm_mo', 'LR_locV5_6mm_2' };

%'SC_Mo_PT_left','SC_Mo_PT_right'
% 'rV5_tmaps_masked_rV512mm_p005_fwe','lV5_tmaps_masked_lV512mm_p005_fwe'

nsubEB = 16; 
nsubSC = 16;

for k = 1:length(Allcondition)
    for   j = 1:length(AllRoi)
        condition = Allcondition{k}
        ROI_Name = AllRoi{j}
        %change it for NS PT and V5 
        roi = [ROI_Name,'.nii'];
        %V5 '_2.nii' 
        %PT '.nii'
        %Parcels, no .nii ext needed; 
        %roi = [ROI_Name];

        if strcmp(group,'EB')
            
            % EB
            count = 1;
            for i = 1:length(accu)
                if strcmp(accu(i).roi,roi) && strcmp(accu(i).conditions,condition)
                    roi_accu(count).value = accu(i).value;
                    EB_acc_permutation(count,:) = accu(i).permutation*100;
                    count = count +1;
                end
            end
            

            EB_Accuracy =  [roi_accu(1:nsubEB).value]*100;
            
        elseif strcmp(group,'SC')
            % SC
            count = 1;
            for i = 1:length(accu)
                
                if strcmp(accu(i).roi,roi) && strcmp(accu(i).conditions,condition) %&& strcmp(accu(i).group,'EB')
                    roi_accu(count).value = accu(i).value;
                    SC_acc_permutation(count,:) = accu(i).permutation*100;
                    count = count +1;
                end
            end
            
            SC_Accuracy =  [roi_accu(nsubEB+1:nsubEB+nsubSC).value]*100;
        end
        %%
        p=1;
        while p<=NrPermutations                                        %Number of Repetitions of Sampling to have a Pooled Sample of Permutations
            
            if strcmp(group,'SC')
                for c=1:length(SC_Accuracy)                                           % From each Subject
                    SC_sample.(ROI_Name)(c)=datasample(SC_acc_permutation(c,:),1);     % Take a one sample from the permuted data
                end
                SC_Pooled_Perm.(ROI_Name)(1,p)=mean(SC_sample.(ROI_Name));      %Take the means of these samples for each model
                
            elseif strcmp(group,'EB')
                for c=1:length(EB_Accuracy)                                           % From each Subject
                    EB_sample.(ROI_Name)(c)=datasample(EB_acc_permutation(c,:),1);     % Take a one sample from the permuted data
                end
                EB_Pooled_Perm.(ROI_Name)(1,p)=mean(EB_sample.(ROI_Name));      %Take the means of these samples for each model
                
            end
            fprintf('Permutation : %.0f out of %.0f \n',p,NrPermutations)
            p=p+1;
        end
        
        disp('Done')
        
        if strcmp(group,'EB')
            save(fullfile(outputPath,...
                ['Perm_pooled_EB_220vx_',ROI_Name,'_',condition,'.mat']), ...
                'EB_Accuracy','ROI_Name','EB_Pooled_Perm');
        else
            save(fullfile(outputPath, ...
                ['Perm_pooled_SC_220vx_',ROI_Name,'_',condition,'.mat']), ...
                'SC_Accuracy','ROI_Name','SC_Pooled_Perm');
        end
        
        disp('mat File saved')
    end
end

end









