clear all;
close all;

pValues = struct('condition', [],'roi', [],'p_value', []);

conditions = {'Motion4','LeftvsRight','UpvsDown','HorizontalvsVertical',...
                'Static4','SLeftvsRight','SUpvsDown','SHorizontalvsVertical'};

rois = {'LR_locPT_6mm_mo', 'LR_locV5_6mm_2' };


groups = {'EB','SC'};


count = 1;
nbins = 20;
permutationsNb=100000;

mainPath = '/Users/cerenbattal/Cerens_files/fMRI/Processed/Spatiotopy/';
inputPath = fullfile(mainPath,'MVPA/results/Spheres/ConcatHemisphere/permutation');


for iGroup = 1:length(groups)
    
    groupLabel = groups{iGroup};
    
    for iCondition = 1:length(conditions)
        
        for   iRoi = 1:length(rois)
            
            condition = conditions{iCondition}
            ROI_Name = rois{iRoi}
            roi = ROI_Name;
            
            fileToLoad = [inputPath,'/Perm_pooled_',groupLabel,'_220vx_',ROI_Name,'_',condition,'.mat'];
            load(fileToLoad);
            
            % calculate the mean/std
            if strcmp(groupLabel, 'EB')
                
                valuesToPlot = struct2cell(EB_Pooled_Perm);
                
                %The mean of our results
                mu = mean(EB_Accuracy); 
                stderr = std(EB_Accuracy)/ sqrt(length(EB_Accuracy));
            else
                
                valuesToPlot = struct2cell(SC_Pooled_Perm);
                
                %The mean of our results
                mu = mean(SC_Accuracy); 
                stderr = std(SC_Accuracy)/ sqrt(length(SC_Accuracy));
            end
            
            %% plot the hist
            % titleme1 = ['Permutation ', group, ' ', ROI_Name,' ', condition];
            permu = valuesToPlot{1};
            %
            % h= figure;
            % hist(permu,nbins)
            %
            %
            % %Overlay the mean
            % hold on
            % plot([mu,mu],ylim,'r','LineWidth',2)
            % hold off
            % % Add title and axis labels
            % title(titleme1)
            % xlabel('Accuracies')
            % ylabel('Number of permutations')
            
            %% calculate p-value
            %what is the prob of having result equal/higher than my results
            observations = length(permu(permu >= mu));
            p_value = ( observations + 1) /(permutationsNb +1);
            
            pValues(count).condition = condition;
            pValues(count).roi = ROI_Name;
            pValues(count).p_value = p_value;
            pValues(count).mean = mu;
            pValues(count).stderr = stderr;
            pValues(count).group = groupLabel;
            
            %savename_fig = ['SC_permu_', condition,'_',ROI_Name,'.fig'];
            % savename_fig = ['EB_permu_', condition,'_',ROI_Name,'.fig'];
            % savefig(h,savename_fig)
            %save([group, '_', 'pvalues_',condition,'_',ROI_Name, '.mat'],'PValues');
            
            %close(h)
            
            count = count +1;
        end
    end
end
save(fullfile(inputPath,'PermutedAccuracy_LocV5PT_220vx_pvalues.mat'),'pValues');