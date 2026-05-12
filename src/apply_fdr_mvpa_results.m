function results = apply_fdr_mvpa_results(useFamilies)
% Apply FDR correction to existing nonmetric permutation results.
%
% Syntax:
%   results = apply_fdr_nonmetric_results()       % Uses default (useFamilies = false)
%   results = apply_fdr_nonmetric_results(true)   % Apply FDR per ROI family
%   results = apply_fdr_nonmetric_results(false)  % Apply FDR across all ROIs
%
% Reads:
%   PermutedAccuracy_nonmetric_groupXXX_results.mat
% Writes:
%   PermutedAccuracy_nonmetric_groupXXX_results_fdr.mat
%
% Configuration:
%   useFamilies (bool): if true, FDR is applied per ROI family within each dataset/group/condition.
%                       if false, FDR is applied across all ROIs within each dataset/group/condition.
%                       Default: false
%
% ROI family rules (when useFamilies=true):
% - lpreM and rpreM together
% - lSTG and rSTG together
% - SMA separate
% - rcereb separate

if nargin < 1
    useFamilies = false;  % Default: FDR across all ROIs, no family grouping
end

clearvars -except results useFamilies;
close all;

resultsPath = '/Volumes/extreme/Cerens_files/fMRI/RhythmCateg/Nonmetric/derivatives/cosmoMvpa/';
% resultsPath = '/Volumes/extreme/Cerens_files/fMRI/RhythmCateg/RhythmBlock/derivatives/cosmoMvpa/';
inputPath = fullfile(resultsPath, 'permutation');

if useFamilies
    modeSuffix = '_fdr_families';
else
    modeSuffix = '_fdr_allrois';
end

% Find the input file matching the pattern using dir()
filePattern = fullfile(inputPath, 'PermutedAccuracy_*_group*.mat');
matches = dir(filePattern);
matches = matches(~contains({matches.name}, '_fdr'));
if isempty(matches)
    error('No input file found matching pattern: %s', filePattern);
end
inputFile = fullfile(inputPath, matches(1).name);
baseFileName = strrep(matches(1).name, '.mat', '');
outputFile = fullfile(inputPath, [baseFileName, modeSuffix, '.mat']);

S = load(inputFile);
if ~isfield(S, 'results')
    error('Input file does not contain ''results'': %s', inputFile);
end

results = S.results;

if isempty(results)
    warning('results is empty. Saving unchanged output.');
    save(outputFile, '-struct', 'S');
    return;
end

for i = 1:numel(results)
    results(i).p_value_fdr = NaN;
end

datasets = unique({results.dataset}, 'stable');
for iDataset = 1:numel(datasets)
    datasetLabel = datasets{iDataset};

    datasetIdx = strcmp({results.dataset}, datasetLabel);
    groupsInDataset = unique({results(datasetIdx).group}, 'stable');

    for iGroup = 1:numel(groupsInDataset)
        groupLabel = groupsInDataset{iGroup};

        groupIdx = datasetIdx & strcmp({results.group}, groupLabel);
        conditionsInGroup = unique({results(groupIdx).condition}, 'stable');

        for iCond = 1:numel(conditionsInGroup)
            conditionLabel = conditionsInGroup{iCond};

            baseIdx = groupIdx & strcmp({results.condition}, conditionLabel);
            baseLinear = find(baseIdx);

            if useFamilies
                % FDR correction per ROI family
                roiNames = {results(baseLinear).roi};
                roiFamilies = cellfun(@get_roi_family, roiNames, 'UniformOutput', false);
                uniqueFamilies = unique(roiFamilies, 'stable');

                for iFam = 1:numel(uniqueFamilies)
                    famLabel = uniqueFamilies{iFam};
                    famMaskLocal = strcmp(roiFamilies, famLabel);
                    famLinearIdx = baseLinear(famMaskLocal);

                    pRaw = [results(famLinearIdx).p_value];
                    pAdj = fdr_correct_pvalues(pRaw);

                    for iEntry = 1:numel(famLinearIdx)
                        results(famLinearIdx(iEntry)).p_value_fdr = pAdj(iEntry);
                    end
                end
            else
                % FDR correction across all ROIs (no family grouping)
                pRaw = [results(baseLinear).p_value];
                pAdj = fdr_correct_pvalues(pRaw);

                for iEntry = 1:numel(baseLinear)
                    results(baseLinear(iEntry)).p_value_fdr = pAdj(iEntry);
                end
            end
        end
    end
end

S.results = results;
save(outputFile, '-struct', 'S');

fprintf('Saved FDR-corrected results to: %s\n', outputFile);

end

function familyLabel = get_roi_family(roiName)
roiLower = lower(roiName);

if strcmp(roiLower, 'lprem') || strcmp(roiLower, 'rprem')
    familyLabel = 'prem';
elseif strcmp(roiLower, 'lstg') || strcmp(roiLower, 'rstg')
    familyLabel = 'stg';
elseif strcmp(roiLower, 'sma')
    familyLabel = 'sma';
elseif strcmp(roiLower, 'rcereb')
    familyLabel = 'rcereb';
else
    % Any unexpected ROI is corrected separately by treating it as its own family.
    familyLabel = ['single_' roiLower];
end
end

function pAdj = fdr_correct_pvalues(pValuesRaw)
% Benjamini-Hochberg FDR correction.
% Input: pValuesRaw (vector of raw p-values)
% Output: pAdj (vector of FDR-corrected p-values, same shape as input)
pRaw = pValuesRaw(:);
pAdj = NaN(size(pRaw));

validMask = ~isnan(pRaw);
if ~any(validMask)
    return;
end

p = pRaw(validMask);
m = numel(p);
[pSorted, sortIdx] = sort(p, 'ascend');
rank = (1:m)';

pSortedAdj = pSorted .* (m ./ rank);
pSortedAdj = min(pSortedAdj, 1);
pSortedAdj = flipud(cummin(flipud(pSortedAdj)));

pAdjValid = NaN(size(p));
pAdjValid(sortIdx) = pSortedAdj;
pAdj(validMask) = pAdjValid;

if isrow(pValuesRaw)
    pAdj = pAdj.';
end
end
