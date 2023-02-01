function [acc0] = permuteAccuracy(ds,accuracy, partitions, opt)                


%number of iterations
niter = 100;

% allocate space for permuted accuracies
acc0 = zeros(niter,1); 

 % make a copy of the dataset
ds0 = ds;

%for niter iterations, reshuffle the labels and compute accuracy
% Use the helper function cosmo_randomize_targets
for k=1:niter
    
    ds0.sa.targets=cosmo_randomize_targets(ds);
    [~, acc0(k)]=cosmo_crossvalidate(ds0, @cosmo_classify_meta_feature_selection,...
        partitions,opt);
end

p = sum(accuracy<acc0)/niter;
fprintf('%d permutations: accuracy=%.3f, p=%.4f\n', niter, accuracy, p);
                 
end