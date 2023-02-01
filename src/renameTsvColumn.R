rm(list=ls()) #clean console
library(tidyverse)

# string separator lib
library(stringr)
# let's read tsv files and reorganize the trial_type so each repetition of 
# a condition would be labeled differently and thus we can model them in 
# GLM as separated regressors == separated betas

pathToFunc <- '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/RhythmBlock/code/rhythmBlock_fMRI_analysis/lib/bids-R/bidsr_queryEvents.R'
source(pathToFunc)


bidsRoot <- '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/Nonmetric/derivatives/cpp_spm/sub-015' 
taskName <- 'Nonmetric' 

taskEventsFiles <- bidsr_queryEvents(bidsRoot = bidsRoot, 
                                     taskName = taskName)
# 
# # for loop to make multiple regressors of 1 condition (1 repetition = 1 regressor)
# for (i in 1:length(taskEventsFiles)) {
#   
#   tsv <- read.table(paste(bidsRoot, taskEventsFiles[i], sep = '/'), header = TRUE)
#   
#   # if it is simple_block or complex_block, rewrite it with "simple_block_stepNum"
#   tsv$trial_type <- ifelse(tsv$trial_type == 'block_simple' | tsv$trial_type == 'block_complex', 
#                             paste(tsv$trial_type, tsv$stepNum, sep = '_'), tsv$trial_type)
#   
#   write.table(tsv,
#               paste(bidsRoot, taskEventsFiles[i], sep = '/'),
#               row.names = FALSE,
#               sep = '\t',
#               quote = FALSE)
#   
# }

################################################
##### reverse the .events.tsv files ###########

# # for loop to make 1 regressor of 1 condition (all repetition = 1 regressor)
# for (i in 1:length(taskEventsFiles)) {
#   
#   tsv <- read.table(paste(bidsRoot, taskEventsFiles[i], sep = '/'), header = TRUE)
#   
#   # if it is simple_block or complex_block, rewrite it, otherwise kept it same
#   simpleName <- paste('block_simple', tsv$stepNum, sep = '_') 
#   complexName <- paste('block_complex', tsv$stepNum, sep = '_')
#   
#   tsv$trial_type <- ifelse(tsv$trial_type == simpleName | tsv$trial_type == complexName, 
#                           substr(tsv$trial_type,1,nchar(tsv$trial_type)-2), tsv$trial_type)
#   
#   write.table(tsv,
#               paste(bidsRoot, taskEventsFiles[i], sep = '/'),
#               row.names = FALSE,
#               sep = '\t',
#               quote = FALSE)
#   
# }
# 


################################################
##### make pitch+block .events.tsv files ###########

# NOTE: number of rows to read from tsv file is FIXED to 40 ! 

# for loop to make 1 regressor of 1 condition (all repetition = 1 regressor)
for (i in 1:length(taskEventsFiles)) {
  
  tsv <- read.table(paste(bidsRoot, taskEventsFiles[i], sep = '/'), nrows = 40, header = TRUE)
  temp <- read.table(paste(bidsRoot, taskEventsFiles[i], sep = '/'), nrows = 40, header = TRUE)
  
  # control - only take first 40 rows in temp / tsv
  # organise the tsv columns
  temp$F0<-round(temp$F0)
  temp$duration <- 2.28
  
  temp$trial_type <- ifelse(temp$trial_type == 'block_simple' | temp$trial_type == 'block_nonmetric', 
                            str_sub(temp$trial_type,7), temp$trial_type)
  
  temp$trial_type <- paste('pitch',  temp$F0, sep = '_')
  
  # remove unnecessary info
  col<- colnames(temp)
  colNA<- col[4:length(col)]
  temp[colNA] <-"n/a"
  
  # combine 2 tsv files
  tsv <-rbind(tsv,temp)
  
  write.table(tsv,
              paste(bidsRoot, taskEventsFiles[i], sep = '/'),
              row.names = FALSE,
              sep = '\t',
              quote = FALSE)
  
}