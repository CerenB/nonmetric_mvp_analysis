library(ggplot2)
library(doBy)
library(cowplot)
library(Rmisc)
library(stringr)



#######################################################
#pathCosmoResults <- '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/RhythmBlock/data/derivatives/cosmoMvpa/neurosynth-freesurfer-2beta-diffVoxelNb'
#pathCosmoResults <- '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/RhythmBlock/data/derivatives/cosmoMvpa/contrast'

pathCosmoResults <- '/Users/battal/Dropbox/Work/CPPLab/Cerens_files_old/Result_sheets/'

########

mvpa <- NA

#dataNames <- paste(pathCosmoResults, '*.csv', sep ='/')
dataNames = "*20210524.csv"
temp = list.files(path = pathCosmoResults,pattern=dataNames)
csvFileNb <- length(temp)     

resultFiles = list()
for (i in 1:csvFileNb) {
  fileToRead = paste(pathCosmoResults, temp[i], sep ='/')
  x  = read.csv(fileToRead)
  x$FileID <- i
  resultFiles[i] = list(x)
}

# bind txt files using rbind comment
mvpa = do.call(rbind, resultFiles)

#####
### DECODING EB - SC PT-V5
#### order things a bit for easy manipulation for plotting

#make sure subjects are factor
mvpa$sub <-as.factor(mvpa$sub)

#make group order to call them in plot EB first
mvpa$group_order<-ifelse(mvpa$group == 'EB', 1, 2)
mvpa$group <-ifelse(mvpa$group == 'CONT', 'SC', 'EB')

#make roi order to call accordingly
mvpa$roi_order <- ifelse(mvpa$roi == 'lV5_6mm_2.nii', 1, 
                         ifelse(mvpa$roi == 'rV5_6mm_2.nii', 2, 
                                ifelse(mvpa$roi == 'lPT_6mm_mo.nii', 3,4)))

mvpa$roiName <- ifelse(mvpa$roi == 'lV5_6mm_2.nii', 'LeftV5', 
                         ifelse(mvpa$roi == 'rV5_6mm_2.nii', 'RightV5', 
                                ifelse(mvpa$roi == 'lPT_6mm_mo.nii', 'LeftPT','RightPT')))

#add motion or static condition
mvpa$isMotion <- ifelse(tolower(substring(mvpa$conditions, 1, 1)) == 's',0,1) 
mvpa$condition <- ifelse(tolower(substring(mvpa$conditions, 1, 1)) == 's','static','motion') 

# filter out some unnecassary columns
# filter out plane column 
mvpa[,6] <- NULL
mvpa[,6] <- NULL
mvpa[,7] <- NULL
mvpa[,7] <- NULL

# let's multiple accuracy with 100
mvpa$value <- mvpa$value * 100
mvpa.motion <- subset(mvpa, isMotion ==1)
mvpa.static <- subset(mvpa, isMotion == 0)

# take only 4 motion
mvpa.4MS <- subset(mvpa, conditions == 'Motion4' | conditions =='Static4')
# below does not work because it converts the second letter lower case, I want: SLeftvsRight
# mvpa.static$conditions <-str_to_title(mvpa.static$conditions)

# # take only 4 motion
# mvpa.4motion <-subset(mvpa.motion, conditions =='Motion4')
# mvpa.4static <-subset(mvpa.static, conditions =='Static4')

# summary stats
mvpa.4MS$condRoi <- paste(mvpa.4MS[,'condition'], mvpa.4MS[,'roiName'])
  
df <- summarySE(data = mvpa.4MS, 
                groupvars=c('group', 'group_order', 'condRoi','roiName', 'roi_order','condition'),
                measurevar='value')
df

setlimit = c(10,55) 
setbreak = c(10,20,30,40,50)

shapesize = 2
shapetype = 21
shapestroke = 1
transparent = 1 #0.6
jitter  = position_jitterdodge(0.2) # position_jitter(width=0.3)


fig <- ggplot(data = mvpa.4MS, 
              aes(x = reorder(condRoi,roi_order), 
                  y = value, 
                  color = group,
                  group = group)) +
  geom_point(data=mvpa.4MS,aes(x = reorder(condRoi,roi_order), y = value), size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  
  stat_summary(aes(color=group), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = value-se, ymax = value+se, group = reorder(group, group_order)), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +


  geom_hline(yintercept=c(25), linetype="dotted", colour="black", size=.5) +
  ggtitle("") +
  ylab("Decoding Accuracy (%)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c("Moving","Static","Moving","Static","Moving","Static","Moving","Static"))+
  # theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("purple","gray")) +
  theme(legend.position="none")
fig

filename <- paste(pathCosmoResults, 'Decoding_4Motion4Static_EBSC.png', sep = '')

# ggsave(filename, fig, dpi=300, width=8, height=2.4)

ggsave(filename, fig, dpi=300, width=4.5, height=3)

#### add fonts for ggpplot. 
filename <- paste(pathCosmoResults, 'Decoding_4Motion4Static_EBSC.pdf', sep = '')
ggsave(filename, fig, dpi=300, width=8, height=2.4)






#######

# audBG <- read.csv(paste(pathCosmoResults, 'RhythmBlockDecoding_freesurfer_s2_20210303.csv', sep ='/'))
# 
# motor <- read.csv(paste(pathCosmoResults, 'RhythmBlockDecoding_neurosnyth_s2_20210303.csv', sep ='/'))
# 
# BG <- read.csv(paste(pathCosmoResults, 'RhythmBlockDecoding_BG_s0_vx520_20210308.csv', sep ='/'))
#   
# mvpa <- BG
# 
# mvpa <- rbind(audBG, motor)

# mvpa <- mvpa[-c(7:9)]
mvpa$subID <-as.factor(mvpa$subID)


mvpa$roi_order <- ifelse(mvpa$mask == 'leftAud', 1, 
                         ifelse(mvpa$mask == 'rightAud', 2, 
                                ifelse(mvpa$mask == 'leftBG', 3, 
                                       ifelse(mvpa$mask == 'rightBG',4,
                                              ifelse(mvpa$mask == 'leftPremotor', 5,
                                                     ifelse(mvpa$mask == 'rightPremotor',6,7))))))
mvpa$roi_color_code <- ifelse(mvpa$mask == 'leftAud', 1, 
                         ifelse(mvpa$mask == 'rightAud', 1, 
                                ifelse(mvpa$mask == 'leftBG', 2, 
                                       ifelse(mvpa$mask == 'rightBG',2,
                                              ifelse(mvpa$mask == 'leftPremotor', 3,
                                                     ifelse(mvpa$mask == 'rightPremotor',3,4))))))

mvpa$mask <- ifelse(mvpa$mask == 'leftAud', 'audL', 
                         ifelse(mvpa$mask == 'rightAud', 'audR', 
                                ifelse(mvpa$mask == 'leftBG', 'bgL', 
                                       ifelse(mvpa$mask == 'rightBG','bgR',
                                              ifelse(mvpa$mask == 'leftPremotor', 'preL',
                                                    ifelse(mvpa$mask == 'rightPremotor','preR','SMA'))))))
# sma roi consist of left/right hemispheres but for plotting 
# we can show under the same vx size as the others
# better way would be dividing the masks an re-run the decoding
mvpa$voxelToPlot <- mvpa$choosenVoxNb
#mvpa$voxelToPlot<- ifelse(mvpa$mask == 'SMA', mvpa$voxelToPlot/2, mvpa$voxelToPlot)

# ==============================================================================
# summary stats
df <- summarySE(data = mvpa, 
                groupvars=c('mask', 'roi_order', 'image','ffxSmooth','voxelToPlot','roi_color_code'),
                measurevar='accuracy')

#################
pd <- position_dodge(0.1)

filename <- paste(pathCosmoResults, '/plot/', 'Decoding_SimpleVsComplex-1Beta.png', sep = '')
title <- paste('Simple vs Complex Rhythm Decoding ')


fig <- ggplot(df, aes(x = reorder(mask, roi_order), y = accuracy)) + 
  geom_point(data = mvpa, aes(group=subID), pos=pd, size=2, color=grey(0.8)) + 
  geom_point(size=2,col='black') + 
  geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
  facet_grid(vars(image,ffxSmooth), vars(voxelToPlot)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),size=1,width=0.2) + 
  scale_y_continuous(limits=c(0, .90)) +
  xlab('ROIs')+
  ylab('classification acc.')+
  ggtitle(title)+
  theme_cowplot(font_size = 12,
                font_family = "",
                line_size = 0.3,
                rel_small = 6/12,
                rel_tiny = 9/12,
                rel_large = 14/12)


fig

ggsave(filename, device="png", units="in", width=18, height=9.08, dpi=300)  


######
# a quick look for significance
chance = 0.5
iImage = 't_maps'
iVoxelNb = 150 # 100 150
iSmoothing = 2
roi = 'audR' #preR, preL, audL, audR, SMA, bgR, bgL

dataAccuracy = subset(mvpa, image == iImage & choosenVoxNb == iVoxelNb & ffxSmooth == iSmoothing & mask ==roi)
accuracyVector = dataAccuracy$accuracy - chance
res = t.test(accuracyVector)
res

#### not assuming normality - let's do non-parametric test

res = wilcox.test(accuracyVector)
Za = qnorm(res$p.value/2)
res




######## only for 1 contrast = MASK = ROI ##################
# only for 1 big roi from the allsounds vs. rest contrast:
mvpa$roi_order <- 1

df <- summarySE(data = mvpa, 
                groupvars=c('mask', 'roi_order', 'image','ffxSmooth','voxelToPlot'),
                measurevar='accuracy')



filename <- paste(pathCosmoResults, '/plot/', 'Decoding_SimpleVsComplex-1Beta.png', sep = '')
title <- paste('Simple vs Complex Rhythm Decoding ')


fig <- ggplot(df, aes(x = reorder(mask, roi_order), y = accuracy)) + 
  geom_point(data = mvpa, aes(group=subID), pos=pd, size=2, color=grey(0.8)) + 
  geom_point(size=2,col='black') + 
  geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
  facet_grid(vars(image,ffxSmooth), vars(voxelToPlot)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),size=1,width=0.2) + 
  scale_y_continuous(limits=c(0, .90)) +
  ylab('classification acc.')+
  xlab('ROIs')+
  ggtitle(title)+
  theme_cowplot(font_size = 14,
                font_family = "",
                line_size = 0.3,
                rel_small = 10/12,
                rel_tiny = 10/12,
                rel_large = 14/12)


fig





###### figure for best ratio for decoding
#################################### destroy afterwards
# summary stats
df <- summarySE(data = mvpa,
                groupvars=c('mask', 'roi_order', 'image','ffxSmooth'),
                measurevar='accuracy')


#################
pd <- position_dodge(0.1)

filename <- paste(pathCosmoResults, '/plot/', 'Decoding_SimpleVsComplex-AllCondition-Ratio95.png', sep = '')
title <- paste('Simple vs Complex Rhythm Decoding ')


fig <- ggplot(df, aes(x = reorder(mask, roi_order), y = accuracy)) +
  geom_point(data = mvpa, aes(group=subID), pos=pd, size=2.5, color=grey(0.8)) +
  geom_point(size=3,col='black') +
  geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
  facet_grid(vars(image,ffxSmooth)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),size=1,width=0.2) +
  scale_y_continuous(limits=c(0, .90)) +
  xlab('ROIs')+
  ylab('classification acc.')+
  ggtitle(title)+
  theme_cowplot(font_size = 14,
                font_family = "",
                line_size = 0.3,
                rel_small = 10/12,
                rel_tiny = 10/12,
                rel_large = 14/12)


fig

ggsave(filename, device="png", units="in", width=9, height=9.08, dpi=300)










########
# with for loop
# filterSmoothing <- c('0','2')
# filterImage <- c('beta', 't_maps')
# filterVoxelNb <- c('100','150','250','400','520')

# for (iImage in filterImage) {
#   
#   
#   for (iSmoothing in filterSmoothing) {
#     
#     for (iVoxelNb in filterVoxelNb) {
#       
#       print(paste(iImage, iSmoothing, iVoxelNb))
#       
#       title <- paste('SimplevsComplex - ',iImage,'s',iSmoothing, 'voxel', iVoxelNb)
#       
#       filename <- paste(pathCosmoResults, '/plot/', 'simpleVscomplex-', iImage, '_s', iSmoothing,'_', iVoxelNb,'_Vx.png', sep = '')
#       
#       fig <- ggplot(subset(df, image == iImage & choosenVoxNb == iVoxelNb & ffxSmooth == iSmoothing), aes(x = reorder(mask, roi_order), y = accuracy)) + 
#         geom_point(data = subset(mvpa, image == iImage & iVoxelNb == choosenVoxNb & iSmoothing == ffxSmooth), aes(group=subID), pos=pd, size=3, color=grey(0.8)) + 
#         geom_point(size=4,col='black') + 
#         geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
#         geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),size=1,width=0.2) + 
#         scale_y_continuous(limits=c(0, .90)) +
#         xlab('ROIs')+
#         ylab('classification acc.')+
#         ggtitle(title)+
#         theme_classic() +
#         theme(
#           text=element_text(size=16),
#           legend.position = 'right',
#           legend.text=element_text(size=14),
#           axis.line = element_line(size = 0.6),
#           axis.text.x = element_text(size=14,colour="black"),
#           axis.text.y = element_text(size=14, colour='black'))
#       ggsave(filename, device="png", units="in", width=9, height=4.54, dpi=300)  
#     }
#   }
#   
# }



