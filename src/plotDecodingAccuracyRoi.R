rm(list=ls()) #clean console


library(ggplot2)
library(doBy)
library(cowplot)
library(Rmisc)
library(stringr)

# analysis libraries
library(rstatix)
library(dplyr)
library(car)

#library(afex)
library(ez)
library(schoRsch)
#######################################################

# pathResults <- '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/Nonmetric/derivatives/cosmoMvpa/'
pathResults <- '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/RhythmBlock/rhythmBlock_derivatives_cosmoMvpa/'

########
mvpa <- read.csv(paste(pathResults, 'RhythmBlockDecoding_contrastSTGOnly_s2_ratio120_202302011234.csv', sep ='/'))
# RhythmBlockDecoding_contrast_s2_ratio120_202302011140
# RhythmBlockDecoding_contrastSTGOnly_s2_ratio120_202302011234

# NonmetricDecoding_neurosnyth_s2_ratio150_202201192323
# NonmetricDecoding_neurosnyth_s2_ratio300_202201192258
# NonmetricDecoding_contrast_s2_ratio120_202301241603
mvpa <- mvpa[-c(7:9)]
mvpa$subID <-as.factor(mvpa$subID)

head(mvpa)

voxelSize = '120'

#### order things a bit for easy manipulation for plotting


#name change from mask to roi
mvpa$mask <-as.factor(mvpa$mask)
names(mvpa)[2] <- 'roi'

# let's make a expType to split the no_pitch exp from pitch exp
mvpa$subNb <- as.numeric(mvpa$subID)
mvpa$expType<- ifelse(mvpa$subNb < 23, 'P4', 'P1')
mvpa$expType <-as.factor(mvpa$expType)

#make roi order to call accordingly
mvpa$roi_order <- ifelse(mvpa$roi == 'lSTG', 1,
                          ifelse(mvpa$roi == 'rSTG', 2, 
                                 ifelse(mvpa$roi == 'SMA', 3, 
                                        ifelse(mvpa$roi == 'lpreM', 4,
                                               ifelse(mvpa$roi == 'rpreM', 5,6)))))

# currently we  don't have 2 hemispheres for every ROI
# later on consider this separation for "Aud Cx only" analysis
mvpa$hemis <- ifelse(substr(mvpa$roi,1,1) == 'l', 'left',
                     ifelse(substr(mvpa$roi,1,1) == 'r', 'right',NA))

# make everything factor
str(mvpa)
mvpa$image<- as.factor(mvpa$image)
mvpa$subType<-as.factor(mvpa$subType)
mvpa$subTypeLabel<- as.factor(mvpa$subTypeLabel)
mvpa$hemis<-as.factor(mvpa$hemis)

# subset the dataframe for plotting/analysis
img = 'beta' # or 't_maps'
exp = 'P4' 

subsetmvpa = subset(mvpa,expType == exp)
subsetmvpa = subset(subsetmvpa, image == img)

str(subsetmvpa)

df <- summarySE(data = subsetmvpa, 
                groupvars=c('roi_order','roi', 'subType'),
                measurevar='accuracy', na.rm = TRUE)
df



######### analyze the data ######### 
results = leveneTest(accuracy ~ roi, subsetmvpa)
results

# test normality 
subsetmvpa %>%
  group_by(roi) %>%
  shapiro_test(accuracy)

# ANOVA IS a bit useless in our case 
# # anova -with ez package
# anova1 <- ezANOVA(data=subsetmvpa, 
#                 dv=.(accuracy), 
#                 wid=.(subID), 
#                 within =.(roi), 
#                 detailed=TRUE, 
#                 type=3) #
# 
# anova1
# anova_out(anova1)
# 
# # lmm from afex package
# m1 <- mixed(accuracy ~  roi * hemis * subType + (1|subID), data = subsetmvpa)
# m1
# 
# m2 <- mixed(accuracy ~  roi + (1|subID), data = subsetmvpa)
# m2


# let's do t-test 

# separate for ROIs
head(subsetmvpa)
#subsetmvpa$accuracy <- subsetmvpa$accuracy + 50

lSTG <-subset(subsetmvpa, roi =='lSTG')
rSTG <-subset(subsetmvpa, roi =='rSTG')
SMA <-subset(subsetmvpa, roi =='SMA')
lpreM <-subset(subsetmvpa, roi =='lpreM')
rpreM <-subset(subsetmvpa, roi =='rpreM')
cerebellum <-subset(subsetmvpa, roi =='cerebellum')


# sig dif than zero?
t.test(lSTG$accuracy, mu = 0.5, alternative = 'greater') # t = 0.19825, df = 9, p-value = 0.4236
t.test(rSTG$accuracy, mu = 0.5, alternative = 'greater') # t = 0.58585, df = 9, p-value = 0.2862
t.test(lpreM$accuracy, mu = 0.5, alternative = 'greater') # t = 1.2451, df = 9, p-value = 0.1223
t.test(rpreM$accuracy, mu = 0.5, alternative = 'greater') # t = 0, df = 9, p-value = 0.5
t.test(SMA$accuracy, mu = 0.5, alternative = 'greater')  # t = 0.73855, df = 9, p-value = 0.2395
t.test(cerebellum$accuracy, mu = 0.5, alternative = 'greater') # t = 0.41917, df = 9, p-value = 0.3425



############# PLOTTING  #############

min(subsetmvpa$accuracy, na.rm = TRUE)
max(subsetmvpa$accuracy, na.rm = TRUE)

setlimit = c(0,1) 
setbreak = c(0,0.25, 0.5, 0.75, 1)


rois = '6ROI'

shapesize = 1
shapetype = 21
shapestroke = 1
transparent = 1 #0.6
jitter  = position_jitterdodge(0.3) # position_jitter(width=0.3)



# colors 
nonmetricGrayBad = "#9ec5aa" # complex green= 3d8c55ff, nonmetricGrap = 6B6B6B
nonmetricGrayGood = "#3d8c55ff" 
simplePurpleBad = "#c4a4c9"
simplePurpleGood = "#8a4a95"
# conditions
category2 = ''
category1 = ''

cond1= paste0(category2," Bad Tapper")
cond2 = paste0(category2,' Good Tapper')
cond3 = paste0(category1,' Bad Tapper')
cond4 = paste0(category1,' Good Tapper')

cond = 'Complex'

##### separate tappers
fig <- ggplot(data = subsetmvpa, 
              aes(x = reorder(roi, roi_order),
                  y = accuracy, 
                  color = subType,
                  group = subType)) +
  geom_point(data=subsetmvpa, 
             aes(x = reorder(roi, roi_order), 
                 y = accuracy), 
             size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke, na.rm = TRUE) + 
  stat_summary(aes(color = subType), fun=mean, fun.min = mean, fun.max = mean, 
               geom="crossbar", size=0.6, width=0.6, position = position_dodge(width=.75), 
               na.rm = TRUE) +
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = accuracy-se, ymax = accuracy+se, group = subType), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  geom_hline(yintercept=c(.5), linetype="dotted", colour="black", size=.5) +
  
  ggtitle("") +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=12, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  # scale_x_discrete(labels = NULL )+
  scale_color_manual(name = '', labels = c(cond1, cond2), values=c(nonmetricGrayBad, nonmetricGrayGood)) + 
  # theme(legend.position= "none")
  theme(legend.position= c(.85, .85)) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.title=element_text(size=9))
fig

filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,  'voxelNb-', voxelSize, '_', rois, 'tappers.png')
# ggsave(filename, fig, dpi=300, width=15, height=6, units='cm') 
ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512


#######################################################
########## ignore tappers #######################################################
#######################################################
df2 <- summarySE(data = subsetmvpa, 
                groupvars=c('roi_order','roi'),
                measurevar='accuracy', na.rm = TRUE)
df2

fig <- ggplot(data = subsetmvpa, 
              aes(x = reorder(roi, roi_order),
                  y = accuracy),
              color = expType) +
  geom_jitter(size = shapesize, shape = shapetype, stroke = shapestroke, width=0.1, color = nonmetricGrayGood, 
              na.rm = TRUE) +
  stat_summary(aes(color = expType), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.3,
               na.rm = TRUE) +
  theme_classic() +
  geom_errorbar(data = df2, 
                aes(ymin = accuracy-se, ymax = accuracy+se), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75),
                na.rm = TRUE) +
  geom_hline(yintercept=c(.5), linetype="dotted", colour="black", size=.5) +
  
  ggtitle("") +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=12, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  # scale_x_discrete(labels = NULL )+
  scale_color_manual(name = '', labels =c('Simple vs. Complex'), values=c(nonmetricGrayGood)) + #
  theme(legend.position= c(.85, .9)) +
  theme(legend.text=element_text(size=9)) +
  theme(legend.title=element_text(size=9))
fig

filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,  'voxelNb-', voxelSize,  '_', rois, '.png')
ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512












##############################################################################################################
################################################################################################################
# OLDER VERSION

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
                groupvars=c('mask', 'roi_order', 'image','voxelToPlot','roi_color_code'),
                measurevar='accuracy')
df

#################
pd <- position_dodge(0.1)

filename <- paste(pathCosmoResults, '/plot/', 'Decoding_SimpleVsNonmetric-1Beta.png', sep = '')
title <- paste('Simple vs Nonmetric Rhythm Decoding ')


fig <- ggplot(df, aes(x = reorder(mask, roi_order), y = accuracy)) + 
  geom_point(data = mvpa, aes(group=subID), pos=pd, size=2, color=grey(0.8)) + 
  geom_point(size=2,col='black') + 
  geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
  facet_grid(vars(image), vars(voxelToPlot)) +
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
roi = 'preL' #preR, preL, audL, audR, SMA, bgR, bgL

dataAccuracy = subset(mvpa, image == iImage & choosenVoxNb == iVoxelNb & mask ==roi)
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



