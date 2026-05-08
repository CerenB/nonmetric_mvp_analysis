rm(list=ls()) #clean console

library(ggplot2)
library(doBy)
library(cowplot)
library(Rmisc)
library(stringr)

library(gghalves)  # For half violin plot
library(extrafont) # for avenir font

# analysis libraries
library(rstatix)
library(dplyr)
library(car)

library(ez) # anova
library(schoRsch)

library(afex) # mixed models
#######################################################

pathResults <- '/Volumes/extreme/Cerens_files/fMRI/RhythmCateg/Nonmetric/derivatives/cosmoMvpa/'

# pathResults <- '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/Nonmetric/derivatives/cosmoMvpa/'
# pathResults <- '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/RhythmBlock/rhythmBlock_derivatives_cosmoMvpa/'

########
mvpa <- read.csv(paste(pathResults, 'NonmetricDecoding_contrast_s2_ratio150_202405311111.csv', sep ='/'))

# UPDATE on 21/03/2025 
# organising here and there
# minor modification in the plot 
#
# RhythmBlockDecoding_contrast_s2_ratio150_202405311129 # used on 31.05.2024 plots - seems good for manuscript
# RhythmBlockDecoding_grahn2007_s2_ratio150_202405281524 # add these to supplementary

# NonmetricDecoding_contrast_s2_ratio150_202405311111 # used on 31.05.2024 plots - seems good for manuscript
# NonmetricDecoding_grahn2007_s2_ratio150_202405221549 # add these to supplementary



# RhythmBlockDecoding_jubrainatlas_s2_ratio150_202305091421 # add these to supplementary ?? 
# NonmetricDecoding_jubrainatlas_s2_ratio150_202303151641 # add these to supplementary ?? 




# RhythmBlockDecoding_contrast_s2_ratio120_202302011140
# RhythmBlockDecoding_contrastSTGOnly_s2_ratio120_202302011234
# NonmetricDecoding_contrastSTGOnly_s2_ratio120_202301251403
# NonmetricDecoding_neurosnyth_s2_ratio150_202201192323
# NonmetricDecoding_neurosnyth_s2_ratio300_202201192258
# NonmetricDecoding_contrast_s2_ratio120_202301241603 # used on 10.05.2024 plots

mvpa <- mvpa[-c(7:9)]
mvpa$subID <-as.factor(mvpa$subID)

head(mvpa)

voxelSize = '150'

#### order things a bit for easy manipulation for plotting


#name change from mask to roi
mvpa$mask <-as.factor(mvpa$mask)
names(mvpa)[2] <- 'roi'

# let's make a expType to split the no_pitch exp from pitch exp
mvpa$subNb <- as.numeric(mvpa$subID)
mvpa$expType<- ifelse(mvpa$subNb < 11, 'P4', 'P1') # 23 for RhythmBlock, 11 for Nonmetric
mvpa$expType <-as.factor(mvpa$expType)

#make roi order to call accordingly
# mvpa$roi_order <- ifelse(mvpa$roi == 'lSTG', 1,
#                           ifelse(mvpa$roi == 'rSTG', 2,
#                                  ifelse(mvpa$roi == 'SMA', 3,
#                                                ifelse(mvpa$roi == 'lpreM', 4,
#                                                       ifelse(mvpa$roi == 'rpreM', 5,6)))))


mvpa$roi_order <- ifelse(mvpa$roi == 'lSTG', 1,
                         ifelse(mvpa$roi == 'rSTG', 2,
                                ifelse(mvpa$roi == 'lSMA', 3,
                                       ifelse(mvpa$roi == 'rSMA', 4,
                                              ifelse(mvpa$roi == 'lpreM', 5,
                                                     ifelse(mvpa$roi == 'rpreM', 6,
                                                            ifelse(mvpa$roi == 'lputa', 7,
                                                                   ifelse(mvpa$roi == 'rputa', 8,
                                                                          ifelse(mvpa$roi == 'lcereb', 9,10)))))))))

# mvpa$roi_order <- ifelse(mvpa$roi == 'lSTG_10mm', 2,
#                          ifelse(mvpa$roi == 'rSTG_10mm', 6, 
#                                 ifelse(mvpa$roi == 'lSTG_15mm', 3, 
#                                        ifelse(mvpa$roi == 'rSTG_15mm', 7,
#                                               ifelse(mvpa$roi == 'lSTG_20mm', 4,
#                                                      ifelse(mvpa$roi == 'rSTG_20mm', 8,
#                                                             ifelse(mvpa$roi == 'lSTG_nopitch_Block_p00001', 1,
#                                                                    ifelse(mvpa$roi == 'rSTG_nopitch_Block_p00001', 5, 99))))))))

# this is for JuelichBrain ROIs
# mvpa$roi_order <- ifelse(mvpa$roi == 'STS1', 1,
#                          ifelse(mvpa$roi == 'STS2', 2,
#                                 ifelse(mvpa$roi == 'TE10', 3,
#                                        ifelse(mvpa$roi == 'TE11', 4,
#                                               ifelse(mvpa$roi == 'TE12', 5,
#                                                      ifelse(mvpa$roi == 'TE21', 6,
#                                                             ifelse(mvpa$roi == 'TE22', 7,
#                                                                    ifelse(mvpa$roi == 'TE30', 8,
#                                                                           ifelse(mvpa$roi == 'TI', 9,
#                                                                                  ifelse(mvpa$roi == 'TPJ', 10, 11))))))))))

# think about other ways of ordering with below function
# mvpa$roi_order <- grepl('nopitch', mvpa$roi, fixed = TRUE)

# currently we  don't have 2 hemispheres for every ROI
# later on consider this separation for "Aud Cx only" analysis
mvpa$hemis <- ifelse(substr(mvpa$roi,1,1) == 'l', 'left',
                     ifelse(substr(mvpa$roi,1,1) == 'r', 'right',NA))

# # delete hemis - for auditory cx decoding .csv files
# mvpa$hemis<- NULL
# ##### change the hemisphere header/name here - 15.03/2023
# colnames(mvpa)[7] <- 'hemis'


# make everything factor
str(mvpa)
mvpa$image<- as.factor(mvpa$image)
mvpa$subType<-as.factor(mvpa$subType)
mvpa$subTypeLabel<- as.factor(mvpa$subTypeLabel)
mvpa$hemis<-as.factor(mvpa$hemis)

# decoding accuracies should be in % 
mvpa$accuracy <-mvpa$accuracy*100

# subset the dataframe for plotting/analysis
img = 't_maps' # or 't_maps' 'beta'
exp = 'P4' 

subsetmvpa = subset(mvpa,expType == exp)
subsetmvpa = subset(subsetmvpa, image == img)

str(subsetmvpa)


# let's subset it again with only 8 rois
#subsetmvpa<- subset(subsetmvpa, roi_order < 9)

df <- summarySE(data = subsetmvpa, 
                groupvars=c('roi_order','roi', 'subType','hemis'),
                measurevar='accuracy', na.rm = TRUE)
df


# ignore the subType (good/badTapper)
df <- summarySE(data = subsetmvpa, 
                groupvars=c('roi_order','roi', 'hemis'),
                measurevar='accuracy', na.rm = TRUE)
df




######### analyze the data ######### 
# Levene's Test is for homogeneity of variances test 
leveneTest(accuracy ~ subType, data = subsetmvpa)
# p< 0.05 means there's no evidence to suggest that the variance in accuracy is 
# statistically different for the two tapping groups. 

# we can also use F-test for that
# to assess whether the variance of 2 groups are equal
result <- var.test(accuracy ~ subType, data = subsetmvpa)
result
# interpretation: The p-value is p = 0.2 which is greater than 
# the significance level 0.05. In conclusion, there is no significant 
# difference between the two variances.


# # test normality before performing t-test to see if decoding accuracy 
# is significantly different than zero
subsetmvpa %>%
  group_by(roi) %>%
  shapiro_test(accuracy)

# # A tibble: 6 x 4
# roi        variable statistic      p
# <fct>      <chr>        <dbl>  <dbl>
#   1 cerebellum accuracy     0.896 0.263 
# 2 lpreM      accuracy     0.898 0.209 
# 3 lSTG       accuracy     0.907 0.260 
# 4 rpreM      accuracy     0.918 0.341 
# 5 rSTG       accuracy     0.802 0.0154
# 6 SMA        accuracy     0.984 0.981




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
# # # lmm from afex package
# m1 <- mixed(accuracy ~  roi * hemis * subType + (1|subID), data = subsetmvpa)
# m1
# 
# m2 <- mixed(accuracy ~  roi + (1|subID), data = subsetmvpa)
# m2


# let's do t-test 

############ 
head(subsetmvpa)

# spill out the roi names t ochoose from for the t-test
subsetmvpa$roi[11][]

roiName = 'lSTG'
lSTG <-subset(subsetmvpa, roi == roiName & hemis == 'left')
roiName = 'rSTG'
rSTG <-subset(subsetmvpa, roi ==roiName & hemis == 'right')

t.test(lSTG$accuracy, mu = 50, alternative = 'greater') 

t.test(rSTG$accuracy, mu = 50, alternative = 'greater')

# rhythmblock varied pitch
# no sig.

# rhythmblock fixed pitch
# no sig.

# rhythmblock varied pitch
# no sig.

# nonmetric fixed pitch results
# rSTS1: p=0.04437
# STS2 : no sig.
# TE10 : left hemi p= 0.032, r p=0.038
# TE11 : no sig., TE12: no sig., TE21 : no sig.
# TE22 : l p= 0.011, r p =0.09
# TE30 = r p=0.0015
# TI : no sig., TPJ no sig., TeI : no sig.

# nonmetric varied pitch
# rSTS1: no sig.
# STS2 : no sig.
# TE10 : no sig.
# TE11 : no sig.,TE12: no sig., TE21 : no sig.
# TE22 : r p =0.035
# TE30 = r p=0.01
# TI : no sig., TPJ r p=0.005, TeI : r p=0.002


#############


# ROIs for Grahn2007 and RhythmBlock contrast
lSTG <-subset(subsetmvpa, roi =='lSTG')
rSTG <-subset(subsetmvpa, roi =='rSTG')
#SMA <-subset(subsetmvpa, roi =='SMA')
rSMA <-subset(subsetmvpa, roi =='rSMA')
lSMA <-subset(subsetmvpa, roi =='lSMA')
lpreM <-subset(subsetmvpa, roi =='lpreM')
rpreM <-subset(subsetmvpa, roi =='rpreM')
# cerebellum <-subset(subsetmvpa, roi =='cerebellum')
lcereb <-subset(subsetmvpa, roi =='lcereb')
rcereb <-subset(subsetmvpa, roi =='rcereb')

lputa <-subset(subsetmvpa, roi =='lputa')
rputa <-subset(subsetmvpa, roi =='rputa')



# results of t-test for RhythmBlock All Sounds vs. Base Contrast 
# 10/05/2024
a = t.test(lSTG$accuracy, mu = 50, alternative = 'greater') 
b = t.test(rSTG$accuracy, mu = 50, alternative = 'greater') 
c = t.test(lpreM$accuracy, mu = 50, alternative = 'greater')
d = t.test(rpreM$accuracy, mu = 50, alternative = 'greater') 
e= t.test(rSMA$accuracy, mu = 50, alternative = 'greater') 
# e = t.test(SMA$accuracy, mu = 50, alternative = 'greater')
f= t.test(lSMA$accuracy, mu = 50, alternative = 'greater')  
g= t.test(lcereb$accuracy, mu = 50, alternative = 'greater') 
h = t.test(rcereb$accuracy, mu = 50, alternative = 'greater') 
i= t.test(lputa$accuracy, mu = 50, alternative = 'greater') 
j= t.test(rputa$accuracy, mu = 50, alternative = 'greater') 

# t.test(cerebellum$accuracy, mu = 50, alternative = 'greater')


# pvalues for fixed pitch, nonmetric
# lSTG, rSTG, lpreM, rpreM, rSMA, lSMA, lcereb, rcereb, lputa, rputa
# c(0.03754,0.0957, 0.0006353, 0.02609,0.03061, 0.1823)

# pvalues for varied pitch nonmetric grahn 2007
# lSTG, rSTG, lpreM, rpreM, rSMA, lSMA, lcereb, rcereb, lputa, rputa
pvalues <- c(a$p.value, b$p.value, c$p.value, d$p.value, e$p.value,
             f$p.value, g$p.value, h$p.value, i$p.value, j$p.value)


# rhythmblock contrast ROIs: lSTG, rSTG, lpreM, rpreM, SMA, rcereb, 
# pvalues <- c(a$p.value, b$p.value, c$p.value, d$p.value, e$p.value, 
#             h$p.value)
pvalues

# update on 21/03/2025
# pvalues for varied pitch, rhythmBlock 
# [1] 0.7939764 0.8038833 0.8281610 0.7216085 0.3721025 0.3960716

fdrs<-p.adjust(pvalues, method="BH")

print(fdrs)



############# PLOTTING  #############
#is.na(mvpa$accuracy)
# min(subsetmvpa$accuracy, na.rm = TRUE)
# max(subsetmvpa$accuracy, na.rm = TRUE)
# 
# setlimit = c(0.2,0.9) 
# setbreak = c(0.25, 0.5, 0.75, 1)
# 
# 
# rois = '11ROI'
# 
# shapesize = 1
# shapetype = 21
# shapestroke = 1
# transparent = 1 #0.6
# jitter  = position_jitterdodge(0.3) # position_jitter(width=0.3)
# 
# 
# 
# # colors 
# nonmetricGrayBad = "#9ec5aa" # complex green= 3d8c55ff, nonmetricGrap = 6B6B6B
# nonmetricGrayGood = "#3d8c55ff" 
# simplePurpleBad = "#c4a4c9"
# simplePurpleGood = "#8a4a95"
# # conditions
# category2 = ''
# category1 = ''
# 
# cond1= paste0(category2," Bad Tapper")
# cond2 = paste0(category2,' Good Tapper')
# cond3 = paste0(category1,' Bad Tapper')
# cond4 = paste0(category1,' Good Tapper')
# 
# cond = 'Nonmetric'
# 
# ##### separate tappers
# fig <- ggplot(data = subsetmvpa, 
#               aes(x = reorder(roi, roi_order),
#                   y = accuracy, 
#                   color = subType,
#                   group = subType)) +
#   geom_point(data=subsetmvpa, 
#              aes(x = reorder(roi, roi_order), 
#                  y = accuracy), 
#              size = shapesize,
#              position = jitter, shape = shapetype, stroke = shapestroke, na.rm = TRUE) + 
#   stat_summary(aes(color = subType), fun=mean, fun.min = mean, fun.max = mean, 
#                geom="crossbar", size=0.6, width=0.6, position = position_dodge(width=.75), 
#                na.rm = TRUE) +
#   theme_classic() +
#   geom_errorbar(data = df, 
#                 aes(ymin = accuracy-se, ymax = accuracy+se, group = subType), 
#                 color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
#   geom_hline(yintercept=c(.5), linetype="dotted", colour="black", size=.5) +
#   
#   ggtitle("") +
#   ylab("") +
#   xlab("") +
#   theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
#   theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
#   theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
#   scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
#   scale_x_discrete(labels = c("lSTG All", "lSTG 10","lSTG 15","lSTG 20","rSTG All","rSTG 10","rSTG 15","rSTG 20"))+
#   scale_color_manual(name = '', labels = c(cond1, cond2), values=c(nonmetricGrayBad, nonmetricGrayGood)) + 
#   # theme(legend.position= "none")
#   theme(legend.position= c(.85, .85)) +
#   theme(legend.text=element_text(size=8)) +
#   theme(legend.title=element_text(size=9))
# fig
# 
# filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,  'voxelNb-', voxelSize, '_', rois,'tappers.png')
# # ggsave(filename, fig, dpi=300, width=15, height=6, units='cm') 
# ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512


#######################################################
# SMA is single hemi in AllRhythmvsSilence Condition 
#######################################################

# make a column with only hemi and only roi

subsetmvpa <- subsetmvpa %>%
  mutate(roi = as.character(roi),
         region = if_else(roi == "SMA", "SMA", str_sub(roi, 2, nchar(roi))))

head(subsetmvpa)

# Convert back to factor
subsetmvpa <- subsetmvpa %>%
  mutate(roi = as.factor(roi),  
         region = as.factor(region))  # Also convert region to factor if needed


###############################################################################

# Juelich Atlas ROIs - roi11 plotting
# plots separately the hemispheres

###############################################################################
# # # make a column for roi x hemis
# # roiHemis <- paste(subsetmvpa$roi, subsetmvpa$hemis)
# # 
# # df2 <- summarySE(data = subsetmvpa, 
# #                  groupvars=c('roi_order','roiHemis'),
# #                  measurevar='accuracy', na.rm = TRUE)
# # df2
# 
# # divide the hemispheres into 2 plots
# rois = 'Right11Roi'
# subsetmvpaL <-subset(subsetmvpa, hemis == 'r')
# 
# df2 <- summarySE(data = subsetmvpaL, 
#                  groupvars=c('roi_order','roi'),
#                  measurevar='accuracy', na.rm = TRUE)
# df2
# 
# 
# fig <- ggplot(data = subsetmvpaL, 
#               aes(x = reorder(roi, roi_order),
#                   y = accuracy),
#               color = expType) +
#   geom_jitter(size = shapesize, shape = shapetype, stroke = shapestroke, width=0.1, color = nonmetricGrayGood, 
#               na.rm = TRUE) +
#   stat_summary(aes(color = expType), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.3,
#                na.rm = TRUE) +
#   theme_classic() +
#   geom_errorbar(data = df2, 
#                 aes(ymin = accuracy-se, ymax = accuracy+se), 
#                 color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75),
#                 na.rm = TRUE) +
#   geom_hline(yintercept=c(.5), linetype="dotted", colour="black", size=.5) +
#   
#   ggtitle("") +
#   ylab("") +
#   xlab("") +
#   theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
#   theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
#   theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
#   scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
#   #scale_x_discrete(labels = c("lSTG All", "lSTG 10","lSTG 15","lSTG 20","rSTG All","rSTG 10","rSTG 15","rSTG 20"))+
#   scale_color_manual(name = '', labels =c('Simple vs. Nonmetric'), values=c(nonmetricGrayGood)) + #
#   theme(legend.position= c(.9, 1)) +
#   theme(legend.text=element_text(size=9)) +
#   theme(legend.title=element_text(size=9))
# fig
# 
# filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,  '_', 'voxelNb-',  voxelSize,  '_', rois,  '_', exp,  '_', img, '.png')
# ggsave(filename, fig, dpi=300, width=8, height=2) # 1024 x 512
# 
# 
# 
# 
# 
# # figure for roi already cointains hemisphere info
# fig <- ggplot(data = subsetmvpa, 
#               aes(x = reorder(roi, roi_order),
#                   y = accuracy),
#               color = expType) +
#   geom_jitter(size = shapesize, shape = shapetype, stroke = shapestroke, width=0.1, color = nonmetricGrayGood, 
#               na.rm = TRUE) +
#   stat_summary(aes(color = expType), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.3,
#                na.rm = TRUE) +
#   theme_classic() +
#   geom_errorbar(data = df2, 
#                 aes(ymin = accuracy-se, ymax = accuracy+se), 
#                 color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75),
#                 na.rm = TRUE) +
#   geom_hline(yintercept=c(.5), linetype="dotted", colour="black", size=.5) +
#   
#   ggtitle("") +
#   ylab("") +
#   xlab("") +
#   theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
#   theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
#   theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
#   scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
#   scale_x_discrete(labels = c("lSTG All", "lSTG 10","lSTG 15","lSTG 20","rSTG All","rSTG 10","rSTG 15","rSTG 20"))+
#   scale_color_manual(name = '', labels =c('Simple vs. Nonmetric'), values=c(nonmetricGrayGood)) + #
#   theme(legend.position= c(.85, .9)) +
#   theme(legend.text=element_text(size=9)) +
#   theme(legend.title=element_text(size=9))
# fig
# 
# filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,  'voxelNb-', voxelSize,  '_', rois, '.png')
# ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512


###############################################################################

# 10/05/2024 - Nonmetric decoding with RhythmBlock AllSounds vs. Rest contrast

# 16/05/2024 - Complex decoding with RhythmBlock AllSounds vs. Rest contrast
###############################################################################
any(is.na(subsetmvpa$accuracy))

min(subsetmvpa$accuracy, na.rm = TRUE)
max(subsetmvpa$accuracy, na.rm = TRUE)

setlimit = c(20,85)  # nonmetric it's 10
setbreak = c(20, 40, 60, 80)


rois = 'Grahn2007ROIs_NonmetricExp' # RhythmBlockExp  NonmetricExp
# rois = 'RhythmBlockContrastROIs_RhythmBlockExp'
shapesize = 1
shapetype = 21
shapestroke = 1
transparent = 1 #0.6
jitter  = position_jitterdodge(0.3) # position_jitter(width=0.3)



###############

# just dot plot  
# May 2024 

##############

# colors 
colorchosen = '#ec5800'

# nonmetricGrayBad = "#9ec5aa" 
# nonmetricGrayGood = "#3d8c55ff" # complex green= 3d8c55ff, nonmetricGrap = 6B6B6B 
# simplePurpleBad = "#c4a4c9"
# simplePurpleGood = "#8a4a95"

# conditions
cond = 'Complex' # Complex Nonmetric
legendText = paste0(" Simple vs. ", cond)
xLabel =c("L STG ", "R STG", "SMA","L PreM","R PreM","R Cereb")
#xLabel =c("L STG ", "R STG","L SMA", "R SMA","L PreM","R PreM","L Puta","R Puta","L Cereb", "R Cereb")

# to calculate the SEM
df2 <- summarySE(data = subsetmvpa, 
                 groupvars=c('roi_order','roi', 'hemis'),
                 measurevar='accuracy', na.rm = TRUE)
df2


fig <- ggplot(data = subsetmvpa, 
              aes(x = reorder(roi, roi_order),
                  y = accuracy),
              color = expType) +
  geom_jitter(size = shapesize, shape = shapetype, stroke = shapestroke, width=0.1, color = colorchosen, 
              na.rm = TRUE) +
  stat_summary(aes(color = expType), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.3,
               na.rm = TRUE) +
  theme_classic() +
  geom_errorbar(data = df2, 
                aes(ymin = accuracy-se, ymax = accuracy+se), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75),
                na.rm = TRUE) +
  geom_hline(yintercept=c(50), linetype="dotted", colour="black", size=.5) +
  
  ggtitle("") +
  ylab("Classification accuracy % ") +
  xlab("") +
  theme(axis.text.x=element_text(size=10,  angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=12, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
#  scale_x_discrete(labels = xLabel)+
  scale_color_manual(name = '', labels =c(legendText), values=c(colorchosen)) + #
  theme(legend.position= c(.9, .9)) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.title=element_text(size=9))
fig

filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,
                     '_voxelNb-', voxelSize,  '_', rois,'_', img, '_', exp, '.png')
ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512


###############

# violin plot trial 
# 25.03.2025

##############
cond = 'Nonmetric'
# new_roi_labels = c(
#     "lSTG" = "L\nSTG",
#     "rSTG" = "R\nSTG",
#     "SMA" = "\nSMA",
#     "lpreM" = "L\nPremotor",
#     "rpreM" = "R\nPremotor",
#     "rcereb" = "R\nCerebellum"
#   )

# grahn 2007 rois
new_roi_labels = c(
  "lSTG" = "L\nSTG",
  "rSTG" = "R\nSTG",
  "lSMA" = "L\nSMA",
  "rSMA" = "r\nSMA",
  "lpreM" = "L\nPremotor",
  "rpreM" = "R\nPremotor",
  "rcereb" = "R\nCerebellum",
  "lcereb" = "L\nCerebellum",
  "rputa" = "R\nPutamen",
  "lputa" = "L\nPutamen"
)

  
colorchosen = '#1b131c'
# simpleOrange = '#ec5800'
# nonmetricBlack= '#1b131c'

# to calculate the SEM
df2 <- summarySE(data = subsetmvpa, 
                 groupvars=c('roi_order','roi', 'hemis'),
                 measurevar='accuracy', na.rm = TRUE)
df2

fig <- ggplot(data = subsetmvpa, 
              aes(x = reorder(roi, roi_order), y = accuracy)) +
  
  # Half Violin Plot (Using Fixed Color)
  geom_half_violin(position = position_nudge(x = -0.15), 
                   side = "l", 
                   alpha = 0.4, 
                   fill = colorchosen,  # Use fixed color
                   color = NA,  na.rm = TRUE) +
  
  # Jittered Dot Plot (Using Fixed Color)
  geom_jitter(size = shapesize, shape = shapetype, stroke = shapestroke, 
              width = 0.1, color = colorchosen, na.rm = TRUE) +
  
  # Mean Crossbar (Using Fixed Color)
  stat_summary(fun = mean, fun.min = mean, fun.max = mean, 
               geom = "crossbar", size = 0.6, width = 0.3, 
               color = colorchosen, na.rm = TRUE) +
  
  # Error Bars (Using Fixed Color)
  geom_errorbar(data = df2, 
                aes(ymin = accuracy - se, ymax = accuracy + se), 
                color = 'black', size = 0.5, width = 0.15, 
                alpha = transparent, position = position_dodge(width = .75), na.rm = TRUE) +
  
  # Chance Level Line
  geom_hline(yintercept = 50, linetype = "dotted", colour = "black", size = .5) +
  
  # Labels & Theme
  ggtitle("Simple vs. Nonmetric Classification") +
  ylab("Classification accuracy %") +
  xlab("") +
  theme_classic() +
  theme(text = element_text(family = "Avenir"), 
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 9, colour = 'black'),
        axis.title.y = element_text(size = 12, angle = 90, colour = 'black'),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  
  # x-axis labels
  scale_x_discrete(labels = new_roi_labels) +
  # Scale
  scale_y_continuous(limits = setlimit, breaks = setbreak, position = "left") +
  
  # Remove Legend (Since Color is Fixed)
  theme(legend.position = "none")


fig

filename <- paste0(pathResults, 'Grahn2007_Decoding_Simple_vs_',cond,
                     '_voxelNb-', voxelSize,  '_', rois,'_', img, '_', exp, '.png')

ggsave(filename, fig, dpi=300, width=10, height=3) # 1024 x 512 # width=6, height=3






#######

## showing the BAD AND GOOD TAPPERS

#####

# df <- summarySE(data = subsetmvpa, 
#                 groupvars=c('roi_order','roi', 'subType','hemis'),
#                 measurevar='accuracy', na.rm = TRUE)
# df

# new_roi_labels = c(
#   "lSTG" = "L\nSTG",
#   "rSTG" = "R\nSTG",
#   "SMA" = "\nSMA",
#   "lpreM" = "L\nPremotor",
#   "rpreM" = "R\nPremotor",
#   "rcereb" = "R\nCerebellum"
# )
# grahn 2007 rois
new_roi_labels = c(
  "lSTG" = "L\nSTG",
  "rSTG" = "R\nSTG",
  "lSMA" = "L\nSMA",
  "rSMA" = "r\nSMA",
  "lpreM" = "L\nPremotor",
  "rpreM" = "R\nPremotor",
  "rcereb" = "R\nCerebellum",
  "lcereb" = "L\nCerebellum",
  "rputa" = "R\nPutamen",
  "lputa" = "L\nPutamen"
)

colorchosen = '#1b131c'
# simpleOrange = '#ec5800'
# nonmetricBlack= '#1b131c'

shapes <- c(goodTapper = 1, badTapper = 17)  # 1 = Empty Circle, 17 = Filled Triangle

fig <- ggplot(data = subsetmvpa, 
              aes(x = reorder(roi, roi_order), y = accuracy)) +  
  
  # Half Violin Plot (Using Fixed Color)
  geom_half_violin(position = position_nudge(x = -0.15), 
                   side = "l", alpha = 0.4, 
                   fill = colorchosen,  
                   color = NA, na.rm = TRUE) +
  
  # Jittered Dot Plot with Shapes
  geom_jitter(aes(shape = subType),  # Only map shape here
              size = shapesize, stroke = shapestroke, 
              width = 0.1, color = colorchosen, na.rm = TRUE) +  
  
  # Mean Crossbar (Fixed Color)
  stat_summary(fun = mean, fun.min = mean, fun.max = mean, 
               geom = "crossbar", size = 0.6, width = 0.3, 
               color = colorchosen, na.rm = TRUE) +
  
  # Error Bars (Fixed Color, No subType)
  geom_errorbar(data = df2,  
                aes(ymin = accuracy - se, ymax = accuracy + se),  
                color = 'black', size = 0.5, width = 0.15, 
                alpha = transparent, position = position_dodge(width = .75), na.rm = TRUE) +
  
  # Chance Level Line
  geom_hline(yintercept = 50, linetype = "dotted", colour = "black", size = .5) +
  
  # Labels & Theme
  ggtitle("Simple vs. Nonmetric Classification") +
  ylab("Classification accuracy %") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 9, colour = 'black'),
        axis.title.y = element_text(size = 12, angle = 90, colour = 'black'),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  
  # X-axis labels
  scale_x_discrete(labels = new_roi_labels) +
  
  # Scale for Y-Axis
  scale_y_continuous(limits = setlimit, breaks = setbreak, position = "left") +
  
  # Shape Mapping (Without Legend)
  scale_shape_manual(values = shapes, guide = "none") +
  
  # Remove Legend
  theme(legend.position = "none")

  # # Manual Shape Scale with Legend
  # scale_shape_manual(name = "", values = shapes) + # Define legend for shapes
  # 
  # # Enable Legend
  # theme(legend.position = c(0.95, 0.95),
  #       legend.spacing.x = unit(0.2, "cm"),
  #       legend.key.size = unit(0.2, "cm")) # legend.text = element_text(size = 10)

fig

filename <- paste0(pathResults, 'Grahn2007_Tappers_Decoding_Simple_vs_',cond,
                   '_voxelNb-', voxelSize,  '_', rois,'_', img, '_', exp, '.png')

ggsave(filename, fig, dpi=300, width=10, height=3) # 1024 x 512




###############################################################################

# 04/06/2024 - Nonmetric decoding with Juelich Brain atlas ROIs

# 04/06/2024 - RhythmBlock decoding with Juelich Brain atlas ROIs
###############################################################################
min(subsetmvpa$accuracy, na.rm = TRUE)
max(subsetmvpa$accuracy, na.rm = TRUE)

setlimit = c(15,90) 
setbreak = c(15, 50, 70, 90)

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
cond = 'Complex' # complex
legendText = paste0(" Simple vs. ", cond)
xLabel =c("STS1", "STS2", "TE10","TE11", "TE12", "TE21", "TE22", "TE30", "TeI", "TI", "TPJ")



# for plotting, let's divide the data
left<-subset(subsetmvpa, hemis =='l')
right<- subset(subsetmvpa, hemis =='r')


# plot LEFT 
dfleft <- summarySE(data = left, 
                    groupvars=c('roi_order','roi'),
                    measurevar='accuracy', na.rm = TRUE)
dfleft
  
rois = 'JuelichAuditory_P4_left'


fig <- ggplot(data = left, 
              aes(x = reorder(roi, roi_order),
                  y = accuracy),
              color = expType) +
  geom_jitter(size = shapesize, shape = shapetype, stroke = shapestroke, width=0.1, color = nonmetricGrayGood, 
              na.rm = TRUE) +
  stat_summary(aes(color = expType), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.3,
               na.rm = TRUE) +
  theme_classic() +
  geom_errorbar(data = dfleft, 
                aes(ymin = accuracy-se, ymax = accuracy+se), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75),
                na.rm = TRUE) +
  geom_hline(yintercept=c(50), linetype="dotted", colour="black", size=.5) +
  
  ggtitle("") +
  ylab("Classification accuracy % ") +
  xlab("") +
  theme(axis.text.x=element_text(size=10,  angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=12, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  #  scale_x_discrete(labels = xLabel)+
  scale_color_manual(name = '', labels =c(legendText), values=c(nonmetricGrayGood)) + #
  theme(legend.position= c(.9, .9)) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.title=element_text(size=9))
fig

filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,  'voxelNb-', voxelSize,  '_', rois, '.png')
ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512



# plot RIGHT
dfright <- summarySE(data = right, 
                    groupvars=c('roi_order','roi'),
                    measurevar='accuracy', na.rm = TRUE)
dfright

rois = 'JuelichAuditory_P4_right'


fig <- ggplot(data = right, 
              aes(x = reorder(roi, roi_order),
                  y = accuracy),
              color = expType) +
  geom_jitter(size = shapesize, shape = shapetype, stroke = shapestroke, width=0.1, color = nonmetricGrayGood, 
              na.rm = TRUE) +
  stat_summary(aes(color = expType), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.3,
               na.rm = TRUE) +
  theme_classic() +
  geom_errorbar(data = dfright, 
                aes(ymin = accuracy-se, ymax = accuracy+se), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75),
                na.rm = TRUE) +
  geom_hline(yintercept=c(50), linetype="dotted", colour="black", size=.5) +
  
  ggtitle("") +
  ylab("Classification accuracy % ") +
  xlab("") +
  theme(axis.text.x=element_text(size=10,  angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=12, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  #  scale_x_discrete(labels = xLabel)+
  scale_color_manual(name = '', labels =c(legendText), values=c(nonmetricGrayGood)) + #
  theme(legend.position= c(.9, .9)) +
  theme(legend.text=element_text(size=8)) +
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



