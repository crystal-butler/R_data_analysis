###### Lab 3 Script : Stroop #######
#### Crystal Butler ####

# set working directory
setwd("/Users/interloper/cb2610/Google Drive/NYU/Courses/Cognition Lab/Lab 3")
setwd("/Users/interloper/Google Drive/NYU/Courses/Cognition Lab/Lab 3")

# step 1 - read in the data
lab3data=read.table("data_lab3.txt", header=F) # heaader = F: there is no header at top of file

# name or rename header
lab3head=c("Subject", "Condition", "Trial", "Word", "Response Type", "Trial Type", "Rotation", "Correct Response", "Actual Response", "Hit", "RT")
names(lab3data) <- lab3head

# add some variables
WORD = 1
COLOR = 2 
NORMAL = 0
ROTATED = 1
CTL = 1
CON = 2
INCON = 3

# tapply() applies whatever function we want (in this case mean()) to any combination of columns
# tapply returns results as a matrix
accuracy = tapply(lab3data$Hit, lab3data$Subject, mean)

# remove outliers from the data. Using data where array elements 
# less than 3 SDs from mean = TRUE
cutoff=mean(lab3data$RT)+3*sd(lab3data$RT)
lab3clean=lab3data[lab3data$RT<cutoff, ]

###### Here is where we begin analysis of #######
###### color rotside-down data ###################
#################################################

# select all trials where the response to was to color
colorResp=lab3clean[lab3clean$"Response Type"==COLOR, ]

# get the trials where the stimulus was rotside-down
rotResp=lab3clean[lab3clean$Rotation==ROTATED, ]

# logical combination of upside-down and color responses 
rotColor=lab3clean[lab3clean$"Response Type"==COLOR & lab3clean$Rotation==ROTATED, ]

# get all the RTs from the control trials
rotColorRTCTL=rotColor[rotColor$"Trial Type"==CTL, ]$RT

# to list all colors, use colors() in R console
# To plot a histrogram just use the hist() function

hist(rotColorRTCTL, breaks=seq(0, max(rotColorRTCTL)+50,50), col="tomato3", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds, Inverted Trials", ylab="Response Frequency")

# Plot individual rotated Color histograms
# to list all colors, use colors() in R console
# get all the RTs from the congruent trials
rotColorRTCON=rotColor[rotColor$"Trial Type"==CON, ]$RT

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
dev.new()
hist(rotColorRTCON, breaks=seq(0,max(rotColorRTCON)+50,50), col="slateblue4", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds, Inverted Trials", ylab="Response Frequency")

# get all the RTs from the incongruent trials
rotColorRTINCON=rotColor[rotColor$"Trial Type"==INCON, ]$RT

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
dev.new()
hist(rotColorRTINCON, breaks=seq(0,max(rotColorRTINCON)+50,50), col="khaki3", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds, Inverted Trials", ylab="Response Frequency")


# Plot all rotated Color trials on a single histogram
# To plot a histrogram just use the hist() function
dev.new()
hist(rotColorRTCON, breaks=seq(0,max(rotColorRTCON)+50,50), col="ivory1", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds, Inverted Trials", ylab="Response Frequency")

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
hist(rotColorRTCTL, breaks=seq(0, max(rotColorRTCTL)+50,50), col="grey63", ylim=c(0,100), xlim=c(0,4000), add=T)

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
hist(rotColorRTINCON, breaks=seq(0,max(rotColorRTINCON)+50,50), col="grey31", ylim=c(0,100), xlim=c(0,4000), add=T)

legend(1500,100,c("Congruent", "Control", "Incongruent"), fill=c("ivory1","grey63", "grey31"))

# Calculate mean and median of reaction time by condition
rotColorRTmean=tapply(rotColor$RT, list(rotColor$"Subject", rotColor$"Trial Type"), mean)
rotColorRTmed=tapply(rotColor$RT, list(rotColor$"Subject", rotColor$"Trial Type"), median)

# Put mean and median into data frames
rotColorRTmean=as.data.frame(rotColorRTmean)
names (rotColorRTmean)=c("Control", "Congruent", "Incongruent")

rotColorRTmed=as.data.frame(rotColorRTmed)
names (rotColorRTmed)=c("Control", "Congruent", "Incongruent")

# calculate mean across subjects for each condition
subjMean=apply(rotColorRTmean, 2, mean)
subjMed=apply(rotColorRTmed, 2, mean)
paste ("Mean of Median Reaction Times")
print (subjMed)

# Compute the standard errors, mean
numSubj = length(rotColorRTmean[, 1])
sDevs = apply(rotColorRTmean, 2, sd)
print (sDevs)
sErrs = sDevs/sqrt(numSubj)
print (sErrs)

# Compute the standard errors, median
numSubj2 = length(rotColorRTmed[, 1])
sDevs2= apply(rotColorRTmed, 2, sd)
print (sDevs2)
sErrs2 = sDevs2/sqrt(numSubj2)
print (sErrs2)

# bar plot the mean reaction times per condition across subjects, with error bars
# store plot to variable because info such as center of bar is associated
dev.new()
conBars = barplot(subjMed, col=c("ivory1", "grey63", "grey31"), main="", xlab="Condition, Color Naming Inverted Trials", ylab="RT in Milliseconds", ylim=c(0, max(subjMed)+max(sErrs2)+100))
segments(conBars, subjMed-sErrs2, conBars, subjMed+sErrs2)

# Reformat data for ANOVA
found = which(rotColorRTmed !=-999, arr.ind=T)
rtANOVA = data.frame(cbind(found, rotColorRTmed[found]))
names(rtANOVA) = c('Subject', 'Condition', 'RT')

rtANOVA$Subject = factor(rtANOVA$Subject)
rtANOVA$Condition = factor(rtANOVA$Condition)

# Run the 1-way ANOVA
rotColorAOV=aov(rtANOVA$RT~rtANOVA$Condition+Error(rtANOVA$Subject))
summary(rotColorAOV)

# Paired t-tests to compare conditions
t.test(rotColorRTmed$Control, rotColorRTmed$Congruent, paired=T, mu=0, alternative="two.sided", var.equal=T)
t.test(rotColorRTmed$Control, rotColorRTmed$Incongruent, paired=T, mu=0, alternative="two.sided", var.equal=T)
t.test(rotColorRTmed$Congruent, rotColorRTmed$Incongruent, paired=T, mu=0, alternative="two.sided", var.equal=T)

# make bar plot for accuracy
# looking for evidence of speed-accuracy tradeoff
# in incongruent condition, people were slower, but should be >= accuracy in 'easier' congruent trials
# Plot all rotright Color trials on a single histogram for Accuracy
# Substitute $Hit for $RT
# To plot a histrogram just use the hist() function

# select all trials where the response to was to color
colorResp=lab3data[lab3data$"Response Type"==COLOR, ]

# get the trials where the stimulus was rotated
rotResp=lab3data[lab3data$Rotation==ROTATED, ]

# logical combination of upright and color responses 
rotColor=lab3data[lab3data$"Response Type"==COLOR & lab3data$Rotation==ROTATED, ]

# Calculate mean and median of reaction time by condition
accbyCondmean=tapply(rotColor$Hit, list(rotColor$"Subject", rotColor$"Trial Type"), mean)
accbyCondmed=tapply(rotColor$Hit, list(rotColor$"Subject", rotColor$"Trial Type"), median)

# Put mean and median into data frames
accbyCondmean=as.data.frame(accbyCondmean)
names (accbyCondmean)=c("Control", "Congruent", "Incongruent")

accbyCondmed=as.data.frame(accbyCondmed)
names (accbyCondmed)=c("Control", "Congruent", "Incongruent")

# calculate mean across subjects for each condition
subjMean=apply(accbyCondmean, 2, mean)
subjMed=apply(accbyCondmed, 2, mean)


# Compute the standard errors, mean
numSubj = length(accbyCondmean[, 1])
sDevs = apply(accbyCondmean, 2, sd)
print (sDevs)
sErrs = sDevs/sqrt(numSubj)
print (sErrs)

# Compute the standard errors, median
numSubj2 = length(accbyCondmed[, 1])
sDevs2= apply(accbyCondmed, 2, sd)
print (sDevs2)
sErrs2 = sDevs2/sqrt(numSubj2)
print (sErrs2)

# bar plot the mean reaction times per condition across subjects, with error bars
# store plot to variable because info such as center of bar is associated
dev.new()
subjMeanAcc=subjMean*100
errLow=(subjMean-sErrs)*100
errHigh=(subjMean+sErrs)*100
conBars = barplot(subjMeanAcc, col=c("ivory1", "grey63", "grey31"),
main="", xlab="Condition, Color Naming Inverted Trials", ylab="Mean Percent Accurate Responses", ylim=c(0, 100))
segments(conBars, errLow, conBars, errHigh)

# need to run 2-way ANOVA to show whether there is variance between upright and inverted trials
# get data from respond to COLOR for both upright & inverted conditions
# Calculate mean and median of reaction time by condition and include rotation
colorData=lab3clean[lab3clean$"Response Type"==COLOR,]

anova2way=tapply(colorData$RT, list(colorData$"Subject", colorData$"Trial Type", colorData$"Rotation"), median)

# Reformat data for ANOVA
found = which(anova2way !=-999, arr.ind=T)
rtANOVA = data.frame(cbind(found, anova2way[found]))
names(rtANOVA) = c("Subject", "Condition","Rotation", "RT")

rtANOVA$Subject = factor(rtANOVA$Subject)
rtANOVA$Condition = factor(rtANOVA$Condition)
rtANOVA$Rotation = factor(rtANOVA$Rotation)

# Run a 2-way ANOVA where both condition and rotation are a within-subjects factor
rotColorAOV=aov(rtANOVA$RT~rtANOVA$Condition*rtANOVA$Rotation+Error(rtANOVA$Subject))
summary(rotColorAOV)








