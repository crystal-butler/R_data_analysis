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

# remove outliers from the data. Using data where array elements less than 3 SDs from mean = TRUE
cutoff=mean(lab3data$RT)+3*sd(lab3data$RT)
lab3clean=lab3data[lab3data$RT<cutoff, ]

# select all trials where the response to was to color
colorResp=lab3clean[lab3clean$"Response Type"==COLOR, ]

# get the trials where the stimulus was upright
upResp=lab3clean[lab3clean$Rotation==NORMAL, ]

# logical combination of upright and color responses 
upColor=lab3clean[lab3clean$"Response Type"==COLOR & lab3clean$Rotation==NORMAL, ]

# get all the RTs from the control trials
upColorRTCTL=upColor[upColor$"Trial Type"==CTL, ]$RT

# to list all colors, use colors() in R console
# To plot a histrogram just use the hist() function

hist(upColorRTCTL, breaks=seq(0, max(upColorRTCTL)+50,50), col="tomato3", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds", ylab="Response Frequency")

# Plot individual Upright Color histograms
# to list all colors, use colors() in R console
# get all the RTs from the congruent trials
upColorRTCON=upColor[upColor$"Trial Type"==CON, ]$RT

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
dev.new()
hist(upColorRTCON, breaks=seq(0,max(upColorRTCON)+50,50), col="slateblue4", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds", ylab="Response Frequency")

# get all the RTs from the incongruent trials
upColorRTINCON=upColor[upColor$"Trial Type"==INCON, ]$RT

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
dev.new()
hist(upColorRTINCON, breaks=seq(0,max(upColorRTINCON)+50,50), col="khaki3", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds", ylab="Response Frequency")


# Plot all Upright Color trials on a single histogram
# To plot a histrogram just use the hist() function
dev.new()
hist(upColorRTCON, breaks=seq(0,max(upColorRTCON)+50,50), col="slateblue4", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds", ylab="Response Frequency")

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
hist(upColorRTCTL, breaks=seq(0, max(upColorRTCTL)+50,50), col="tomato3", ylim=c(0,100), xlim=c(0,4000), add=T)

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
hist(upColorRTINCON, breaks=seq(0,max(upColorRTINCON)+50,50), col="khaki3", ylim=c(0,100), xlim=c(0,4000), add=T)

legend(1500,100,c("Congruent", "Control", "Incongruent"), fill=c("slateblue4","tomato3", "khaki3"))

# Calculate mean and median of reaction time by condition
upColorRTmean=tapply(upColor$RT, list(upColor$"Subject", upColor$"Trial Type"), mean)
upColorRTmed=tapply(upColor$RT, list(upColor$"Subject", upColor$"Trial Type"), median)

# Put mean and median into data frames
upColorRTmean=as.data.frame(upColorRTmean)
names (upColorRTmean)=c("Control", "Congruent", "Incongruent")

upColorRTmed=as.data.frame(upColorRTmed)
names (upColorRTmed)=c("Control", "Congruent", "Incongruent")

# calculate mean across subjects for each condition
subjMean=apply(upColorRTmean, 2, mean)
subjMed=apply(upColorRTmed, 2, mean)

# Compute the standard errors
numSubj = length(upColorRTmean[, 1])
sDevs = apply(upColorRTmean, 2, sd)
print (sDevs)
sErrs = sDevs/sqrt(numSubj)
print (sErrs)

# Compute the standard errors
numSubj2 = length(upColorRTmed[, 1])
sDevs2= apply(upColorRTmean, 2, sd)
print (sDevs2)
sErrs2 = sDevs2/sqrt(numSubj2)
print (sErrs2)

# bar plot the mean reaction times per condition across subjects, with error bars
# store plot to variable because info such as center of bar is associated
dev.new()
conBars = barplot(subjMean, col=c("ivory1", "grey63", "grey31"), main="", xlab="Condition", ylab="RT in Milliseconds", ylim=c(0, max(subjMean)+max(sErrs)+100))
segments(conBars, subjMean-sErrs, conBars, subjMean+sErrs)

# Reformat data for ANOVA
found = which(upColorRTmean !=-999, arr.ind=T)
rtANOVA = data.frame(cbind(found, upColorRTmean[found]))
names(rtANOVA) = c('Subject', 'Condition', 'RT')

rtANOVA$Subject = factor(rtANOVA$Subject)
rtANOVA$Condition = factor(rtANOVA$Condition)

# Run the ANOVA
upColorAOV=aov(rtANOVA$RT~rtANOVA$Condition+Error(rtANOVA$Subject))
summary(upColorAOV)

# Paired t-tests to compare conditions
t.test(upColorRTmean$Control, upColorRTmean$Congruent, paired=T, mu=0, alternative="two.sided", var.equal=T)
t.test(upColorRTmean$Control, upColorRTmean$Incongruent, paired=T, mu=0, alternative="two.sided", var.equal=T)
t.test(upColorRTmean$Congruent, upColorRTmean$Incongruent, paired=T, mu=0, alternative="two.sided", var.equal=T)

