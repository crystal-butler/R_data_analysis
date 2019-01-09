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
###### Word Inverted data #######################`
#################################################

# select all trials where the response to was to Word
wordResp=lab3clean[lab3clean$"Response Type"==WORD, ]

# get the trials where the stimulus was Inverted
upResp=lab3clean[lab3clean$Rotation==ROTATED, ]

# logical combination of Inverted and Word responses 
upWord=lab3clean[lab3clean$"Response Type"==WORD & lab3clean$Rotation==ROTATED, ]

# to list all Words, use Words() in R console
# To plot a histrogram just use the hist() function

# Plot individual Inverted Word histograms
# to list all Words, use Words() in R console
# get all the RTs from the congruent trials
upWordRTCON=upWord[upWord$"Trial Type"==CON, ]$RT

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
dev.new()
hist(upWordRTCON, breaks=seq(0,max(upWordRTCON)+50,50), col="slateblue4", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds, Inverted Trials", ylab="Response Frequency")

# get all the RTs from the incongruent trials
upWordRTINCON=upWord[upWord$"Trial Type"==INCON, ]$RT

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
dev.new()
hist(upWordRTINCON, breaks=seq(0,max(upWordRTINCON)+50,50), col="khaki3", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds, Inverted Trials", ylab="Response Frequency")


# Plot all Inverted Word trials on a single histogram
# To plot a histrogram just use the hist() function
dev.new()
hist(upWordRTCON, breaks=seq(0,max(upWordRTCON)+50,50), col="ivory1", ylim=c(0,100), xlim=c(0,4000),main="", xlab="Reaction Time in Milliseconds, Inverted Trials", ylab="Response Frequency")

# To plot a histrogram just use the hist() function. add=T adds to previous plot.
hist(upWordRTINCON, breaks=seq(0,max(upWordRTINCON)+50,50), col="grey31", ylim=c(0,100), xlim=c(0,4000), add=T)

legend(1500,100,c("Congruent", "Control"), fill=c("ivory1","grey63"))

# Calculate mean and median of reaction time by condition
upWordRTmean=tapply(upWord$RT, list(upWord$"Subject", upWord$"Trial Type"), mean)
upWordRTmed=tapply(upWord$RT, list(upWord$"Subject", upWord$"Trial Type"), median)

# Put mean and median into data frames
upWordRTmean=as.data.frame(upWordRTmean)
names (upWordRTmean)=c("Congruent", "Incongruent")

upWordRTmed=as.data.frame(upWordRTmed)
names (upWordRTmed)=c("Congruent", "Incongruent")

# calculate mean across subjects for each condition
subjMean=apply(upWordRTmean, 2, mean)
subjMed=apply(upWordRTmed, 2, mean)
paste ("Mean of Median Reaction Times")
print (subjMed)

# Compute the standard errors, mean
numSubj = length(upWordRTmean[, 1])
sDevs = apply(upWordRTmean, 2, sd)
print (sDevs)
sErrs = sDevs/sqrt(numSubj)
print (sErrs)

# Compute the standard errors, median
numSubj2 = length(upWordRTmed[, 1])
sDevs2= apply(upWordRTmed, 2, sd)
print (sDevs2)
sErrs2 = sDevs2/sqrt(numSubj2)
print (sErrs2)

# bar plot the mean reaction times per condition across subjects, with error bars
# store plot to variable because info such as center of bar is associated
dev.new()
conBars = barplot(subjMed, col=c("ivory1", "grey63"), main="", xlab="Condition, Word Naming Inverted Trials", ylab="RT in Milliseconds", ylim=c(0, max(subjMed)+max(sErrs2)+100))
segments(conBars, subjMed-sErrs2, conBars, subjMed+sErrs2)

# Reformat data for ANOVA
found = which(upWordRTmed !=-999, arr.ind=T)
rtANOVA = data.frame(cbind(found, upWordRTmed[found]))
names(rtANOVA) = c('Subject', 'Condition', 'RT')

rtANOVA$Subject = factor(rtANOVA$Subject)
rtANOVA$Condition = factor(rtANOVA$Condition)

# Run the ANOVA
upWordAOV=aov(rtANOVA$RT~rtANOVA$Condition+Error(rtANOVA$Subject))
summary(upWordAOV)

# Paired t-tests to compare conditions
t.test(upWordRTmed$Congruent, upWordRTmed$Incongruent, paired=T, mu=0, alternative="two.sided", var.equal=T)

# make bar plot for accuracy
# looking for evidence of speed-accuracy tradeoff
# in incongruent condition, people were slower, but should be >= accuracy in 'easier' congruent trials
# Plot all Inverted Word trials on a single histogram for Accuracy
# Substitute $Hit for $RT
# To plot a histrogram just use the hist() function

# select all trials where the response to was to Word
WordResp=lab3data[lab3data$"Response Type"==WORD, ]

# get the trials where the stimulus was Inverted
upResp=lab3data[lab3data$Rotation==ROTATED, ]

# logical combination of Inverted and Word responses 
upWord=lab3data[lab3data$"Response Type"==WORD & lab3data$Rotation==ROTATED, ]

# Calculate mean and median of reaction time by condition
accbyCondmean=tapply(upWord$Hit, list(upWord$"Subject", upWord$"Trial Type"), mean)
accbyCondmed=tapply(upWord$Hit, list(upWord$"Subject", upWord$"Trial Type"), median)

# Put mean and median into data frames
accbyCondmean=as.data.frame(accbyCondmean)
names (accbyCondmean)=c("Congruent", "Incongruent")

accbyCondmed=as.data.frame(accbyCondmed)
names (accbyCondmed)=c("Congruent", "Incongruent")

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
subjMeanAcc=subjMean*100
errLow=(subjMean-sErrs)*100
errHigh=(subjMean+sErrs)*100
dev.new()
conBars = barplot(subjMeanAcc, col=c("ivory1", "grey63"),
main="", xlab="Condition, Word Naming Inverted Trials", ylab="Mean Percent Accurate", ylim=c(0, 100))
segments(conBars, errLow, conBars, errHigh)









