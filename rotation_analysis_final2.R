###### Lab 2 Script : Mental Rotation#######
#### Crystal Butler ####

# set working directory
setwd("/Users/interloper/cb2610/Google Drive/NYU/Courses/Cognition Lab/Lab 2")

# step 1 - read in the data
lab2data=read.table("data_lab2.txt", header=T) # heaader = T: there is a header at top of file
lab2data$hit=lab2data$stim==lab2data$resp # add a "hit" category to header
# name or rename header
lab2head=c("sub", "trial", "rot", "stim", "resp", "msec", "hit")
names(lab2data) <- lab2head

# 3 arg function: takes two columns and does mean (can take any function). "tappply" groups elements.
accuracy=tapply(lab2data$hit, lab2data$sub, mean)
print("Accuracy by subject number:")
print(accuracy)

# run a one-sample t-test using null hypothesis of .5, or chance accuracy
lab2_t=t.test(accuracy, mu=0.5, alternative="greater")
print(lab2_t)
# get standard deviation of accuracy
lab2_sd=sd(accuracy)
paste("Standard Deviation of",lab2_sd,".")

#summarize data: can be done for entire dataframe or individual columns
summary(lab2data)
summary(lab2data$msec)

#create a histogram, breaks is bins, seq is range
bins=seq(0, max(lab2data$msec) + 100, 50)
dev.new() #opens new graphics window
hist(lab2data$msec, breaks=bins)

#drop trials where RTs are excessively slow
# lab2data$msec<200000 is the selector
# calculate and print the amount of data we kept
# print new histogram
lab2_clean=lab2data[lab2data$msec<20000,]
keptData=round(c((nrow(lab2_clean)/nrow(lab2data))*100), digits = 2)
paste("The ratio of good data to bad is ", keptData, "%.")
bins=seq(0, max(lab2_clean$msec) + 100, 25)
dev.new() #opens new graphics window
hist(lab2_clean$msec, breaks=bins, main="Same Trials", xlab="RT in Milliseconds")

# filter same and mirror trials into different dataframes
SAME = 0
MIRROR = 1
samedata = lab2_clean[lab2_clean$stim==SAME,]
mirrordata = lab2_clean[lab2_clean$stim==MIRROR,]

# get RTs for same and mirror trial hits (msec)
same_true = samedata[samedata$hit == TRUE,]
mirror_true = mirrordata[mirrordata$hit == TRUE,]

# compute Pearson's r correlation between rotation and RT (msec)
same_cor=cor.test(same_true$msec, same_true$rot)
mirror_cor=cor.test(mirror_true$msec, mirror_true$rot)

# now find the best-fitting line through same and mirror data: linear regression
same_reg = lm(same_true$msec~ same_true$rot)
summary(same_reg)
mirror_reg = lm(mirror_true$msec~ mirror_true$rot)
summary(mirror_reg)

# create a scatter plot for RT and angle with regression line
same_xaxis=c(0, 180, 6) # sets up x-axis range and number of divisions
same_yaxis=c(0, 20000, 5) # sets up y-axis range and number of divisions
dev.new() #opens a new graphics window
plot(same_true$rot, same_true$msec, xaxp=same_xaxis, yaxp=same_yaxis ,
	main="Same Trials", xlab="Angle of Rotation", ylab="RT in Milliseconds");
abline(same_reg)
mirror_xaxis=c(0, 180, 6) # sets up x-axis range and number of divisions
mirror_yaxis=c(0, 20000, 5) # sets up y-axis range and number of divisions
dev.new() #opens a new graphics window
plot(mirror_true$rot, mirror_true$msec, xaxp=mirror_xaxis, yaxp=mirror_yaxis, 
	main="Mirror Trials", xlab="Angle of Rotation", ylab="RT in Milliseconds");
abline(mirror_reg)

# let's analyze the individual data for same trials now
quartz(title="Same Trials by Subject", width=8, height=10) # window title
par(mfrow=c(5,3)) # number of rows and columns
par(mar=c(4,4,2,2)) # margin bottom, left, top, right: in lines
par(mgp=c(2,1,0)) # set margins for axis title, labels, and lines

fitdata_same=c()
slopedata_same=c()
s_count=0
for (s in unique (same_true$sub)){
	plot(same_true[same_true$sub==s,]$rot, same_true[same_true$sub==s,]$msec, xaxp=same_xaxis, yaxp=same_yaxis ,
		main=paste("Subject", s, "Same Trials"), xlab="Angle of Rotation", ylab="RT in Milliseconds");
	same_reg = lm(same_true[same_true$sub==s,]$msec~ same_true[same_true$sub==s,]$rot)
	same_regSum = summary(same_reg)
	abline(same_reg)
	intercept=signif(coef(same_reg)[1], digits=8) #get intercept
	slope=signif(coef(same_reg)[2], digits=5) #get slope
	pval=signif(coef(same_regSum)[8], digits=5) # get probability
	if (pval < .0033) slope_sig="True" else slope_sig="False" # significance is .05/number of participants (15)
	if (slope > .0001) s_count=s_count+1 # for binomial test, probability of positive slope count
	fitdata_same=rbind(fitdata_same, c(intercept, slope))
	slopedata_same=rbind(slopedata_same, c(pval, slope_sig))
	sub_summary=paste("Subject", s)
	print(sub_summary)
	print(same_regSum)
}
fitdata_same=as.data.frame(fitdata_same) # create data frame
names(fitdata_same) = c("Intercept", "Slope")
print(fitdata_same)
slopedata_same=as.data.frame(slopedata_same) # create data frame
names(slopedata_same) = c("Slope p-val", "p<.0033")
print(slopedata_same)
sig_summary=paste("The total number of significant slopes is", s_count, ".")
print(sig_summary)
binom_same=binom.test(s_count, 15, p=0.5) # binomial test for slope significance
paste("The number of same trials with slopes > 0 is", s_count,".")
print(binom_same)

# let's analyze the individual data for mirror trials now
quartz(title="Mirror Trials by Subject", width=8, height=10) # window title
par(mfrow=c(5,3)) # number of rows and columns
par(mar=c(4,4,2,2)) # margin bottom, left, top, right: in lines
par(mgp=c(2,1,0)) # set margins for axis title, labels, and lines

fitdata_mirror=c()
slopedata_mirror=c()
m_count=0
for (s in unique (mirror_true$sub)){
	plot(mirror_true[mirror_true$sub==s,]$rot, mirror_true[mirror_true$sub==s,]$msec, xaxp=same_xaxis, 
		yaxp=same_yaxis ,main=paste("Subject", s, "Mirror Trials"), xlab="Angle of Rotation", ylab="RT in Milliseconds");
	mirror_reg = lm(mirror_true[mirror_true$sub==s,]$msec~ mirror_true[mirror_true$sub==s,]$rot)
	mirror_regSum = summary(mirror_reg)
	abline(mirror_reg)
	intercept=signif(coef(mirror_reg)[1], digits=8) #get intercept
	slope=signif(coef(mirror_reg)[2], digits=5) #get slope
	pval=signif(coef(mirror_regSum)[8], digits=5) # get probability
	if (pval < .0033) slope_sig="True" else slope_sig="False" # significance is .05/number of participants
	if (slope > .0001) m_count=m_count+1 # for binomial test, probability of positive slope count
	fitdata_mirror=rbind(fitdata_mirror, c(intercept, slope))
	slopedata_mirror=rbind(slopedata_mirror, c(pval, slope_sig))
	sub_summary=paste("Subject", s)
	print(sub_summary)
	print(mirror_regSum)
}
fitdata_mirror=as.data.frame(fitdata_mirror)
names(fitdata_mirror) = c("Intercept", "Slope")
print(fitdata_mirror)
slopedata_mirror=as.data.frame(slopedata_mirror)
names(slopedata_mirror) = c("Slope p-val", "p<.0033")
print(slopedata_mirror)
sig_summary=paste("The total number of significant slopes is", m_count, ".")
print(sig_summary)
binom_mirror=binom.test(m_count, 15, p=0.5) # binomial test for slope significance
paste("The number of mirror trials with slopes > 0 is",m_count,".")
print(binom_mirror)

# one-sample t-tests on intercepts and slopes of trials
tsame_int=t.test(fitdata_same[,"Intercept"], mu=0, alternative="two.sided")
print(tsame_int)
sd(fitdata_same[,"Intercept"])
tsame_slope=t.test(fitdata_same[,"Slope"], mu=0, alternative="two.sided")
print(tsame_slope)
sd(fitdata_same[,"Slope"])
tmirror_int=t.test(fitdata_mirror[,"Intercept"], mu=0, alternative="two.sided")
print(tmirror_int)
sd(fitdata_mirror[,"Intercept"])
tmirror_slope=t.test(fitdata_mirror[,"Slope"], mu=0, alternative="two.sided")
print(tmirror_slope)
sd(fitdata_mirror[,"Slope"])

# paired t-tests on intercepts and slopes between same and mirror trials
tcompare_int=t.test(fitdata_same[,"Intercept"], fitdata_mirror[,"Intercept"], paired=TRUE)
print(tcompare_int)
sd(fitdata_same[,"Intercept"])
sd(fitdata_mirror[,"Intercept"])

tcompare_slope=t.test(fitdata_same[,"Slope"], fitdata_mirror[,"Slope"], paired=TRUE)
print(tcompare_slope)
sd(fitdata_same[,"Slope"])
sd(fitdata_mirror[,"Slope"])
