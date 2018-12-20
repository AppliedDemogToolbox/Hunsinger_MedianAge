##############################################################################################################################
##############################################################################################################################
##R CODE TO CALCULATE THE MEDIAN AGE FOR A GIVEN AGE PROFILE
##
##EDDIE HUNSINGER, JANUARY 2008 (LAST UPDATED DECEMBER 2018)
##http://www.demog.berkeley.edu/~eddieh/
##edyhsgr@gmail.com
##
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS FUNCTION/SCRIPT, BE SURE TO CITE THE SOURCE
##
##EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R
##
##THERE IS NO WARRANTY FOR THIS CODE
##############################################################################################################################
##############################################################################################################################

##Median age is calculated with this function based on the population by age group ("agegroups") 
##and the size of the age groups in years ("agesize")
medage<-function(agegroups,agesize)
{
cat<-cumsum(agegroups)
fish<-((cat[length(agegroups[,]),1]))/2
foot<-t(agegroups)
foot<-array(0,length(foot))
hand<-foot
for (i in 2:length(foot)){if(cat[i,1]>fish) {foot[i]<-cat[i-1,1]}}
for (i in 2:length(hand)){if(cumsum(foot[i-1])==0) {hand[i]<-foot[i]}}
leg<-seq(0,length(agegroups[,]),1)
arm<-array(0,length(foot))
for (i in 2:length(hand)){if(hand[i]!=0) {arm[i]<-leg[i]}}
head<-array(0,length(foot))
for (i in 2:length(hand)){if(arm[i]!=0) {head[i]<-agegroups[i,1]}}
air<-fish-sum(hand)
water<-(air/sum(head))+sum(arm)
water*agesize
}

##Read the table of data into R
data<-read.table(file="https://github.com/AppliedDemogToolbox/Hunsinger_MedianAge/raw/master/K05.csv",header=TRUE,sep=",")
data

##Calculate median age for the first column of data
datamedianage<-medage(data[1],5)
datamedianage

##Or calculate median age for each column of data
datamedianage<-array(0,length(data[,]))
for (i in 1:length(datamedianage)) {datamedianage[i]<-medage(data[i],5)}
datamedianage

#write.table(###, file="G:/###/###.csv", sep=",")

