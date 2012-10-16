outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")

org = par(mfcol=c(3,1))

#
outcome[,11] <- as.numeric(outcome[,11])


title = paste(c("Heart Attack x=", mean(outcome[,11],na.rm=T)),sep="")
hist(outcome[,11],xlab="30-day Death Rate",main=title )
abline(v=median(outcome[,11],na.rm=T),lwd=2,col="red")

#
outcome[,19] <- as.numeric(outcome[,19])

title = paste(c("Heart Failure x=", mean(outcome[,19],na.rm=T)),sep="")
hist(outcome[,19],xlab="30-day Death Rate",main=title)
abline(v=median(outcome[,19],na.rm=T),lwd=2,col="red")
#
outcome[,23] <- as.numeric(outcome[,23])

title = paste(c("Pneumonia X=", mean(outcome[,23],na.rm=T)),sep="")
hist(outcome[,23],xlab="30-day Death Rate",main=title)
abline(v=median(outcome[,23],na.rm=T),lwd=2,col="red")
par(org)



### 
outcome2 <- subset(outcome, State %in% names(table(outcome$State) [table(outcome$State) > 20]))




death <- outcome2[,11]
outcome2$State <- factor(outcome2$State)
state <- outcome2$State

state <- reorder(outcome2$State,death,na.rm=TRUE)

state.mean = tapply(outcome2[,11],outcome2$State,mean,na.rm=TRUE)
state.mean = sort(state.mean)

boxplot(death~state,data=outcome2,ylab="30-Day Death Rate",main="Heart Attack 30-day by State")

###

outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")

hospital <-read.csv("hospital-data.csv",colClasses="character")

outcome.hospital <- merge(outcome,hospital,by="Provider.Number")

rm(outcome)
rm(hospital)

death <- as.numeric(outcome.hospital[,11])
npatient <- as.numeric(outcome.hospital[,15])
owner <- factor(outcome.hospital$Hospital.Ownership)

library(lattice)

xyplot(death~npatient|owner,groups=owner,
       xlab="Number of Patients Seen", ylab = "30-day Death Rate")






