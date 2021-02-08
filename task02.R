setwd('~/Desktop/Evolution/Tasks/Task_02')
Data <- read.csv ('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]

Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds ,]
head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
head(Feeds)
Feeds <- which(Data$event == 'bottle')
head(Feeds)

dayID <- apply(Data, 1, function(x) paste(x[1:3],collapse='-'))
head(dayID)
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
head(dateID)
Data$age <- dateID - dateID [which(Data$event == 'birth')]
head(data)
beren2<-Data
beren3<- beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)

# start of part b
setwd('~/Desktop/Evolution/Tasks/Task_02')
beren3 <- read.csv('beren_new.csv', stringsAsFactors=F)

Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)

head(numFeeds)

cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])

berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])

head(berenCor)

berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
head(berenANOVA)

boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle" , ylab = "amount of milk consumed (oz)" )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay . pdf", height = 4, width = 4)
plot(as.numeric(names(totalFeed)) , totalFeed , type="b" , pch=16, xlab=" age in days" , ylab="ounces of milk")
abline(h=mean(totalFeed) , lty=2, col='red')
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")

Hypothesis tested for part)c) There is a relationship between the type of diaper change (  bowel movement) and amount eaten. 

Extra credit:
beren4<- beren3[Naps,]

#Start of c:

numNaps <- tapply(beren3$value[Naps] , beren3$age[Naps], length)
cor( beren3$value[Naps], beren3$age[Naps])
 cor.test(beren3$value[Naps], beren3$age[Naps])
 berenCor<- cor.test(beren3$value[Naps], beren3$age[Naps])
 summary(berenCor)
 berenANOVA<- aov(beren3$value[Naps] ~ beren3$age[Naps])
 boxplot( beren3$value[Naps] ~ beren3$age[Naps], xlab= "how old was he", ylab = "amount slept (min)")
 par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
 plot(as.numeric(names(totalNaps)), totalNaps , type="b" , pch=16, xlab="age in days", ylab="amount slept")
 