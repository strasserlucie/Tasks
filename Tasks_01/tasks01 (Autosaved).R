Data <- read.csv ('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)

write.csv(Data, 'rawdata.csv', quote=F)
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head( berenMilk )
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
head(Feeds)
dateID<- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
head( beren3 )
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
cd~/Desktop/Evolution/Tasks
∼
