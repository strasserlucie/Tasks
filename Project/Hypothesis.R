#Human cancer cells have higher Wnt expression than other human cells 

setwd("~/Desktop/Evolution/Tasks/Project")
#mydata <- read.csv('reformattedData.csv')
worm <- read.table('Selection_exp_data.txt', skip=1)
colnames(worm) <- c('Pop', 'Treat', 'Gen', 'prop_adults', 'juveniles')

boxplot(worm[,'juveniles']~worm[,'Treat'], boxwex=0.25, col='white', ylab='juveniles', xlab='')

Zero <- which(worm$Treat == 'Zero')
Low <- which(worm$Treat == 'Low')
High <- which(worm$Treat == 'High')


pdf('firstplot.pdf', height=3, width=9)
par(mfrow=c(1,3), las=1, mar=c(5,4,3,1))
plot(worm[Zero,'Gen'], worm[Zero,'juveniles'], pch=16, xlim=c(1,10), ylim=c(300,3300), main='Zero', xlab='generation', ylab='L2 and L3 stage larvae')
plot(worm[Low,'Gen'], worm[Low,'juveniles'], pch=16, xlim=c(1,10), ylim=c(300,3300), main='Low', xlab='generation', ylab='')
plot(worm[High,'Gen'], worm[High,'juveniles'], pch=16, xlim=c(1,10), ylim=c(300,3300), main='High', xlab='generation', ylab='')
dev.off()


bio <- read.table('bioassay.txt', skip=1)
colnames(bio) <- c('Pop', 'Treat', 'Gen', 'dose', 'Rep', 'L1count', 'alive', 'dead', 'padults', 'stage')

Cols <- c('green', 'blue', 'red')
Zero <- which(bio$dose == 'zerodose')
Low <- which(bio$dose == 'lowdose')
High <- which(bio$dose == 'highdose')

pdf('secondplot.pdf', height=4, width=8)
par(mfrow=c(1,2), las=1, mar=c(5,4,1,1), tck=-0.01, mgp=c(2.3, 0.25, 0))
plot(bio$Gen[Zero], bio$alive[Zero], col=Cols[1], pch=16, xlab='generation', ylab='alive', ylim=c(0,135))
abline(lm(bio$alive[Zero]~bio$Gen[Zero]), col=Cols[1])
points(bio$Gen[Low], bio$alive[Low], col=Cols[2], pch=16)
abline(lm(bio$alive[Low]~bio$Gen[Low]), col=Cols[2])
points(bio$Gen[High], bio$alive[High], col=Cols[3], pch=16)
abline(lm(bio$alive[High]~bio$Gen[High]), col=Cols[3])

plot(bio$Gen[Zero], bio$dead[Zero], col=Cols[1], pch=16, xlab='generation', ylab='dead', ylim=c(0,80))
abline(lm(bio$dead[Zero]~bio$Gen[Zero]), col=Cols[1])
points(bio$Gen[Low], bio$dead[Low], col=Cols[2], pch=16)
abline(lm(bio$dead[Low]~bio$Gen[Low]), col=Cols[2])
points(bio$Gen[High], bio$dead[High], col=Cols[3], pch=16)
abline(lm(bio$dead[High]~bio$Gen[High]), col=Cols[3])
dev.off()


Regression <- lm(bio$dead~as.factor(bio$Treat))
summary(Regression)

