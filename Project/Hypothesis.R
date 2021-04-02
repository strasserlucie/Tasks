#Human cancer cells have higher Wnt expression than other human cells 

setwd("~/Desktop/Evolution/Tasks/Project")
#mydata <- read.csv('reformattedData.csv')
worm <- read.table('Selection_exp_data.txt', skip=1)
colnames(worm) <- c('Pop', 'Treat', 'Gen', 'prop_adults', 'juveniles')

boxplot(worm[,'juveniles']~worm[,'Treat'], boxwex=0.25, col='white', ylab='juveniles', xlab='')

Zero <- which(worm$Treat == 'Zero')
Low <- which(worm$Treat == 'Low')
High <- which(worm$Treat == 'High')

par(mfrow=c(1,3), las=1, mar=c(5,4,3,1))
plot(worm[Zero,'Gen'], worm[Zero,'juveniles'], pch=16, xlim=c(1,10), ylim=c(300,3300), main='Zero', xlab='generation', ylab='L2 and L3 stage larvae')
plot(worm[Low,'Gen'], worm[Low,'juveniles'], pch=16, xlim=c(1,10), ylim=c(300,3300), main='Low', xlab='generation', ylab='')
plot(worm[High,'Gen'], worm[High,'juveniles'], pch=16, xlim=c(1,10), ylim=c(300,3300), main='High', xlab='generation', ylab='')



