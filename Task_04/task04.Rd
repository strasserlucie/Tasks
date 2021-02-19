source("http://jonsmitchell.com/code/fxn05.R")

III) Simulation

Popl<- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Popl), Popl[,1], ylim=c(0, 1), type = "l" , xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Popl), Popl[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit( nruns = 10, n = 50, ngens =100, init_p = 0.5, h = 1, s = 0 )


V) What is a test statistic?

Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq<- sum( ( ( Expectation - Observed ) ^ 2 ) / Expectation )
barplot(rbind(Expectation, Observed), beside =T, main = bquote( chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))



Expectation <- c(10, 10, 10, 10)
Observed <- c(2, 3, 10, 30)
Chisq<- sum( ( ( Expectation - Observed ) ^ 2 ) / Expectation )
barplot(rbind(Expectation, Observed), beside =T, main = bquote( chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
The Chisquared value was now 51.3.

Expectation <- c(10, 10, 10, 10)
Observed <- c(10, 10, 10, 10)
Chisq<- sum( ( ( Expectation - Observed ) ^ 2 ) / Expectation )
barplot(rbind(Expectation, Observed), beside =T, main = bquote( chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
When I observe all 10's Chisquared will be 0. 
I would say they are also 0 when all are in the same in only one category. 
The lower Chi squared the more even they are.


results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts<- results[,c("yellow","red","green","blue","black","tan")]
backrounds <- c("White","Red","Yellow","Green","Blue","Black")
backroundCol <-c("white", "#d53e4f" , "#fee08b","#abdda4","#3288bd","black")
calcChi(counts[1 ,])

Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
I would say they are pretty even when Chi squared is low. And more uneven when Chisquared is high.

#I would say the average is about 65.
------
Avg<-mean(Chisqs)
-----------------

backgroundAvgs <- tapply(Chisqs, results[,3], mean)

propSig<- length( which( Chisqs > 11.70) ) / length(Chisqs)
percSig<- round(100 * propSig)
# I would say no, it is something different.

par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las = 1, mar =c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backrounds), labels = backrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)

I just noticed I  misspelled background and used backround all the time.

counter<- 1
for (i in backrounds) {
	Data<- Chisqs[which(results[,3] == i)]
	addHist(Y=counter, Dat=Data, Color=backroundCol[counter])
	counter<- counter + 1
	}
	
	abline( v = 11.70, lty=2, lwd = 2, col='black')
	# I don't see any meaningful difference.
	
VI) Running a simulation

Simulation<- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0 "simulated")
abline(v = 11.70, lty=2, lwd=2)
#Anything above 30%.And because a final cont can hvae different variations to the original one. 

Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2<- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))

Fit<- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3<- simDraws(1e4, w= Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))

Fit<- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4<- simDraws(1e4, w= Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))

Fit<- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5<- simDraws(1e4, w= Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))

Fit<- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6<- simDraws(1e4, w= Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")

Simulation7<-c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
#The mixture shows less variation, I would say than the students. Yes i would say most stdent groups show evidence of strong selction. 

VII)
I would says omething like random selection and the computer, a statitic test. 
It is pret strong I would say. The graphs look similar to the ones I just created. I would say if a mutation occurs, Chi squared woould go up, since ther e will be more difference. 
