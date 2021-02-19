trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2)

They are a little different. 2 as a lower mean compared to one, which means the number in the sample are most likely to be lower(, or there are just a lot of low outliers.) 1 has a mean slightly above 5, so compared t 4 it is higher. 



#.III
source("http://jonsmitchell.com/code/simFxn04.R")
head(Grandpa)   <--- what??? 
nrow()

MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")

Alan<- makeBaby(PatGrandma, PatGrandpa)
Brenda<- makeBaby(MatGrandma, MatGrandpa)

ToMom <- length( grep("mom" , Focus ) ) / length( Focus )
There should be about 25% from each Grandparent be. 

ToMomMom<- length( grep( "grandma_mom", Focus ) ) / length( Focus )
ToMomDad<- length( grep( "grandpa_mom" , Focus ) ) /length( Focus )
I would say it is equally related to each grandparent ( maternal and paternal). To all 4 grandparents I would say it is about 25%.  (Was I supposed to se a graph or something here? Because I didn't). 

Sibling_01<- makeBaby(Brenda, Alan)
I would say Focus shares most of the DNA with him. Not all of the siblings share the same amount with each other, but I would says still a decent amount.

ToSib <- length( intersect( Focus, Sibling_01 ) ) /length( Focus )

I would say he shares each time a different amount of genes. As I said earlier, it is always different how many genes are being shared by siblings.
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan))) / length( Focus))

quantile(ManySiblings)
mean(ManySiblings)
plot( density(ManySiblings), main="" , xlab="proportion shared genes")
The range of values vary, because it is always different what genes and how the genes are received from the parents. 



# IV.
HWE<- function(p)  {
  aa<- p^2
  ab<- 2 * p * (1-p)
  bb<- (1-p) ^2
  return(c(aa=aa , ab=ab , bb=bb))
}
HWE( 0.5 )
plot(1, 1, type="n" , xlim=c(0, 1) , ylim=c(0, 1) , xlab="freq. allele a", ylab="geno. freq")
p<- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
They frequency should increase. As it decreases ther will be less aa individuals. 

lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legen=c("aa","ab","bb"), col=c("red","purple","blue"), lty=1, lwd=2, bty="n")
the red dots are more spread, its more spreat since we use a smaller population. It won't be as accurate as with a bigger population.  I dont't think it is shown on the graph. No geographic space


Pop<- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
No I don't think it matches HW, because the graph had majorty aa instead of ab'

Pop<- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")


# V.
install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
the graph is recorded between .8 to .1 and includes multiple increases and decreases 

x <- genetic.drift(Ne=400, nrep=5, pause=0.01) 
to the data falling between .2 and .8 but there are still many increases and decreases
x <- genetic.drift(Ne=700, nrep=5, pause=0.01)  # this shrunk the data to between .3 and .8 there are also not as drastic increases and decreases

 PopSizes <- 5:50
  makes a bunch of populations of different sizes between 5 and 50 individuals 
Samples <- 5:50 

say that there are 5 populations with each given size		
#simulate all 230 of populations and get the time one of the two allels went extincit 
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)  # lm() used to fit a line to data 
summary(Line)
Line$coef # 
extract coefficients use: $coef
plot(Samples, tExt)
abline(Line)
graph has a gradual increasing line with data plots throughout the graph the highest at 45 samples at tExt 400 

to compare
Line2 <-lm(tExt~Samples+0)
summary(Line2)
Line2$coef
plot(Samples, tExt)
abline(Line2)



