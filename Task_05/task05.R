III) 
library("coala")
library("phytools")

model<-coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
feat_mutation(10) +
feat_recombination(20) +
sumstat_trees() +
sumstat_nucleotide_div()

stats <- simulate(model, nsim = 1) 

Diversity <- stats$pi

Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
Question 6. Because it doesnt represent the number of individuals. 

Agel <- max(nodeHeights(t1))

t2<- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
#No the second graph goes all the way up to 1 on the x axis.
Question 7: They do not match. the x axis goes up to 1 in graph 2 and the y axis is a little different as well.

par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()

compare.chronograms(t1, t2) 

t1_1<- read.tree(text=stats$trees[[1]][1])
t1_2<- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)

------ explain

for (locus in 1:Nloci)    { 
	ntrees<- length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus == 1 && n ==1) {
	outPhy<-read.tree(text=stats$trees[[locus]][n])
		  }
		  else  {
		  	outPhy<- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n])) 		  
	}
   }
 }

par(mfrow=c(1,1))
densityTree(outPhy)



model<-coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
feat_mutation(20) +
feat_recombination(50) +
sumstat_trees() +
sumstat_nucleotide_div()

stats <- simulate(model, nsim = 1) 

Diversity <- stats$pi

Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()


Agel <- max(nodeHeights(t1))

t2<- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()


par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()

compare.chronograms(t1, t2) 

t1_1<- read.tree(text=stats$trees[[1]][1])
t1_2<- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)



for (locus in 1:Nloci)    { 
	ntrees<- length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus == 1 && n ==1) {
	outPhy<-read.tree(text=stats$trees[[locus]][n])
		  }
		  else  {
		  	outPhy<- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n])) 		  
	}
   }
 }

par(mfrow=c(1,1))
densityTree(outPhy)
# I changed the recombination to 50, and I predicted that it wont be as wide spreat as when it was 10. 

model3<- coal_model(10, 50) +
feat_mutation(par_prior("theta", sample.int(100, 1))) +
sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)

mean_pi <- sapply(stats, function(x) mean(x$pi))
theta<- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi, theta)
abline(lm(mean_pi ~ theta))




II) 
library(learnPopGen)
?coalescent.plot
coalescent.plot(n=20,ngen=30,col.order="alternating")
		object<-coalescent.plot()
		print(object)
		plot(object)
1) Simmulation 1 started with about 5 alleles, Simmulation 2 with about 13 and Simmulation 3 with about 17. I changed the number of individuals.
2) on Average about 10 generations. 
3) -------
4) I would say none, since I cannot set it up
5) No it is not alive in generation 0.  




1)coalescent.plot(n=20,ngen=10,col.order="alternating")
> 		object<-coalescent.plot()
		print(object)


2)
coalescent.plot(n=15,ngen=30,col.order="alternating")
		object<-coalescent.plot()
		print(object)
		plot(object)
		
3)
coalescent.plot(n=25,ngen=30,col.order="alternating")
		object<-coalescent.plot()
		print(object)
		plot(object)
		