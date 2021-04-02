
library("phytools")
library("ape")
tree <-read.tree("https://jonsmitchell.com/data/anolis.tre")

plot(tree, type="fan")

#1: 82 tips

data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
data
date [,1]


#2 100 demensions, and its a list of species


svl<-setNames(data$svl, rownames(data))

Ancestors<- fastAnc(tree, svl, cars=TRUE, CI=TRUE)
fastAnc

#3 The values are stored in the tips. the C195 element can compute 95% confidence interval on ancestral state estimates
#4 The assumptions are that the function re-roots the trees

par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)

obj<- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData<- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tipl=c( "Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolus_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticepts", "Anolis_angusticeps"))

#5 

fossilNodes<-c()
nodeN<- c()

fossilNodes <- c()
nodeN <- c()
{
for(i in 1:nrow(fossilData))
i <- 1 if( i == 1) {
print(Ancestors) }
}


Node<- fastMRCA(tree, fossilData[im "tip1"], fossilData[i, "tip2"])
fossilNodes[i, "svl"]
nodeN[i]<-Node
plot(tree, cex=0.5)
for (i in 1:nrow(fossilData))	{
+ 	Node <- fastMRCA(tree, fossilData [i,"tip1"], fossilData[i,"tip2"])
+ 	fossilNodes[i] <- fossilData[i, "svl"]
+ 	nodeN[i] <- Node
+ 	nodelabels(node=Node, pch=21, bg="red", cex=1.2)
+ }



names(fossilNodes)<- nodeN

Ancestors_withFossils<- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
install.packages("geiger")

?fastAnc

#7 

#8-10


