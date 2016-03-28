#You can use code you wrote for the correlation exercise here.
source("DiversificationFunctions.R")
tree <- read.tree("____PATH_TO_TREE_OR_SOME_OTHER_WAY_OF_GETTING_A_TREE____")

#First, let's look at a sister group comparison. Imagine you have one clade you think is especially noteworthy. 

ntax.focal.clade <- ___________________
ntax.sister.clade <- __________________
depth.both <- ____________ #time of the MRCA
actual.ratio <- min(c(ntax.focal.clade, ntax.sister.clade)) / max(c(ntax.focal.clade, ntax.sister.clade))

estimated.div.rate <- log(ntax.focal.clade + ntax.sister.clade)/depth.both #N(t) = N0 * exp(r*t)

nsim <- 1000
sim.ratios <- rep(NA, nsim)
for (i in sequence(nsim)) {
	left.clade <- sim.bd(b=estimated.div.rate, times=depth.both)[2,2] #get the number of taxa. We're assuming a pure birth model. This is dumb: if there's one thing we know about life, it's that extinction happens. But it's convenient for this case. This is known as a Yule model.
	right.clade <- sim.bd(b=estimated.div.rate, times=depth.both)[2,2] 
	sim.ratios[i] <- min(c(left.clade, right.clade)) / max(c(left.clade, right.clade))
}

plot(hist(sim.ratios, breaks=20))
abline(v=actual.ratio)