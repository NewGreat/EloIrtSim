# Elo rating
#
# similar to the rating of chess and tennis players, 
# Elo rating can be used to progressively estimate 
# subjects abilities and item difficulties for observed test results
# 
# Elo rating is particularly interesting with progressive observations
# and with different items being administered to different subjects
#
# for more information, consult the work of M. Brinkhuis, among others.
#

# simulate data (see simIRTdata.r)
S <- round(qnorm(seq(.01,(1-.01),by=.02),0,1),3); names(S) <- paste0('s',1:length(S))
I <- seq(-2.4,2.4,by=.075); names(I) <- paste0('i',1:length(I))
D <- c(seq(.05,3.15,.1),1,seq(3.15,.05,-.1))
tst <- unlist(lapply(1:4,function(.x) seq(.x,32,by=4)))
D <- D[c(tst[32:1],33,tst)];rm(tst);names(D) <- names(I)
# probabilities for 2pl and 1pl model
P2 <- round(plogis(sweep(outer(S,I,'-'),MARGIN=2,D,`*`)),3)
P1 <- round(plogis(sweep(outer(S,I,'-'),MARGIN=2,rep(1,length(D)),`*`)),3)
dimnames(P1) <- dimnames(P2) <- list(names(S),names(I))
# simulated observations for 2pl and 1pl model
set.seed(312)
O2 <- matrix(mapply(function(r,c) rbinom(1,1,P2[r,c]), row(P2), col(P2)  ),nrow=nrow(P2),ncol=ncol(P2))
O1 <- matrix(mapply(function(r,c) rbinom(1,1,P1[r,c]), row(P1), col(P1)  ),nrow=nrow(P1),ncol=ncol(P1))
dimnames(O1) <- dimnames(O2) <- list(names(S),names(I))

# use the reshape package to reshape the data
library(reshape)
# multivariate data (subject x item) is transformed to univariate data (value per subject - item combination)
o2 <- melt(O2)
names(o2) <- c("user","item","score")

# initial values users/items/simulations/step size
n <- length(unique(o2$user))			#	number of users
k <- length(unique(o2$item))			#	number of items for each person

# assume each subject responds to the same items
# assume that the order of responses over subjects and items is random
# note that typically the Elo rating is used when items differ over users
# note that typically the Elo rating could be used for adaptive testing, in which the order is not random
set.seed(777)
n2 <- o2[sample(1:nrow(o2)),]

n2s <- vector("numeric",length=n)
names(n2s) <- unique(o2$user)
n2i <- vector("numeric",length=k)
names(n2i) <- unique(o2$item)

# the idea :
# if a subject responds to an item
# it may be wrong (0) suggesting that the subject is less proficient 
# than that the item is difficult or vise verse
# so that an incorrect response triggers an adjustment downwards for the ability
# and an adjustment upwards for the difficulty
# the adjustments will be more strong if the evidence opposed current estimates
# while evidence that corresponds to current knowledge is less influential

# the elo function implementation
runElo <- function(response,ability,difficulty,step){
 change <- step*(response-plogis(ability-difficulty))
 return(list(ability=ability+change,difficulty=difficulty-change,change=change))
}

# for all observations
for(it in 1:nrow(n2)){
	obs <- runElo(n2[it,'score']							# an observation
		,n2s[as.character(n2[it,'user'])]					# for a subject
		,n2i[as.character(n2[it,'item'])]					# on an item
		,.5*(nrow(n2)-it+1)/(it^1.2+nrow(n2))+.01)			# step size, evolving from large to small
	n2s[as.character(n2[it,'user'])] <- obs$ability			# update the ability for the observed subject
	n2i[as.character(n2[it,'item'])] <- obs$difficulty		# update the difficulty for the responded item
}

# the final estimates
cbind(n2s)
cbind(n2i)
