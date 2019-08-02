source("runtsir.R")
source("param.R")

reslist <- vector('list', ntrial)

for (i in 1:ntrial) {
	print(i)
	
	reslist[[i]] <- replicate(nsim, do.call("runtsir", ltab[i,]), simplify=FALSE)
}

save("reslist", file="simulation.rda")
