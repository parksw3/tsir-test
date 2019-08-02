source("param.R")

load("simulation.rda")

fitlist <- vector('list', ntrial)

for (i in 1:ntrial) {
	print(i)
	templist <- vector('list', nsim)
	
	for (j in 1:nsim) {
		dd <- reslist[[i]][[j]]
		
		fitdata <- data.frame(
			Inew=tail(dd$cases, -1)/ltab[i,]$rho,
			Iprev=head(dd$cases, -1)/ltab[i,]$rho,
			Sprev=head(dd$S, -1),
			pop=head(dd$N, -1),
			biweek=factor(head(dd$biweek, -1))
		)
		
		ff <- lm(log(Inew+1)~ biweek + log(Iprev+1)+offset(log(Sprev)-log(pop)), data=fitdata,
				 contrasts = list(biweek=contr.poly))
		
		templist[[j]] <- data.frame(
			param=i,
			sim=j,
			betamean=unname(exp(coef(ff)[1])),
			alpha=unname(tail(coef(ff), 1)),
			relbeta=unname(exp(coef(ff)[1]))/ltab[i,]$betamean,
			relalpha=unname(tail(coef(ff), 1))/ltab[i,]$alpha
		)
	}
	
	fitlist[[i]] <- do.call("rbind", templist)
}

estimation_report <- do.call("rbind", fitlist)

save("estimation_report", file="estimation_report.rda")
