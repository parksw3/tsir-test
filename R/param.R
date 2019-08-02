ntrial <- 200
nsim <- 10

pars_range <- data.frame(
	min=c(20, 100000, 50, 0.1, 0.3, 0.8),
	max=c(40, 5e6, 1000, 10, 0.7, 1),
	row.names=c("betamean", "N", "theta", "i", "rho", "alpha"))

ltab <- as.data.frame(apply(pars_range,1,
							function(x) exp(seq(log(x[1]),log(x[2]),
												length=ntrial))))
set.seed(101)
ltab[] <- lapply(ltab,sample)
