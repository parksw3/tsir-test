runtsir <- function(beta=c(1.24, 1.14, 1.16, 1.31,
							  1.24, 1.12, 1.06, 1.02, 0.94, 0.98, 1.06, 1.08, 0.96, 0.92,
							  0.92, 0.86, 0.76, 0.63, 0.62, 0.83, 1.13, 1.20, 1.11, 1.02,
							  1.04, 1.08),
					betamean=30,
					alpha=0.97,
					N=3e6,
					i=10,
					rho=0.52,
					tmax=20 * 26,
					S0=0.03*N,
					I0=0.001*N,
					meanbirth=N*1/50*14/365,
					theta=10,
					method=c("nbinom", "poisson", "deterministic"),
					seed){
	if (!missing(seed)) set.seed(seed)
	
	method <- match.arg(method)
	
	S <- I <- B <- rep(NA, tmax)
	
	S[1] <- S0
	I[1] <- I0
	
	for (t in 1:(tmax-1)) {
		j <- t %% 26
		
		if (j == 0) j <- 26
		
		size <- ifelse(I[t] > 0, I[t], 1)
		
		infection <- switch(method,
			nbinom=rnbinom(1, mu=betamean * beta[j] *  S[t] * (I[t] + i)^alpha/N, size=size),
			poisson=rpois(1, lambda=betamean * beta[j] *  S[t] * (I[t] + i)^alpha/N),
			deterministic=betamean * beta[j] *  S[t] * (I[t] + i)^alpha/N
		)
		
		if (infection > S[t]) infection <- S[t]

		B[t] <- switch(method, 
			deterministic=meanbirth,
			rpois(1, meanbirth)
		)
		
		I[t+1] <- infection
		S[t+1] <- S[t] - infection + B[t]
	}
	
	biweek <- (1:tmax) %% 26
	biweek[biweek==0] <- 26
	
	data.frame(
		time=1:tmax,
		biweek=biweek,
		birth=B,
		N=N,
		I=I,
		S=S,
		cases=emdbook::rbetabinom(length(I), prob=rho, size=round(I), theta=theta)
	)
}
