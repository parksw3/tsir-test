library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
source("../R/param.R")

load("../R/estimation_known.rda")

ltab$param <- 1:nrow(ltab)

estimation_known <- dplyr::select(estimation_known, -betamean, -alpha)

full <- merge(ltab, estimation_known, by="param")

fullcomb <- full %>%
	gather(key, value, -relbeta, -relalpha, -sim, -param) %>%
	rename(parname=key, parvalue=value) %>%
	gather(key, value, -parname, -parvalue, -param, -sim)

gcomb <- ggplot(fullcomb) +
	geom_point(aes(parvalue, value, col=key), shape=".", alpha=0.5) +
	geom_smooth(aes(parvalue, value, col=key), method="lm", se=FALSE) +
	scale_x_log10() +
	geom_hline(yintercept=1, lty=2) +
	facet_wrap(~parname,scale="free_x", nrow=1) +
	theme(
		panel.grid=element_blank(),
		strip.background = element_blank(),
		panel.spacing = grid::unit(0, "cm")
	)

ggsave("known_fig.pdf", gcomb, width=10, height=6)
