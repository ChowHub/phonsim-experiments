library(lme4)
library(reshape)
N = 1000
pre = rnorm(N)
post = rnorm(N, 1)     # difference of 1 sd from pre to post

dat <- data.frame(subject=1:N, pre = pre, post = post, diff = post - pre)
mdat <- melt(dat, id.vars = 'subject', measure.vars = c('pre', 'post'))
fit <- lmer(value ~ variable + (1 | subject), data=mdat)
head(mdat)
rand.fx = data.frame(VarCorr(fit))
resVar = rand.fx[rand.fx$grp == 'Residual', 'vcov']

sqrt(2*resVar)
sd(dat$diff)
