#' ---
#' title: Multilevel model of accuracy
#' output: 
#'  html_document:
#'    keep_md: true
#' params:
#'  dv_var: ACC.ser
#'  nsim: 10000
#'  plot_ymax: 1
#'  plot_yshift: 0
#' ---

params = list(dv_var= 'ACC.ser', plot_ymax= 1, plot_yshift = 0)
#+ config, echo=FALSE
debug = FALSE
library(knitr)
opts_chunk$set(comment='', fig.width=14, fig.height=6.5)
opts_knit$set(self.contained=TRUE, base.dir="./", root.dir="../")

#+ setup, echo=debug
library(plyr)
library(ggplot2)
library(lme4)
library(reshape)

#' Params:
params

#' Read in data
#+ data, echo=TRUE
DV_VAR = params$dv_var

all.dat = read.csv('data/1_scored.csv')
all.dat$Subject = factor(all.dat$Subject)
all.dat$dv = all.dat[,DV_VAR]

# Remove regular ospan, which has substantially lower accuracy 
# due to verification requirements
dat = subset(all.dat, !task %in% 'Ospan.reg')

# Mark high and low interference conditions
low_int = c('spOspan.noVer', 'Ospan.scram.noVer', 'Rspan.names.long', 'Rspan.names.short', 'Ospan.reg')
dat$interference = ifelse(dat$task %in% low_int, 'low', 'high')

#' Models
#' ----------------------------------------------------------------------------
#+ models, class="nohighlight", echo=TRUE
dat$cond = paste(dat$interference, dat$trialtype)
contrasts(dat$trialtype) <- c(0,1)                # similarity increment

#' ### Model with recall predictions for each interference:trialtype explicit
fit.mlm = lmer(dv ~ 0 + cond + (1 | task:Subject) + (1 | task), data=dat)
summary(fit.mlm)

#' ### Same model contrast coded for similarity benefit
fit.mlm.con = lmer(dv ~ 0 + interference/trialtype + (1 | task:Subject) + (1 | task), data=dat)
summary(fit.mlm.con)

#' ### Same model contrast coded for interference benefit
fit.mlm.int = lmer(dv ~ 0 + trialtype/interference + (1 | task:Subject) + (1 | task), data=dat)
summary(fit.mlm.int)

#' ### Why is task variance estimated to be 0?
#' Sanity check, injecting noise at task level. Note the accurate task variance estimates.
tmp_dat = ddply(dat, .(task), transform, dv = dv + rnorm(1, sd=.1))
fit.mlm2 = lmer(dv ~ 0 + cond + (1 | task:Subject) + (1 | task), data=tmp_dat)
summary(fit.mlm2)

#' Another Sanity check, looking at task variance from ANOVA standpoint.
#' Note that the F-value for task is 1 (no between task var beyond subject var)
fit.aov = aov(dv ~ interference + task + Error(task:Subject), data=dat)
summary(fit.aov)

#' Confidence Intervals
#' ----------------------------------------------------------------------------
#+ confint, echo=debug
library(effects)
library(boot)
confint(fit.mlm, method='boot', nsim=params$nsim)
confint(fit.mlm.con, method='boot', nsim=params$nsim)

#' ### Cohen's d
#' Here, I divided group differences by either the residual variance,
#' or between-subject variance +  residual variance.
#+ cohensd, echo=debug
booted = bootMer(fit.mlm.con, function(fit) {
  # Calculate Cohen's d both for within sub and between sub variance
  # and high / low interference conditions
  rand.fx = data.frame(VarCorr(fit))
  res.var     = rand.fx[rand.fx$grp == 'Residual', 'vcov']
  res.var_bet = rand.fx[rand.fx$grp == 'task:Subject', 'vcov']
  beta_high = fixef(fit)[['interferencehigh:trialtype1']]
  beta_low  = fixef(fit)[['interferencelow:trialtype1']]
  c(d_high = beta_high / sqrt(2* (res.var)),
    d_low  = beta_low  / sqrt(2* (res.var)),
    d_sub_high = beta_high / sqrt(res.var_bet + 2*res.var),
    d_sub_low  = beta_low  / sqrt(res.var_bet + 2*res.var)
    )
},
  nsim=params$nsim)

d.ci = lapply(1:4, function(ii) boot.ci(booted, type=c('norm', 'perc'), index=ii))
names(d.ci) = colnames(booted$t)
d.ci


#' Plotting
#' ----------------------------------------------------------------------------
#' Means and Standard Errors
#+ plotprep, echo=FALSE
meanse = ddply(dat, .(task, trialtype, interference), plyr:::summarize, m  = mean(dv),
               se = sqrt(var(dv) / length(dv))
)

meanse = meanse[order(meanse$interference, decreasing=TRUE),]                  # order tasks by interference
meanse$task = factor(meanse$task, levels=unique(meanse$task)) # remove any extra task levels
meanse$trialtype = relevel(meanse$trialtype, ref='S')         # will put S first in legend

# Model predictions
fit.mean = data.frame(m.pred = fixef(fit.mlm), 
                      colsplit(names(fixef(fit.mlm)), ' ', names=c('cond', 'sim'))
                      )
fit.mean$sim = relevel(fit.mean$sim, ref='S')

# Plot All Conds
source('scripts/helper.r')
dodge = position_dodge(width=.1)
meanse$task = revalue(meanse$task, mlm_conds)
meanse$task = factor(meanse$task, levels=mlm_conds)
p = ggplot(meanse, aes(task, m, color=trialtype, shape=trialtype)) + 
  geom_hline(aes(yintercept=m.pred, linetype=cond, color=sim), alpha = .5, show.legend=FALSE, data=fit.mean) +
  geom_point(position=dodge) + geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.05, position=dodge) +
  scale_shape_manual(values=c(17,16)) + 
  scale_color_brewer(palette='Dark2') + scale_y_continuous(breaks=seq(0,1,.2), limits=c(0 + params$plot_yshift ,params$plot_ymax), expand=c(0,0)) +
  expand_limits(x=0,y=0)+
  theme_bw(base_size=40) + ggtitle(DV_VAR)

# make rectangles indicating high or low interference
YMAX = params$plot_ymax / 30 + params$plot_yshift
YMIN = 0 + params$plot_yshift
group_annot = data.frame(xmin = c(0, 4.5), xmax = c(4.5, 12.6), 
                         ymin = YMIN,   ymax = YMAX)
group_annot$text.x = with(group_annot, xmin + (xmax - xmin)/2)
group_annot$text.y = with(group_annot, ymin + (ymax - ymin)/2)
group_annot$label = c('low interference', 'high interference')

scale_args = list("Trial Type", breaks = c('S', 'D'), labels = c('Similar', 'Dissimilar'))
colors = do.call(scale_color_manual, c(scale_args, list(values=c("#377eb8", "#e41a1c"))))
shapes = do.call(scale_shape_manual, c(scale_args, list(values=c(17, 16))))

#+ plots, echo=TRUE
p + 
  geom_rect(aes(x=NULL, y=NULL, shape=NULL,xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color='black', fill='white', data=group_annot) + 
  geom_text(aes(shape=NULL, color=NULL, x=text.x, y=text.y, label=label), 
            show.legend=FALSE, data=group_annot, size=5) + pub_theme + colors + shapes +
  ylab('Serial Recall Accuracy') + xlab('Task') + ggtitle("")

ggsave('test.png',width = 12*1.25, height=5.6*1.25, units='in')
