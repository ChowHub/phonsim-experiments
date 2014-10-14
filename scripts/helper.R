ps.getdiff = function(subdat, dv){
  #get similar - dissimilar for each participant
  #requires scored data, such that each participant x task has 2 rows
  ddply(subdat, .(Subject, task, folder), function(df) {
    stopifnot(nrow(df) == 2)
    c(diff=df[,dv][df$trialtype == 'S'] - df[,dv][df$trialtype == 'D'])
  })
}

ps.t.test = function(dat, dv, tasks){
  dat.sub = subset(dat, task %in% tasks)
  diffs = ps.getdiff(dat.sub, dv)
  print(unique(diffs$task))
  t.test(diffs$diff ~ diffs$task)
}

ps.meanse = function(y) {
  m = mean(y)
  N = length(y)
  se = sqrt(var(y) / N)
  stopifnot(length(y) < 30)       #Crude sanity check
  data.frame(y = m, ymin = m - se, ymax = m + se, se = se, N = N)
}

ps.stat_meanse = function(...){
  stat_summary(fun.data=ps.meanse, ...)
}

#------------------------------------------------------------------------------
# PLOTS
#------------------------------------------------------------------------------

# Mappings
# scoring titles
pub_theme = theme(axis.text  = element_text(size=12),
      axis.title = element_text(size=15),
      legend.text = element_text(size=12),
      legend.title = element_text(size=12),
      panel.grid.major.y = element_line(size=.4),
      panel.grid.major.x = element_line(size=0))

pub_conds = c('Ospan.scram.noVer'= 'Ospan.scramb',
                 'R.scramb' = 'Rspan.scramb',
                 'Rspan.names.long' = 'long.names', 'Rspan.names.short' = 'short.names')

library(ggplot2)
d = read.csv('Data/1_scored.csv') #stand in
h_dodge = position_dodge(width=.05, height=0)
p.summary = ggplot(d, aes(task, y, ymin=ymin, ymax=ymax, group=trialtype, color=trialtype, shape=trialtype)) +
  geom_point(position=h_dodge) +
  geom_line(position=h_dodge, lty=2) +
  geom_errorbar(position=h_dodge, width=.1) + 
  scale_y_continuous(breaks=seq(0,1,.2), limits=c(0,1)) +
  theme_bw() + pub_theme

#p.summary = ggplot(d, aes(trialtype, ACC.ser, group=task, color=task), position=h_dodge) + ps.stat_meanse(geom='errorbar', width=.1, position=h_dodge) + 
#              ps.stat_meanse(geom='point', position=h_dodge) +
#              ps.stat_meanse(geom='line', lty=2, position=h_dodge)

