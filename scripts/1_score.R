library(plyr)
source('scripts/helper.R')
# Load data
all.dat = read.csv('data/scored_all.csv', colClasses=c(TBR='character', resp='character'))
all.dat$Subject = factor(all.dat$Subject)

# Drop participants noted in log
# Note that because earlier experiments (E2, E4) have overlapping subject IDs
# we need to make sure we only drop them from the most recent experiments
drop = read.csv('data/drop.csv')
pruned = subset(all.dat, (!Subject %in% drop$subject) | (folder %in% c('E2', 'E4')))
dat = subset(pruned, trialtype != 'P')

# Rename pooled experiment, to reflect size of word pools
subcond = read.csv('data/groups.csv')
indx.pool = dat$task == 'R.poolb'
indx.table = match(dat[indx.pool, 'Subject'], subcond$subid)
levels(dat$task) = c(levels(dat$task), paste0('R.pool', unique(subcond$poolsize)))
dat[indx.pool, 'task'] = paste0('R.pool', subcond[indx.table, 'poolsize'])

# Get trial lengths
dat = ddply(dat, .(Subject, task, trialtype, trialnum), transform, triallen = max(inpos))

# Score Serial and Item recall
dat$ACC.ser = dat$resp != "" & dat$TBR == dat$resp
dat = ddply(dat, .(Subject, task, trialnum, trialtype), transform, 
            ACC.item=TBR %in% resp[resp != ""])
dat$ACC.blank = dat$TBR != "" & dat$resp == ""
dat$ACC.seqerr = dat$ACC.item & !dat$ACC.ser

# Prior list intrustions
is.pli = function(ii, TBR, resp, trialnum){
  crnt.trial = trialnum[ii]
  (crnt.trial != min(trialnum)) & # not first trial
    (resp[ii] != "") &            # not blank response
    (resp[ii] %in% TBR[trialnum < crnt.trial]) # response is TBR for prev trial
}

dat = ddply(dat, .(Subject, task), transform, 
            ACC.pli = sapply(1:length(TBR), is.pli, TBR=TBR, resp=resp, trialnum=trialnum)
)

# Aggregate across various factors
meanit = colwise(mean, .cols=grep('ACC', names(dat), value=TRUE))
scored.dat   = ddply(dat, .(Subject, task, folder, trialtype), meanit)             # across trials x trialtype, 2 rows per sub
scored.curve = ddply(dat, .(Subject, task, folder, trialtype, triallen), meanit)   # recall curves (acros trials x trialtype x triallen)

# Order Scoring (ratio of serial to item recall)
scored.dat$ACC.order = scored.dat$ACC.ser / scored.dat$ACC.item
scored.curve$ACC.order = scored.curve$ACC.ser / scored.curve$ACC.item

#Write data
write.csv(scored.dat, file='data/1_scored.csv')
write.csv(scored.curve, file='data/1_scored_curve.csv')

ps.getdiff(scored.dat, 'ACC.ser')
