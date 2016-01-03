# Multilevel model of accuracy


```r
debug = FALSE
library(knitr)
opts_chunk$set(comment='', fig.width=14, fig.height=6.5)
opts_knit$set(self.contained=TRUE)
```


Read in data


```r
DV_VAR = params$dv_var

all.dat = read.csv('data/1_scored.csv')
all.dat$Subject = factor(all.dat$Subject)
all.dat$dv = all.dat[,DV_VAR]

# Omit task, because Brooke thought effects were originally in other direction
# it's not clear if the data were preprocessed incorrectly (and raw data is
# not available)
dat = subset(all.dat, !task %in% 'Ospan.reg')

# Mark high and low interference conditions
low_int = c('spOspan.noVer', 'Ospan.scram.noVer', 'Rspan.names.long', 'Rspan.names.short', 'Ospan.reg')
dat$interference = ifelse(dat$task %in% low_int, 'low', 'high')
```

Models
----------------------------------------------------------------------------


```r
dat$cond = paste(dat$interference, dat$trialtype)
contrasts(dat$trialtype) <- c(0,1)                # similarity increment
```

### Model with recall predictions for each interference:trialtype explicit


```r
fit.mlm = lmer(dv ~ 0 + cond + (1 | task:Subject) + (1 | task), data=dat)
summary(fit.mlm)
```

```
Linear mixed model fit by REML ['lmerMod']
Formula: dv ~ 0 + cond + (1 | task:Subject) + (1 | task)
   Data: dat

REML criterion at convergence: -604.2

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-2.50496 -0.49645  0.01264  0.50477  2.32377 

Random effects:
 Groups       Name        Variance  Std.Dev.
 task:Subject (Intercept) 0.0112915 0.10626 
 task         (Intercept) 0.0003014 0.01736 
 Residual                 0.0041352 0.06431 
Number of obs: 368, groups:  task:Subject, 184; task, 12

Fixed effects:
           Estimate Std. Error t value
condhigh D  0.64776    0.01333   48.58
condhigh S  0.75458    0.01333   56.59
condlow D   0.78859    0.01702   46.32
condlow S   0.81071    0.01702   47.62

Correlation of Fixed Effects:
           cndhgD cndhgS cndlwD
condhigh S 0.792               
condlow D  0.000  0.000        
condlow S  0.000  0.000  0.802 
```

### Same model contrast coded for similarity benefit


```r
fit.mlm.con = lmer(dv ~ 0 + interference/trialtype + (1 | task:Subject) + (1 | task), data=dat)
summary(fit.mlm.con)
```

```
Linear mixed model fit by REML ['lmerMod']
Formula: dv ~ 0 + interference/trialtype + (1 | task:Subject) + (1 | task)
   Data: dat

REML criterion at convergence: -604.2

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-2.50496 -0.49645  0.01264  0.50477  2.32377 

Random effects:
 Groups       Name        Variance  Std.Dev.
 task:Subject (Intercept) 0.0112915 0.10626 
 task         (Intercept) 0.0003014 0.01736 
 Residual                 0.0041352 0.06431 
Number of obs: 368, groups:  task:Subject, 184; task, 12

Fixed effects:
                            Estimate Std. Error t value
interferencehigh            0.647764   0.013333   48.58
interferencelow             0.788586   0.017025   46.32
interferencehigh:trialtype1 0.106812   0.008593   12.43
interferencelow:trialtype1  0.022119   0.010718    2.06

Correlation of Fixed Effects:
            intrfrnch intrfrncl intrfrnch:1
interfrnclw  0.000                         
intrfrnch:1 -0.322     0.000               
intrfrncl:1  0.000    -0.315     0.000     
```

### Why is task variance estimated to be 0?
Sanity check, injecting noise at task level. Note the accurate task variance estimates.


```r
tmp_dat = ddply(dat, .(task), transform, dv = dv + rnorm(1, sd=.1))
fit.mlm2 = lmer(dv ~ 0 + cond + (1 | task:Subject) + (1 | task), data=tmp_dat)
summary(fit.mlm2)
```

```
Linear mixed model fit by REML ['lmerMod']
Formula: dv ~ 0 + cond + (1 | task:Subject) + (1 | task)
   Data: tmp_dat

REML criterion at convergence: -584.7

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.4891 -0.4837  0.0285  0.4961  2.3397 

Random effects:
 Groups       Name        Variance Std.Dev.
 task:Subject (Intercept) 0.011160 0.10564 
 task         (Intercept) 0.009341 0.09665 
 Residual                 0.004135 0.06431 
Number of obs: 368, groups:  task:Subject, 184; task, 12

Fixed effects:
           Estimate Std. Error t value
condhigh D  0.70185    0.03624   19.37
condhigh S  0.80867    0.03624   22.32
condlow D   0.82929    0.05048   16.43
condlow S   0.85141    0.05048   16.86

Correlation of Fixed Effects:
           cndhgD cndhgS cndlwD
condhigh S 0.972               
condlow D  0.000  0.000        
condlow S  0.000  0.000  0.977 
```

Another Sanity check, looking at task variance from ANOVA standpoint.
Note that the F-value for task is 1 (no between task var beyond subject var)


```r
fit.aov = aov(dv ~ interference + task + Error(task:Subject), data=dat)
```

```
Warning in aov(dv ~ interference + task + Error(task:Subject), data =
dat): Error() model is singular
```

```r
summary(fit.aov)
```

```

Error: task:Subject
              Df Sum Sq Mean Sq F value   Pr(>F)    
interference   1  0.886  0.8865  33.507 3.29e-08 ***
task          10  0.399  0.0399   1.508     0.14    
Residuals    172  4.550  0.0265                     
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Error: Within
           Df Sum Sq  Mean Sq F value Pr(>F)
Residuals 184  1.409 0.007658               
```

Confidence Intervals
----------------------------------------------------------------------------


```
Computing bootstrap confidence intervals ...
```

```
                                 2.5 %     97.5 %
sd_(Intercept)|task:Subject 0.09250889 0.11903519
sd_(Intercept)|task         0.00000000 0.03990784
sigma                       0.05751928 0.07096867
condhigh D                  0.62111733 0.67345914
condhigh S                  0.72815185 0.78031713
condlow D                   0.75500345 0.82201072
condlow S                   0.77674508 0.84369097
```

```
Computing bootstrap confidence intervals ...
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
control$checkConv, : Model failed to converge: degenerate Hessian with 1
negative eigenvalues
```

```
                                  2.5 %     97.5 %
sd_(Intercept)|task:Subject 0.092225059 0.11913481
sd_(Intercept)|task         0.000000000 0.03986962
sigma                       0.057698984 0.07090716
interferencehigh            0.622249320 0.67372581
interferencelow             0.755181186 0.82200029
interferencehigh:trialtype1 0.089893383 0.12356648
interferencelow:trialtype1  0.001203204 0.04290715
```

### Cohen's d
Here, I divided group differences by either the residual variance,
or between-subject variance +  residual variance.


```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
control$checkConv, : Model failed to converge: degenerate Hessian with 1
negative eigenvalues
```

```
$d_high
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 10000 bootstrap replicates

CALL : 
boot.ci(boot.out = booted, type = c("norm", "perc"), index = ii)

Intervals : 
Level      Normal             Percentile     
95%   ( 0.945,  1.395 )   ( 0.958,  1.410 )  
Calculations and Intervals on Original Scale

$d_low
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 10000 bootstrap replicates

CALL : 
boot.ci(boot.out = booted, type = c("norm", "perc"), index = ii)

Intervals : 
Level      Normal             Percentile     
95%   ( 0.0113,  0.4792 )   ( 0.0127,  0.4776 )  
Calculations and Intervals on Original Scale

$d_sub_high
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 10000 bootstrap replicates

CALL : 
boot.ci(boot.out = booted, type = c("norm", "perc"), index = ii)

Intervals : 
Level      Normal             Percentile     
95%   ( 0.6244,  0.8983 )   ( 0.6309,  0.9064 )  
Calculations and Intervals on Original Scale

$d_sub_low
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 10000 bootstrap replicates

CALL : 
boot.ci(boot.out = booted, type = c("norm", "perc"), index = ii)

Intervals : 
Level      Normal             Percentile     
95%   ( 0.0083,  0.3108 )   ( 0.0082,  0.3103 )  
Calculations and Intervals on Original Scale
```

Plotting
----------------------------------------------------------------------------
Means and Standard Errors


```
The following `from` values were not present in `x`: Ospan.reg
```

```r
p + 
  geom_rect(aes(x=NULL, y=NULL, shape=NULL,xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color='black', fill='white', data=group_annot) + 
  geom_text(aes(shape=NULL, color=NULL, x=text.x, y=text.y, label=label), 
            show_guide=FALSE, data=group_annot) + pub_theme + colors + shapes
```

```
Scale for 'colour' is already present. Adding another scale for 'colour', which will replace the existing scale.
Scale for 'shape' is already present. Adding another scale for 'shape', which will replace the existing scale.
ymax not defined: adjusting position using y instead
```

![](ACC.item_files/figure-html/plots-1.png) 


---
title: "1_mlm.R"
author: "machow"
date: "Sat Jan  2 18:42:14 2016"
---
