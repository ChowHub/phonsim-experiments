library(pander)
library(knitr)

# Report for Experiments 1-4 
anova_to_md = function(exp, conds){
  rmarkdown::render('0_anovas.Rmd', 
                    output_file=paste0(exp, '.md'),
                    params=list(
                      conds=conds, 
                      title=paste(exp, " - ", paste0(conds, collapse=", "))
                      )
                    )
}

exps = list(e1 = c('Ospan.reg', 'spOspan.noVer'),
            e2 = c('R.scramb', 'Ospan.scram.noVer'),
            e3 = c('R.pool10', 'R.pool15', 'R.pool30', 'R.pool60', 'R.pool350'),
            e4 = c('Rspan.names.long', 'Rspan.names.short'))

anova_to_md(names(exps)[1], exps$e1)
lapply(names(exps), function(exp_name) 
  anova_to_md(exp_name, exps[[exp_name]])
)

# Report for MLM ------------------------------------------------------------
opts_knit$set(base.dir="./", root.dir="../")

library(rmarkdown)
dv_var = "ACC.ser"
mlm_to_md = function(dv_var){
  rmarkdown::render('1_mlm.R',
                    output_file=paste0(dv_var, '.html'),
                    params=list(dv_var=dv_var, nsim=10000))
}

lapply(c("ACC.ser", "ACC.item", "ACC.order"), mlm_to_md)

# Set back knitr defaults
opts_knit$restore()