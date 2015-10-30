library(pander)
library(knitr)

# Report for Experiments 1-4 
condlist = list(src.e1="c('Ospan.reg', 'spOspan.noVer')",
                src.e2="c('R.scramb', 'Ospan.scram.noVer')",
                src.e3="c('R.pool10', 'R.pool15', 'R.pool30', 'R.pool60', 'R.pool350')",
                src.e4="c('Rspan.names.long', 'Rspan.names.short')")

exp = lapply(condlist, function(conds) {
  condstr = paste(eval(parse(text=conds)), collapse=", ") # For displaying on report
  paste0('\n\n', condstr, '\n===\n\n', knit_expand('templates/anovas.Rmd'))
})
opts_knit$set(base.dir="./", root.dir="../")
knit(text = unlist(exp), output="summary.md")
Pandoc.brew(file = 'summary.md', output = 'summary.html', convert = "html")

# Report for MLM
knit2html(spin('../scripts/3_mlm.R'))

# Set back knitr defaults
opts_knit$restore()
