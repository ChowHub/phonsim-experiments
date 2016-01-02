all: clean SCORE REPORT

clean: 
	rm data/1_*.csv
	rm -rf summary/results/{exps,mlm}

data/1_%.csv: SCORE 

summary/results/exps/%.md: REPORT
summary/results/mlm/%.md: REPORT

SCORE: data/drop.csv data/groups.csv data/scored_all.csv
	Rscript scripts/1_score.R

REPORT:
	cd summary && Rscript create_summary.R
	rm -rf summary/reports/{exps,mlm} && mkdir -p summary/reports/{exps,mlm}
	mv summary/e{1,2,3,4}* summary/reports/exps/
	mv summary/ACC* summary/reports/mlm/