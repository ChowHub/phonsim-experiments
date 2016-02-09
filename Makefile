all: clean SCORE REPORT

deploy: 
	# may need to install mkdocs
	# pip install mkdocs
	mkdocs gh-deploy --clean

clean: 
	rm data/1_*.csv
	rm -rf summary/results/{exps,mlm}

data/1_%.csv: SCORE 

summary/results/exps/%.md: REPORT
summary/results/mlm/%.md: REPORT
summary/results/misc/%.md: REPORT

SCORE: data/drop.csv data/groups.csv data/scored_all.csv
	Rscript scripts/1_score.R

REPORT:
	cd summary && Rscript create_summary.R
	rm -rf summary/reports/{exps,mlm} && mkdir -p summary/reports/{exps,mlm,misc}
	mv summary/e{1,2,3,4}* summary/reports/exps/
	mv summary/ACC* summary/reports/mlm/
	mv summary/misc_distractor_intrusions* summary/reports/misc
