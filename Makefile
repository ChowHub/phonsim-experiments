all: SCORE REPORT

data/1_%.csv: SCORE 

summary/summary.html summary/3_mlm.html: REPORT

SCORE: data/drop.csv data/groups.csv data/scored_all.csv
	Rscript scripts/1_score.R

REPORT:
	Rscript scripts/2_gen_summaries.R
