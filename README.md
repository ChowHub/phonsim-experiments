Phonological Similarity in Complex Span
============================================

Overview of Experiments
-----------------------

E1 to E3 correspond order-wise to the experiments in the McNamara, Moore, & Conway (2011).

* E1 - Simple word span, complex reading span, and complex reading span with TBR-words as last item in sentence.
* E2 - Long and short versions of reading span.  Short corresponds to first complex reading span in E1.  Long was used in Copeland and Radvansky.
* E3 - Reading span manipulating phonemic overlap instead of rhyming.  (should there also be word span data for this experiment?)
* E4 - Regular OSPAN.
* E7 - (A) Reading span (same as short RSPAN in E1 and E2) with words randomly shuffled within each sentence.  (B) Spelled-out OSPAN (no processing component)
* E9 - Pooled word span.
* E10 - Ospan with scrambled, spelled-out words (no verification).
* E11 - Long and short names as distractor items.


Processing and Summary Scripts
------------------------------
Scripts for processing / analysis can be found on [github](https://github.com/machow/phonsim-experiments)

Each folder has a data sub-folder with the unprocessed experimental files.
Where applicable, I exported the e-merge files to tab-seperated .txt files, then scored them using python.
This wrote a new file (generally) labeled output.txt, which was then ready to crunch in R.

A single script (`0\_processing.py`) generates the data files for the exported E-prime files.  
This can then be fed to subsequent analyses.  So, E-merge => tab-seperated => 0\_processing.py => 1\_...


Complex span tasks for these experiments have identical structures / labels.
They were preprocessed in the same way.

E2, E4, E6, E7, (E8 to E11?)..

The data for the other experiments had to be preprocessed seperately. 
See 0\_preprocess.py for details.

Complete E-dat files for:
E2, E4, E6, E7, E8, E9, E10, E11

Missing in Brooke's data..

* E1 - RT's?  
* E3 - Only have recall data
* E5 - Only have recall data
