# Overview of SANDRA Analysis Framework demo Files
File | description
------ | -----------
Install SANDRA (from GitHub).R | Installs SANDRA (run this once when setting up a new analysis framework)
Load SANDRA.R | Loads SANDRA (run this every time you want to use an existing analysis framework)
README.md | This README file
scripts/t.1.a Decode JASMIN1 in LOTUS.R | Script for decoding JASMIN1 data embedded in a LOTUS results file into different output tables for trial, slideshow, and screen data.
scripts/t.1.b Decode JASMIN2 in LOTUS.R | Script for decoding JASMIN2 data embedded in a LOTUS results file into different output tables for trial, slideshow, and screen data.
scripts/t.1.c Decode SPRIF1 in LOTUS.R | Script for decoding SPRIF data embedded in a LOTUS results file into metadata and trialdata.
scripts/t.2.a Calculate Scores.R | Calculates d-scores and difference-of-medians from trial data.
scripts/t.2.c Calculate Difference of Medians Reliability.R | Calculates randomized repeated split-halve reliability of d-scores and difference-of-medians
scripts/t.3 Join and Widen.R | Joins scores together into one file (with one row per participant)
scripts/z. Unit Tests Difference of Medians.R | Unit test of difference-of-medians scoring against pre-calculated scores
scripts/z. Unit Tests D-Scores.R | Unit test of d-scoring against pre-calculated scores
original/jasmin1_data.csv | JASMIN1 encoded data of one participation in a Visual Probe Task (VPT)
original/jasmin2_data.csv | JASMIN2 encoded data of Approach Avoidance Task (AAT), Valence and Approach/Avoidance Single Category Implicit Association Task (SCIATs), and Go/Nogo of one participant across two sessions 
original/tests_dscores.scores.iat.csv | Manually calculated scores, used for for d-score unit tests
original/tests_dscores.trialdata.iat.csv | Trial data used for d-score unit tests
original/tests_medians.scores.aat.csv | Manually calculated scores, used for difference-of-medians unit tests
original/tests_medians.trialdata.aat.csv | Trial data used for difference-of-medians unit tests
meta/splithalve\_is\_1.trialdata.vpt.xlsx | Spreadsheet used to generate test data with a perfect split halve reliability
meta/tests_dscores.scores.iat.xlsx | Spreadsheet containing manually calculated scores
meta/tests_dscores.trialdata.iat.xlsx | Spreadsheet containing manually calculated scores
meta/tests_medians.scores.aat.xlsx | Spreadsheet containing manually calculated scores
meta/tests_medians.trialdata.aat.xlsx | Spreadsheet used to generate test data

# Script prefix scheme
The data processing scripts are prefixed as follows: <prefix1>.<prefix2>.<prefix3>
* Prefixes that are numbers need to be executed in order (e.g. first step t.1, then step t.2)
* Prefixes that are letters can be executed independently of one another (e.g. you can first do step t.2.a, then step t.2.b, or vice versa).