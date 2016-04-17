# Overview of SANDRA Analysis Framework demo Files
File | description
------ | -----------
Install SANDRA (from GitHub).R | Installs SANDRA (run this once when setting up a new analysis framework)
Load SANDRA.R | Loads SANDRA (run this every time you want to use an existing analysis framework)
README.md | This README file
scripts/t.1.a Decode JASMIN1 in LOTUS.R | Decodes JASMIN1 data encoded in a LOTUS results file into metadata and trialdata.
scripts/t.1.b Decode SPRIF in LOTUS.R | Decodes SPRIF data encoded in a LOTUS results file into metadata and trialdata.
scripts/t.2.a Calculate D-Scores.R | Calculates d-scores from trial data.
scripts/t.2.b Calculate Difference of Medians.R | Calculates difference of medians from trial data.
scripts/t.2.c Calculate Difference of Medians Reliability.R | Calculates split-halve reliability of medians from trial data.
scripts/t.3 Join and Widen.R | Joins scores together into one file (with one row per participant)
scripts/z. Difference of Medians Unit Tests.R | A unit test of difference of medians scoring
original/jasmin1_data.csv | JASMIN1 encoded data of one participation in a Visual Probe Task
original/tests_medians.trialdata.aat.csv | Trial data used for difference of medians unit tests
original/tests_medians.scores.aat.csv | Manually calculated scores, used for difference of medians unit tests
interim/splithalve\_is\_1.trialdata.vpt.csv | Test trialdata with a perfect split halve reliability
meta/tests_medians.trialdata.aat.xlsx | Spreadsheet used to generate test data
meta/tests_medians.scores.aat.xlsx | SpreadsheetI used to generate test data
interim/splithalve\_is\_1.trialdata.vpt.xlsx | Spreadsheet used to generate test data


# Script prefix scheme
The data processing scripts are prefixed as follows: <prefix1>.<prefix2>.<prefix3>
* Prefixes that are numbers need to be executed in order (e.g. first step t.1, then step t.2)
* Prefixes that are letters can be executed in parallel (e.g. you can pick step t.2.a, step t.2.b, or both)
* Complete all lower prefixes before completing higher prefixes (e.g. first 1.a.1, then 1.a.2, then 2.b)