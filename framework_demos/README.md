# Overview of SANDRA Analysis Framework demo Files
File | description
------ | -----------
Install SANDRA (GitHub master).R | Installs SANDRA (run this once when setting up a new analysis framework)
Load SANDRA.R | Loads SANDRA (run this every time you want to use an existing analysis framework)
scripts/0.t.1.a Decode JASMIN1.R | Decodes JASMIN1 data encoded in a LOTUS results file into metadata and trialdata. See: sandra::decodeJasmin1
scripts/0.t.2 Calculate Scores.R | Calculates scores from trial data. See: sandra::calculateScores
scripts/0.t.3 Join and Widen.R | Joins scores together into one file (with one row per participant)

original/jasmin1_data.csv | JASMIN1 encoded data of one participation in a Visual Probe Task
original/tests.trialdata.aat.xlsx | Trial data used for scoring unit tests
original/tests.medians.aat.xlsx | Manually calculated scores from tests.trialdata.aat.xlsx, used for scoring unit tests

data used for scoring unit tests