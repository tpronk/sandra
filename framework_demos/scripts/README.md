# Overview of SANDRA Install scripts
module | description
------ | -----------
demo.jasmin1_data.1.decodeJasmin1.R | Decodes JASMIN1 VPT data encoded in file original/jasmin1_data.csv into trial data and metadata
demo.jasmin1_data.2.calculateDScores.R | Calculates d-scores from jasmin1_data.csv trial data
demo.jasmin1_data.3.addMetadataToScores.R | Merges d-scores from jasmin1_data.csv with metadata
demo.jasmin1_data.RUN_ALL_PROCESSING.R | Runs all jasmin1_data.csv processing steps in successsion