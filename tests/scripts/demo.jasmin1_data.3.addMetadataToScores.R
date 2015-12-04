# ***********
# *** Calculates d-scores from trialdata
# Demo needs this dataset: interim/jasmin1_data.vpt.csv
# Which is produced by: scripts/demo.jasmin1_data.decodeJasmin1.R

# *** Start of config
fileScores   = "jasmin1_data.vpt.scores.csv";
fileMetadata = "jasmin1_data.metadata.csv";
# *** End of config

# Read data
dsScores   = io$readData( fileScores );
dsMetadata = io$readData( fileMetadata );

# Merge
dsMerged = sandra$leftMerge(
  dsScores,
  dsMetadata,
  c( "set_id" )
)

# Store data
io$writeData(
  io$addPostfix( fileScores, "merged" ),
  dsMerged
);
 