# ***********
# *** Decodes JASMIN1 data encoded in LOTUS results files into metadata and trialdata

# *** Start of config
fileSource = "jasmin1_data.csv";

scorings = list(
  vpt = list(
    run_var      = "set_id",
    resp_var     = "response",
    rt_var       = "rt",
    comp_var     = "patt",
    comp_levels  = c( "yes", "no" ), # Bias calculated towards first element
    
    fast_drop    = 300,
    fast_report  = 300,
    
    slow_drop    = 3000,
    slow_report  = 3000,
    
    sd_drop      = 1.5,
    sd_report    = 1.5,  
    
    resp_drop    = c( "NA", 0, 3, 4 ),
    resp_report  = c( 1 ),
    
    resp_correct = 1,  
    resp_penalty = c( 2 ),
    rt_penalty   = "2sd",
    
    aux_report   = c( 
      # For each compatible and incompatible block
      "correct_n", "correct_mean", "correct_sd", "penalty", "adjusted_mean",
      # Across compatible and incompatible
      "inclusive_sd",
      # Across task
      "task_n"
    )
  )
);
# *** End of config

# Read JASMIN1 data
ds = io$readData( 
  fileSource, 
  original = TRUE,
  encoding = "UTF-8"
);

# Decode 
dsDecoded = decodeJasmin1(
  ds,
  participationID = c( "UserID" ),
  verbose = F
);

# Store metadata and trialdata for each task in interim folder
io$writeData(
  addPostfix( fileSource, "metadata" ),
  dsDecoded$metadata
);
for( task in names( dsDecoded$trialdata ) ) {
  io$writeData(
    addPostfix( fileSource, task, "trialdata" ),
    dsDecoded$trialdata[[ task ]]
  );
}

# Calculate and merge per task
for( task in names( scorings ) ) {
  # Calculate scores
  dsScores = calculateDScores(
    dsDecoded$trialdata[[ task ]],
    scorings[[ task ]]
  );
  
  # Store scores
  io$writeData(
    addPostfix( fileSource, task, "scores" ),
    dsScores
  );
  
  # Merge
  dsMerged = leftMerge(
    dsScores,
    dsDecoded$metadata,
    c( "set_id" )
  )
  
  # Store data
  io$writeData(
    addPostfix( fileSource, task, "merged" ),
    dsMerged
  );
}
