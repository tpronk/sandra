# ***********
# *** Calculates d-scores from trialdata
# Demo needs this dataset: interim/jasmin1_data.vpt.csv
# Which is produced by: scripts/demo.jasmin1_data.decodeJasmin1.R

# *** Start of config
fileTrialdata = "jasmin1_data.vpt.trialdata.csv";
original   = FALSE; # If TRUE, data is in original folder, if not, in interim folder

# d-score settings
settings = list(
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
);
# *** End of config

# Read data
ds = io$readData( fileTrialdata, original );

# Calculate scores
scores = sandra$calculateDScores(
  ds,
  settings
);

# Store data
io$writeData(
  io$addPostfix( fileTrialdata, "scores" ),
  scores
);
