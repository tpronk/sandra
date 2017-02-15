# ************************** 
# *** START OF CONFIGURATION

# This file contains the original JASMIN1/JASMIN2/SPRIF1 encoded data, but the scoring 
# algorithms will only use derived files such as "jasmin2_data.sciat.csv" located in the
# "interim" folder
fileSource = "jasmin1_data.csv"; 

# Each element of this list specifies a scoring. Output files are postfixed with keys of each element
scorings = list(
  vpt.medians = list(
    # *** General scoring settings
    data         = addPostfix(fileSource, "vpt"), # File containing trialdata
    run_var      = "participation_id", # Each participation has a unique value for run_var
    resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
    rt_var       = "rt", # Response time in ms
    
    # *** Dropping and reporting
    fast_drop    = 300, # Drop RTs lower than this value
    fast_report  = 300, # Count RTs lower than this value in output file as "outlier_fast_n"
    slow_drop    = 3000, # Drop RTs higher than this value
    slow_report  = 3000, # Count RTs lower than this value in output file as "outlier_slow_n"
    sd_drop      = 1.5, # Drop RTs higher or lower than the mean +/- sd_drop times the SD
    sd_report    = 1.5, # Count RTs higher or lower than the mean +/- sd_drop times the SD as "outlier_sd_n"
    resp_drop    = c("NA", 3, 4), # Drop responses with these values
    resp_report  = c(1, 3), # Count responses with these values as "resp_1_n", "resp_3_n", etc.
    
    # *** Aggregation specific settings
    type  = "aggregation", # Type of scoring; dscore or aggregation    
    # For each participation calculate scores for trial_type == approach and trial_type == avoid
    aggregation_factors = c("patt"),
    # Scores are calculated as median RTs
    aggregation_factor_function = function( rts ) {
      return( median( rts ) )
    },
    # For each participation, calculate difference between approach and avoid scores
    aggregation_run_function = function( wide ) {
      score = wide["score.no"] - wide["score.yes"];
      return(score);
    },
    # Additional variables to report
    aux_report = c("factor_scores", "task_n"),
    
    
    # Function for filtering and/or recoding trial data before it's fed into the scoring algorithm
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      # NB - Filter on assessments blocks etc. here
      ds = ds[ds[,"block_type"] == "assess",];
      
      return(ds);
    }
  )
);
  
# ************************
# *** END OF CONFIGURATION

dsMetadata = io$readData(
  addPostfix( fileSource, "metadata" )
);

dsScores = list();
for(scoring_i in names(scorings)) {
  scoring = scorings[[scoring_i]];
  dsTrialdata = io$readData(
    scoring[["data"]]
  );
  dsTrialdata = scoring[["select_data"]](dsTrialdata);
  
  dsRawScores = calculateScores(
    scoring[[ "type" ]],
    dsTrialdata,
    scoring
  );
  
  # Merge with metadata
  dsScores = leftMerge(
    dsRawScores,    
    dsMetadata,
    c("participation_id")
  )
  
  io$writeData(
    addPostfix(fileSource, scoring_i),
    dsScores
  );  
}
