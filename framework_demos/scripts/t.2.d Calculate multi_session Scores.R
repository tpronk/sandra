# ************************** 
# *** START OF CONFIGURATION

# This file contains the original JASMIN1 encoded data, but the scoring algorithms will only use
# derived files such as "jasmin1_data.metadata.csv" located in the "interim" subdirectory
fileSource = "multi_session.csv"; 

# Scoring settings; one per task
scorings = list(
  vpt = list(
    type = "aggregation",
    
    run_var = "set_id",
    resp_var = "response",
    rt_var = "rt",
    
    fast_drop = 200,
    slow_drop = 2000,
    fast_report = 200,
    slow_report = 2000,
    resp_report = c(1),
    resp_drop = c(2,3,4,NA),
    
    aux_report = c("factor_scores"),
    
    aggregation_factors = c( "patt" ),
    aggregation_factor_function = function( rts ) {
      return( median( rts ) )
    },
    aggregation_run_function = function( wide ) {
      score = wide["score.no"] - wide["score.yes"];
      return(score);
    }
  ),  
  aat = list(
    # split_var is used by this script: calculate separate scores for each level of
    # split_var, then average these together
    split_var    = "cat",
    
    type         = "dscore",
    
    run_var      = "set_id",
    resp_var     = "response",
    rt_var       = "rt",
    comp_var     = "appr",
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

# Only use task data returned by this function
selectData = function( ds ) {
  return( ds[ ds[ ,"type" ] == "assess", ] );
}

# ************************
# *** END OF CONFIGURATION

dsMetadata = io$readData(
  addPostfix( fileSource, "metadata" )
);

for( task in names( scorings ) ) {
  dsTrialdata = io$readData(
    addPostfix( fileSource, "trialdata", task )
  );
  dsTrialdata = selectData( dsTrialdata );
  
  dsRawScores = calculateScores(
    scorings[[ task ]][[ "type" ]],
    dsTrialdata,
    scorings[[ task ]]
  );
  
  # Merge with metadata
  dsScores = leftMerge(
    dsRawScores,
    dsMetadata,    
    c( "set_id" )
  )
  
  io$writeData(
    addPostfix( fileSource, "scores", task ),
    dsScores
  );  
}
