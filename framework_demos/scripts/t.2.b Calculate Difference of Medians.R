# ************************** 
# *** START OF CONFIGURATION

# This file contains the original JASMIN1 encoded data, but the scoring algorithms will only use
# derived files such as "jasmin1_data.metadata.csv" located in the "interim" subdirectory
fileSource = "jasmin1_data.csv"; 

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
    dsMetadata,
    dsRawScores,
    c( "set_id" )
  )
  
  io$writeData(
    addPostfix( fileSource, "scores", task ),
    dsScores
  );  
}
