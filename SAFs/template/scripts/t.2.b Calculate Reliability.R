# ************************** 
# *** START OF CONFIGURATION

# This file contains the original JASMIN1/JASMIN2/SPRIF1 encoded data, but the scoring 
# algorithms will only use derived files such as "jasmin2_data.sciat.csv" located in the
# "interim" folder
fileSource = "jasmin2_data.csv"; 

# This suffix  identifies a "metadata" file. This suffix is appended to fileSource to 
# identify the metadata file, for example: jasmin2_data.task_start.csv
#   - "metadata" for JASMIN1 and SPRIF1 data
#   - "task_start" for SPRIF1 data
suffixMetadata = "task_start";

# This Variable identifies a participation: 
#   - "set_id" for JASMIN1 and SPRIF1 data
#   - "participation_id" for JASMIN2 data
run_var = "participation_id";

# Number of times to repeat splithalve procedure
splithalves = 10;

# Scoring settings; one per task
scorings = list(
  sciat = list(
    type         = "dscore",
    
    run_var      = run_var,
    resp_var     = "response",
    rt_var       = "rt",
    comp_var     = "block_type",
    comp_levels  = c( "tar1att1_2", "tar1att2_2" ), # Bias calculated towards first element
    
    fast_drop    = 300,
    fast_report  = 300,
    
    slow_drop    = 3000,
    slow_report  = 3000,
    
    sd_drop      = 1.5,
    sd_report    = 1.5,  
    
    resp_drop    = c( "NA", 0, 3, 4 ),
    resp_report  = c( 1, 3 ),
    
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
  ),
  aat = list(
    type = "aggregation",
    
    run_var = run_var,
    resp_var = "response",
    rt_var = "rt",
    
    fast_drop = 200,
    slow_drop = 2000,
    fast_report = 200,
    slow_report = 2000,
    resp_report = c(1),
    resp_drop = c(2,3,4,NA),
    
    aux_report = c("factor_scores"),
    
    # For each participation calculate scores for trial_type == approach and trial_type == avoid
    aggregation_factors = c( "trial_type" ),
    # Scores are calculated as median RTs
    aggregation_factor_function = function( rts ) {
      return( median( rts ) )
    },
    # For each participation, calculate difference between approach and avoid scores
    aggregation_run_function = function( wide ) {
      score = wide["score.avoid"] - wide["score.approach"];
      return(score);
    }
  )  
);

# Only use task data returned by this function
selectData = function( ds ) {
  return(ds);
}

# ************************
# *** END OF CONFIGURATION

dsMetadata = io$readData(
  addPostfix( fileSource, suffixMetadata )
);

for( task in names( scorings ) ) {
  dsTrialdata = io$readData(
    addPostfix( fileSource, task )
  );
  dsTrialdata = selectData( dsTrialdata );
  
  reliability = calculateScores(
    scorings[[ task ]][[ "type" ]],
    dsTrialdata,
    scorings[[ task ]],
    splithalves = splithalves
  );
  
  print( paste(
    task, "r =", reliability
  ) );
}