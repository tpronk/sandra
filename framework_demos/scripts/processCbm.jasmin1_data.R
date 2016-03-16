# *** START OF CONFIGURATION

# Task data processing takes place in three steps:
# 1. decode: converts raw JASMIN data to trial data (one file per task) and medata 
# 2. score:  calculates task scores from trial data
# 3. join:   joins task scores and metadata into one file
# Via stepFrom and stepTo you can specify which processing steps to undertake
stepFrom = "decode";
stepTo   = "join";

# Here you can setup all kinds of stuff for the processing
settings = list(
  # Original JASMIN1 data filename (filenames for all processing output)
  # is derived from this filename)
  fileSource = "jasmin1_data.csv",
  # Combinations of values in these columns identify a participation in a task
  participationID = c( "UserID", "Session" ),
  # Per task, settings for how to calculate task scores
  # For a detailed description of scoring settings, see the calculateDScores(...) documentation:
  # ?calculateDScores
  scorings = list(
    aat = list(
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
    ),
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
  ),
  # Values in this column are used to make "joined" output data wide, so that you get columns like this:
  # dscore.vpt.Session1, dscore.vpt.Session2, etc.
  sessionID = "Session",
  # This function is used to remove artefacts produced by scoring (due to errors, test cases, etc.)
  prepareForJoin = function( dsScores ) {
    return( dsScores );
  }
);

# *** END OF CONFIGURATION

io$runScript( "CbmLotusProcessor.R" );
processor = CbmLotusProcessor( settings );
result = processor$processCbm( "decode", "join" );
