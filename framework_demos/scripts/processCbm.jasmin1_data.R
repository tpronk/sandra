# Config
stepFrom = "decode";
stepTo   = "join";
  
settings = list(
  fileSource = "jasmin1_data.csv",
  participationID = c( "UserID", "Session" ),
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
  sessionID = "Session",
  prepareForJoin = function( dsScores ) {
    return( dsScores );
  }
);


# End of config
io$runScript( "CbmLotusProcessor.R" );
processor = CbmLotusProcessor( settings );
result = processor$processCbm( "decode", "join" );
