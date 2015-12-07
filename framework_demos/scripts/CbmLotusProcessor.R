# Creates a CbmLotusProcessor, which keeps track of starting the analysis chain
# at a specific step abd loading any datasets not loaded yet during earlier runs
CbmLotusProcessor = function( settings ) {
  this = new.env();
  this$settings = settings;
  this$processCbm = function( ... ) { processCbm( this, ... ); };
  return( this );
}

processCbm = function( this, stepFrom = "decode", stepTo = "join" ) {
  running   = FALSE;
  
  # Decode JASMIN1 data in LOTUS results file
  if( running || stepFrom == "decode" ) {
    running = TRUE;
    if( is.null( this$dsEncoded ) ) {
      print( "processCbm - decode. Reading JASMIN1 data into dsEncoded")
      this$dsEncoded <- io$readData(
        this$settings$fileSource, 
        original = TRUE,
        encoding = "UTF-8"
      );
    } else {
      print( "processCbm - decode. JASMIN1 data already available in dsEncoded")
    }
    print( "processCbm - decode. Decoding JASMIN1 data")
    this$dsDecoded = cbmLotusDecodeResultsFile( this$settings$fileSource, this$dsEncoded, this$settings$participationID );
  }
  if( stepTo == "decode" ) { return( this$dsDecoded ); }
  
  # Calculate scores
  if( running || stepFrom == "score" ) {
    running = TRUE;
    if( is.null( this$dsDecoded ) ) {
      print( "processCbm - score. Reading trial- and metadata into dsDecoded")
      # Metadata and trialdata for each task in scoring    
      this$dsDecoded = list();
      this$dsDecoded$metadata = io$readData(
        addPostfix( this$settings$fileSource, "metadata" )
      );
      for( task in names( this$settings$scorings ) ) {
        this$dsDecoded$trialdata[[ task ]] = suppressWarnings( io$readData(
            addPostfix( this$settings$fileSource, task, "trialdata" )
          )
        );
      }      
    } else {
      print( "processCbm - score. Trial- and metadata already available in dsDecoded")
    }
    print( "processCbm - score. Calculating scores")
    this$dsScores = cbmLotusCalculateScores( 
      this$settings$fileSource, 
      this$dsDecoded, 
      this$settings$scorings 
    );
  }
  if( stepTo == "score" ) { return( this$dsScores ); }
  
  # Join scores into one file
  if( running || stepFrom == "join" ) {
    running = TRUE;
    if( is.null( this$dsScores ) ) {
      print( "processCbm - join. Reading scores into dsScores")
      this$dsScores = list();
      for( task in names( this$settings$scorings ) ) {
        filename = addPostfix( this$settings$fileSource, task, "scores" );
        if( io$existsData( filename ) ) {
          this$dsScores[[ task ]] = io$readData( filename );
        }
      }
    } else {
      print( "processCbm - join. Scores already available in dsScores")
    }
    
    print( "processCbm - join. Joining scores into one file")
    this$dsJoined = cbmLotusJoinFiles( 
      this$settings$fileSource, 
      this$dsScores, 
      this$settings$scorings,
      this$settings$prepareForJoin,
      this$settings$sessionID
    );
  }    
  if( stepTo == "join" ) { return( this$dsJoined ); }
}

cbmLotusDecodeResultsFile = function( fileSource, dsEncoded, participationID ) {
  dsDecoded = decodeJasmin1(
    dsEncoded,
    participationID = participationID,
    verbose = FALSE
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
  
  return( dsDecoded );
}

cbmLotusCalculateScores = function( fileSource, dsDecoded, scorings ) {
  # Calculate scores per task type
  dsScores = list();
  for( task in names( scorings ) ) {
    # Only calculate scores if any data is available for this task
    if( !is.null( dsDecoded$trialdata[[ task ]] ) ) {
      # Calculate scores
      dsRawScores = calculateDScores(
        dsDecoded$trialdata[[ task ]],
        scorings[[ task ]]
      );
      
      # Merge with metadata
      dsScores[[ task ]] = leftMerge(
        dsRawScores,
        dsDecoded$metadata,
        c( "set_id" )
      )
      
      io$writeData(
        addPostfix( fileSource, task, "scores" ),
        dsScores[[ task ]]
      );
    }
  } 
  
  # Return output
  return( dsScores );
}

cbmLotusJoinFiles = function( fileSource, dsScores, scorings, prepareForJoin, sessionID ) {
  dsSelected = list();
  dsWide = list();
  dsJoined = NULL;
  for( task in names( scorings ) ) {
    # Only join if any scores are available for this task
    if( !is.null( dsScores[[ task ]] ) ) {
      # Any recoding and filtering needed for join
      dsSelected[[ task ]] = prepareForJoin( dsScores[[ task ]] );
      
      # Add task prefix
      dsSelected[[ task ]] = data.frame.affixNames( 
        dsSelected[[ task ]], 
        postfix = task, 
        not = "UserID" 
      );
      
      # Convert to wide
      dsWide[[ task ]] = makeWide(
        dsSelected[[ task ]],
        c( "UserID" ),
        c( paste( sessionID, task, sep = "." ) )
      );
      
      # Store wide
      io$writeData(
        addPostfix( fileSource, task, "wide" ),
        dsWide[[ task ]]
      );  
      
      # Join on UserID
      if( is.null( dsJoined ) ) {
        dsJoined = dsWide[[ task ]];
      } else {
        dsJoined = niceMerge(
          dsJoined,
          dsWide[[ task ]],
          c( "UserID" ),
          all.x = TRUE,
          all.y = TRUE
        );
      }
    }
  }
  io$writeData(
    addPostfix( fileSource, "joined" ),
    dsJoined
  );    
}
