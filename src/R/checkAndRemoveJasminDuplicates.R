# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Checks JASMIN1 and JASMIN2 data for duplicates 
#'
#' @export
#' @param evlogs                 (data.frame) JASMIN1 or JASMIN2 trial data
#' @return  (list) Element "evlogs" contains deduplicated data; Element "sequence_report" contains a report about whether anything suspicious was found. See "2. Decoding Trial Data, Scoring Tasks, and Widening" for information about the sequence_report
#' @family SANDRA
checkAndRemoveJasminDuplicates = function( evlogs, snVar = "logger_sn", timeVar = "logger_time" ) {
  # sequence_report: short list of strange things in sequence numbers
  sequence_report = c();
  
  # Setup column types
  numeric_columns = c( snVar, timeVar );
  for( i in numeric_columns )
  {
    evlogs[ ,i ] = as.numeric( as.character( evlogs[ ,i ] ) );
  }
  
  # Sort sequence
  seq_sorted = sort( evlogs[ ,snVar ] );
  evlogs = evlogs[order(evlogs[,snVar]),]
  
  #order(evlogs[,snVar])
  # Check for duplicates or missing via lagged difference
  seq_diff = diff( seq_sorted );
  
  # Any difference == 0? Report duplicates
  duplicates_to_remove = c();
  if( sum( seq_diff == 0 ) > 0 )
  {
    duplicates = seq_sorted[ which( seq_diff == 0 ) ]
    #print( paste("checkAndRemoveJasminDuplicates. Duplicate sns for: ", paste( unique( duplicates ), collapse=", " ) ) );
    # Check if the duplicates are consistent (have the same value)
    inconsistents = c();
    for( i in 1 : length( duplicates ) )
    {
      inconsistent_found = F;
      
      duplicate_evlogs = evlogs[ evlogs[ ,snVar ] == duplicates[i], ]
      # Ignore RunID column
      duplicate_evlogs = data.frame.dropVar( duplicate_evlogs, "RunID" )
      
      duplicates_to_remove = c(
        duplicates_to_remove,
        which(evlogs[ ,snVar ] == duplicates[i])[-1]
      );
      # Check row 1 with 2, 2 with 3 etc.
      for( j in 1 : ( nrow( duplicate_evlogs ) - 1 ) )
      {
        # If any column differs, they are inconsistent
        if( sum( duplicate_evlogs[ j, ] == duplicate_evlogs[ j + 1, ] ) < ncol( duplicate_evlogs ) )
        {
          inconsistent_found = T;
        }
      }
      
      if( inconsistent_found )
      {
        inconsistents = c( inconsistents, duplicates[i] );
      }
    }
    
    # Log if any are inconsistent
    if( length( inconsistents ) > 0 )
    {
      print( paste("checkAndRemoveJasminDuplicates. Inconsistent data for sns: ", paste( unique( inconsistents ), collapse=", " ) ) );
      print(evlogs[1,]);
      sequence_report = c( sequence_report, "inconsistent (", length(unique(inconsistents)), ")" );
    }
  }
  
  # Any difference > 1? Report missing
  if( sum( seq_diff > 1 ) > 0 )
  {
    missing = seq_sorted[ which( seq_diff > 1 ) ]
    missing = paste( unique( missing ), collapse=", " );
    print( paste("checkAndRemoveJasminDuplicates. Missing sns after: ", missing ) );
    sequence_report = c( sequence_report, "missing" );  
  }
  
  # Sort sns by time and compute relative time
  evlogs = evlogs[ order( evlogs[ ,snVar ] ), ];
  evlogs[ ,"rel_time" ] = evlogs[ ,timeVar ] - evlogs[ 1,timeVar ];
  
  # Check on negative time differences
  if( sum( evlogs[ ,"rel_time" ] < 0 ) > 0 )
  {
    neg_time = seq_sorted[ which( evlogs[ ,"rel_time" ] < 0 ) ];
    neg_time = paste( unique( neg_time ), collapse=", " );
    print( paste("checkAndRemoveJasminDuplicates. Negative time difference at sns: ", neg_time ) );
    sequence_report = c( sequence_report, "negtime" );
  }
  
  sequence_report = paste( sequence_report, collapse = " " );
  
  if (!is.null(duplicates_to_remove)) {
    evlogs = evlogs[-duplicates_to_remove,];
  }

  return( list( 
    sequence_report = sequence_report,
    evlogs = evlogs
  ) );
}