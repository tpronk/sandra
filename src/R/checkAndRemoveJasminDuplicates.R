# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Checks JASMIN1 and JASMIN2 data for duplicates 
#'
#' @export
#' @param evlogs                 (data.frame) JASMIN1 or JASMIN2 trial data
#' @return  (list) Element "evlogs" contains deduplicated data; Element "sequence_report" contains a report about whether anything suspicious was found. See "2. Decoding Trial Data, Scoring Tasks, and Widening" for information about the sequence_report
#' @family SANDRA
checkAndRemoveJasminDuplicates = function( evlogs ) {
  # sequence_report: short list of strange things in sequence numbers
  sequence_report = c();
  
  # Setup column types
  numeric_columns = c( "time_log", "sn" );
  for( i in numeric_columns )
  {
    evlogs[ ,i ] = as.numeric( as.character( evlogs[ ,i ] ) );
  }
  
  # Sort sequence
  seq_sorted = sort( evlogs[ ,"sn" ] );
  
  # Check for duplicates or missing via lagged difference
  seq_diff = diff( seq_sorted );
  
  # Any difference == 0? Report duplicates
  duplicates_to_remove = c();
  if( sum( seq_diff == 0 ) > 0 )
  {
    duplicates = seq_sorted[ which( seq_diff == 0 ) ]
    #print( paste("parse_data_evlogs. Duplicate sns for: ", paste( unique( duplicates ), collapse=", " ) ) );
    # Check if the duplicates are consistent (have the same value)
    inconsistents = c();
    for( i in 1 : length( duplicates ) )
    {
      inconsistent_found = F;
      
      duplicate_evlogs = evlogs[ evlogs[ ,"sn" ] == duplicates[i], ]
      # Ignore RunID column
      duplicate_evlogs = data.frame.dropVar( duplicate_evlogs, "RunID" )
      
      duplicates_to_remove = c(
        duplicates_to_remove,
        which(evlogs[ ,"sn" ] == duplicates[i])[-1]
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
      print( paste("parse_data_evlogs. Inconsistent data for sns: ", paste( unique( inconsistents ), collapse=", " ) ) );
      sequence_report = c( sequence_report, "inconsistent" );
    }
  }
  
  # Any difference > 1? Report missing
  if( sum( seq_diff > 1 ) > 0 )
  {
    missing = seq_sorted[ which( seq_diff > 1 ) ]
    missing = paste( unique( missing ), collapse=", " );
    print( paste("parse_data_evlogs. Missing sns after: ", missing ) );
    sequence_report = c( sequence_report, "missing" );  
  }
  
  # Sort sns by time and compute relative time
  evlogs = evlogs[ order( evlogs[ ,"sn" ] ), ];
  evlogs[ ,"rel_time" ] = evlogs[ ,"time_log" ] - evlogs[ 1,"time_log" ];
  
  # Check on negative time differences
  if( sum( evlogs[ ,"rel_time" ] < 0 ) > 0 )
  {
    neg_time = seq_sorted[ which( evlogs[ ,"rel_time" ] < 0 ) ];
    neg_time = paste( unique( neg_time ), collapse=", " );
    print( paste("parse_data_evlogs. Negative time difference at sns: ", neg_time ) );
    sequence_report = c( sequence_report, "negtime" );
  }
  
  sequence_report = paste( sequence_report, collapse = " " );
  
  print( "*** parse_data_evlogs done" );
  if (!is.null(duplicates_to_remove)) {
    evlogs = evlogs[-duplicates_to_remove,];
  }

  return( list( 
    sequence_report = sequence_report,
    evlogs = evlogs
  ) );
}