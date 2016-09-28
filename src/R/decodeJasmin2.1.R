# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Decodes JASMIN2 data stored in a LOTUS results file
#'
#' @export
#' @param ds                 (data.frame) LOTUS results file 
#' @param verbose            (logical) If TRUE, then print debug output
#' @return  (list) Decoded data
#' @family SANDRA
#' @examples
#' See: SANDRA/framework_demos/scripts/t.1.c Decode JASMIN2 in LOTUS.R
#' See: SANDRA/tutorials/2. Decoding Trial Data, Scoring Tasks, and Widening.docx
decodeJasmin2 = function (ds, tables, verbose=FALSE) {
  decode_task_start = function( ds_current ) {
    # json encoded data
    value_json = ds_current[ 1, "Value" ];
    # decode to list
    value_list = fromJSON( value_json );
    # init result, if needed
    result = data.frame.new( names( value_list ) );
    # Create column denoting all combinations of user and runid
    ds_current[ ,"set" ] = apply(
      ds_current,
      1,
      function( cur_row ) {
        return( paste( cur_row[[ "UserID" ]], "_", cur_row[[ "RunID" ]], sep = "" ) );
      }
    )
    
    result = niceBy(
      ds_current,
      c( "set" ),
      function( result, cur_row ) {
        if( nrow( cur_row ) > 1 ) {
          print( "decode_task_start WARNING" );
        }
        # json encoded data
        value_json = cur_row[[ "Value" ]];
        if( value_json != "" ) {
          # decode to list
          value_list = fromJSON( value_json );
          # Add to result
          for( i in names( value_list ) ) {
            result[[ i ]] = value_list[[ i ]];
          }
        } else {
          print("invalid task_start")
          #result[[ "userAgent" ]] = "";
          #result[[ "UserID"    ]] = "";
        }
        # Add participation_id (=RunID of this event)
        result[[ "participation_id" ]] = cur_row[["RunID"]];
        
        # Add additional columns
        for (column in names(ds_current[,names(ds_current) != "Value"])) {
          result[[column]] = cur_row[[column]];
        }
        return( result );
      }
    );
    
    return( result );
  }
  
  decode_jasmin = function( ds_current, timer ) {
    if (nrow(ds_current) == 0) {
      return(NULL);
    }
    # json encoded data
    value_json = ds_current[ 1, "Value" ];
    # decode to list
    value_list = fromJSON( value_json );
    # init result, if needed
    result = data.frame.new( c( 
      value_list[[1]],
      "UserID",
      "RunID"
    ) );
    if (verbose) {
      print( "Row for establishing columns: " );
      print( ds_current[ 1, ] );
    }
    ds_current[ ,"row_counter" ] = 1 : nrow( ds_current );
    
    result = niceBy(
      ds_current,
      c( "row_counter" ),
      function( result, ds_subset ) {
        timer$reportProgress( ds_subset[ 1, "row_counter" ] );
        # json encoded data
        value_json = ds_subset[ 1, "Value" ];
        # TP 2013-12-11: Remove doubly escaped slashes
        value_json = gsub( "\\\\", "\\", value_json, fixed = TRUE )
        # decode to list
        value_list = fromJSON( value_json );
        # decode to data.frame; the first element of the list contains the variable names
        value_df   = data.frame.new( unlist( value_list[[1]] ) );
        
        # Add each element (except the first) to results
        results = list();
        for( value_i in 2 : length( value_list ) ) {
          # Encode any special values inside of the table
          for( value_j in 1 : length( value_list[[ value_i ]] ) ) {
            # More than one element -> encode to JSON again (or escape \t and \n)
            if( length( value_list[[ value_i ]][[ value_j ]] ) != 1 ) {
              value_list[[ value_i ]][[ value_j ]] = toJSON( value_list[[ value_i ]][[ value_j ]] );
            } 
          }
          
          # which values in values_list are null?
          value_null = unlist( lapply(
            value_list[[ value_i ]],
            is.null
          ) );
          # Replace these by "NA"
          value_list[[ value_i ]][ value_null ] = "NA";
          
          # Add to results
          results[[ value_i - 1 ]] = value_list[[ value_i ]];
          names( results[[ value_i - 1 ]] ) = value_list[[1]]
          
          # Add user and runID
          results[[ value_i - 1 ]][[ "UserID" ]] = ds_subset[ 1, "UserID"  ];
          results[[ value_i - 1 ]][[ "RunID"  ]] = ds_subset[ 1, "RunID" ];    
        }
        return( results );
      }
    );
    return( result );
  }

  # Get each type of encoded table via Name
  ds_tables = unique( ds[,"Name"] );
  
  # Decode each contained table
  allResults = list();
  for( ds_table in tables ) {
    # Current table data
    ds_current = ds[ ds[ ,"Name" ] == ds_table, ];
    if (verbose) {
      print( paste( 
        "Decoding table:",
        ds_table
      ) );
    }    
    timer = ProgressTimer( 
      nrow( ds_current ), 
      paste(
        "Decoding: ",
        ds_table,
        ".",
        sep = ""
      ),
      width = 500
    );
    
    result = switch( 
      ds_table, 
      decode_jasmin( ds_current, timer ),
      task_start = decode_task_start( ds_current ),
      survey     = decode_jasmin( ds_current, timer ),
      screen     = decode_jasmin( ds_current, timer ),
      slideshow  = decode_jasmin( ds_current, timer )
    )
    if (ds_table == "task_start") {
      task_start_table <<- result;
    }
    timer$done();
    
    # Replace any \t and \n by space
    if( "value" %in% names( result ) ) {
      result[ ,"value" ] = gsub( "\t", " ", result[,"value"] );
      result[ ,"value" ] = gsub( "\n", " ", result[,"value"] );
      result[ ,"value" ] = gsub( "\r", " ", result[,"value"] );
    }

    allResults[[ds_table]] = result;
  }
  
  return (allResults);
}