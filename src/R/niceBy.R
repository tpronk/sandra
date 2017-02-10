# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Wrapper for R's native by function
#' 
#' Calls the function 'aggregation' for each unique combination of values of the the columns factors in the dataframe ds.
#' The aggregation function is called with two arguments: (1) (list) result list to add aggregation output to, and (2) (data.frame) subset of data selected by this combination of values for factors
#' If niceBy is set to return a data frame, then names of variables in this data frame 
#' are are drawn from the names of the elements in the list returned by the aggregation
#' funtion. The aggregation function can return a list of scalars if each call to 
#' aggregation produces one row of output, or it can return a list of list of scalars
#' if each call produces one or more rows of output.
#' 
#' @export
#' @param ds              (data.frame) Dataset to aggregate
#' @param factors         (vector) Columns in dataset that serve as factors
#' @param aggregation     (function) Aggregation 
#' @param result_type     (character) Three possible values: "data.frame", "vector", any other is interpreted as "list
#' @param verbose         (logical) If TRUE, then print debug output
#' @param ...             Additional arguments passed to aggregation function
#' @return (mixed) Aggregation result
#' @family SANDRA
niceBy = function(
  ds,
  factors,
  aggregation,
  result_type = "list_to_data.frame",
  verbose = F,
  ...
) {
  # BUG HERE; what if ds has no rows?
  
  # *** Construct factors
  if( verbose ) { print( paste( Sys.time(), ", niceBy, construct factors", sep = "" ) ); }
  indices = list();
  for( f in factors ) {
    indices[[ length( indices ) + 1 ]] = ds[ ,f ];
  }
  
  # *** Create a list with all the subsets
  if( verbose ) { print( paste( Sys.time(), ", niceBy, create subsets", sep = "" ) ); }
  ds_list = by(
    ds,
    indices,
    function( subset ) {
      return( subset );
    }
  );
  
  # Remove NULL values in subsets
  ds_list = ds_list[ which( !unlist( lapply( ds_list, is.null ) ) ) ];
      
  # *** Apply aggregation to each element of ds_list 
  if( verbose ) { print( paste( Sys.time(), ", niceBy, apply aggregations", sep = "" ) ); }  
  result_raw = lapply(
    ds_list,
    function( subset ) {
      # Fill result with levels of each factor
      result = list();
      for( f in factors ) {
        result[[ f ]] = as.character(subset[ 1, f ]);
      }
      # Call aggregation with result and subset argument
      result = aggregation( 
        result,
        subset,
        ...
      );
      return( result );
    }
  );
  if( verbose ) { print( paste( Sys.time(), ", niceBy, aggregations done", sep = "" ) ); }
  
  if( result_type == "list_to_data.frame" ) {
    # list of lists to data.frame
    sorted_result_raw = list();
    if( length( result_raw[[1]][[1]] ) > 1 ) {
      # List of lists
      result_names = names( result_raw[[1]][[1]] );
      ncol = length( result_raw[[1]][[1]] );
      for( i in 1 : length( result_raw ) ) {
        for( j in 1 : length( result_raw[[i]] ) ) {
          sorted_result_raw[[length(sorted_result_raw) + 1]] = 
            result_raw[[i]][[j]][result_names];
        }
      }      
    } else {
      result_names = names( result_raw[[1]] );
      ncol = length( result_raw[[1]] );
      for( i in 1 : length( result_raw ) ) {
        sorted_result_raw[[length(sorted_result_raw) + 1]] = 
          result_raw[[i]][result_names];
      }      
    }    

    # Convert each element to character
    converted_result_raw = lapply( 
      sorted_result_raw, 
      function(x) { 
        return(
          lapply(
            x,
            function(y) {
              return(as.character(y[[1]]))          
            }
          )
        )
      }
    );
    
    result = data.frame( matrix(
      unlist( converted_result_raw ),
      ncol = ncol,
      byrow = T
    ) );  
    names( result ) = result_names;
    
    # Get names from first element of result_raw (if it is a list), 
    # or from first element of first element of list (if it is a list of lists)
#     if( length( result_raw[[1]][[1]] ) > 1 ) {
#       names( result ) = names( result_raw[[1]][[1]] );
#     } else {
#       names( result ) = names( result_raw[[1]] );
#     }
  } else if( result_type == "data.frame_to_data.frame" ) {
    # data.frame to data.frame
    result = result_raw[[1]];
    if(length(result_raw) > 1) {
      for(i in 2:length(result_raw)) {
        result = rbind(result, result_raw[[i]]);
      }
    }
  } else if( result_type == "vector" ) {
    # vector
    result = unlist( result_raw );
  } else {
    # list
    result = result_raw;
  }
  
  return( result );
}