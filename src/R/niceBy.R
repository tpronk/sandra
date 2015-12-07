# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Wrapper for R's native by function
#' 
#' Calls the function 'aggregation' for each unique combination of values of the the columns factors in the dataframe ds.
#' The aggregation function is called with two arguments: (1) (list) result list to add aggregation output to, and (2) (data.frame) subset of data selected by this combination of values for factors
#'
#' @param ds              (data.frame) Dataset to aggregate
#' @param factors         (vector) Columns in dataset that serve as factors
#' @param aggregation     (function) Aggregation 
#' @param result_type     (character) Three possible values: "data.frame", "vector", any other is interpreted as "list
#' @param verbose         (logical) If TRUE, then print debug output
#' @param ...             Additional arguments passed to aggregation function
#' @return (mixed) Aggregation result
niceBy = function(
  ds,
  factors,
  aggregation,
  result_type = "data.frame",
  verbose = F,
  ...
) {
  
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
        result[[ f ]] = subset[ 1, f ];
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
  
  if( result_type == "data.frame" ) {
    # data.frame
    ncol = length( result_raw[[1]] );
    result = data.frame( matrix(
      unlist( result_raw ),
      ncol = ncol,
      byrow = T
    ) );  
    names( result ) = names( result_raw[[1]] );
  } else if( result_type == "vector" ) {
    # vector
    result = unlist( result_raw );
  } else {
    # list
    result = result_raw;
  }
  
  return( result );
}