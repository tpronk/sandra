# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

# ***************************************************
# *** Handy wrappers for R's native 'by' function ***
# ***************************************************

# Create sandra namespace if not exists
if( !exists( "sandra" ) ) { 
  sandra = list();
}

# nice_by calls the function 'aggregation' for each unique combination of values of the the columns
# 'factors' in the dataframe 'ds' 
#
# 'aggregation' is called with two arguments:
# 1. results - A list of key-value pairs for the current values of factors. To add results of your aggregation
#              function to the output, add these to results and return it.
# 2. subset  - A data frame with rows of 'ds' for the current unique combination of values in 'factors'
# Each call to aggregation should return an instance of results with the same set of names, if not, 
# then parallel_by reports an error.
#
# parallel_by returns a data.frame with as columns the elements in results and as rows the results returned
# by each call to aggregation. 
sandra$nice_by = function(
  ds,
  factors,
  aggregation,
  apply_function = lapply,
  debug = T,
  result_type = "data.frame",
  ...
) {
  
  # *** Construct factors
  if( debug ) { print( paste( Sys.time(), ", nice_by, construct factors", sep = "" ) ); }
  indices = list();
  for( f in factors ) {
    indices[[ length( indices ) + 1 ]] = ds[ ,f ];
  }
  
  # *** Create a list with all the subsets
  if( debug ) { print( paste( Sys.time(), ", nice_by, create subsets", sep = "" ) ); }
  ds_list = by(
    ds,
    indices,
    function( subset ) {
      return( subset );
    }
  );
      
  # *** Apply aggregation to each element of ds_list via apply_function (lapply or mclapply)
  if( debug ) { print( paste( Sys.time(), ", nice_by, apply aggregations", sep = "" ) ); }  
  result_raw = apply_function(
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
  if( debug ) { print( paste( Sys.time(), ", nice_by, aggregations done", sep = "" ) ); }
  
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