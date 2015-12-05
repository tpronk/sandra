# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Wrapper for R's native lm function
#' 
#' Constructs an instance of EasyLM, which adds some arguments to the native lm function for specifying which variables are continuous or discrete 
#'
#' @param ds              (data.frame) Dataset to run regressions on
#' @param varsContinuous  (vector) Converts corresponding columns in dataframe to numeric, then Z-transforms
#' @param varsDiscrete    (vector) Converts corresponding columns in dataframe to factor
#' @return (function) wrapped lm function, use as lm without specifying data argument
EasyLM = function( 
  ds, 
  varsContinuous = c(), 
  varsDiscrete   = c()
) {
  # 
  # For continuous: 
  # For discrete:   
  
  # Extracts variables from a formula
  formula_to_variables = function( formula ) {
    # Symbols to replace
    symbols = c( "~", "+", "*", "(", ")" );
    # Replace each symbol to |
    for( symbol in symbols ) {
      formula = gsub( symbol, "~", formula, fixed = T )
    } 
    # Remove whitespace
     whites = c( " ", "\t" );
     for( white in whites ) {
       formula = gsub( white, "", formula, fixed = T )
     }
    # Split
    result = strsplit( formula, "~" )[[1]]
    result = result[ result != "" ];
    return( result );
  }
  
  # Converts columns in data.frame to numeric (for vars_continuous) and factor (vars_discrete)
  convert_columns = function(
    ds,
    vars_continuous = c(),
    vars_discrete   = c()
  ) {
    # Convert discrete to factor and continuous to numeric
    for( var_discrete in vars_discrete ) {
      ds[ ,var_discrete ] = as.factor( ds[ ,var_discrete ] );
    }
    for( var_continuous in vars_continuous ) {
      ds[ ,var_continuous ] = scale( as.numeric( ds[ ,var_continuous ] ) );
    }
    return( ds );
  }
  
  # Returns data.frame with any rows that have NAs for specified columns removed
  only_complete_rows = function( ds, cols ) {
    complete_rows = apply(
      ds,
      1,
      function( row ) {
        return( sum( is.na( row[ cols ] ) ) == 0 );
      }
    );
    return( ds[ complete_rows, ] );
  }
  
  # Return an lm function with some additional features
  presetLM = function( formula, histograms = F, ... ) {
    ds = convert_columns( ds, vars_continuous, vars_discrete );
    current_vars = formula_to_variables( formula );
    
    # Show histograms?
    if( histograms ) {
      for( current_var in current_vars ) {
        if( is.numeric( ds[, current_var ] ) ) {
          hist(
            main = current_var,
            ds[, current_var ] 
          );
          readline(prompt="Press [enter] to continue")
        }
      }
    }
    
    # Only rows with complete data
    ds = only_complete_rows( ds, current_vars );
    
    fit = lm(
      formula = formula,
      data = ds,
      ...
    );
    return( fit );
  }
  
  # Return exposed properties
  return( list(
    formula_to_variables = formula_to_variables,
    convert_columns      = convert_columns,
    only_complete_rows   = only_complete_rows,
    presetLM             = presetLM
  ) );
}