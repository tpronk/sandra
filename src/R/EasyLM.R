# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

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


# Converts columns in data.frame to numeric (for varsContinuous) and factor (varsDiscrete)
convert_columns = function(
  ds,
  varsContinuous = c(),
  varsDiscrete   = c(),
  zTranform      = F
) {
  # Convert discrete to factor and continuous to numeric
  for( varDiscrete in varsDiscrete ) {
    ds[ ,varDiscrete ] = as.factor( ds[ ,varDiscrete ] );
    # More than 2 levels? Use deviation coding
    nLevels = length( unique( ds[ ,varDiscrete ] ) );
    if( nLevels > 2 ) {
      # contrasts( ds[ ,varDiscrete ] ) = contr.sum( nLevels );
    }
  }
  for( varContinuous in varsContinuous ) {
    if( zTranform ) {
      ds[ ,varContinuous ] = scale( as.numeric( ds[ ,varContinuous ] ) );
    } else {
      ds[ ,varContinuous ] = as.numeric( ds[ ,varContinuous ] );
    }
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
select_rows_by_formula = function( formula, ds, histograms = F, ... ) {
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
  return( ds );
}

# Fits formula on rows that have full data
fit_on_full_data = function( formula, ds, callback = lm ) {
  ds = select_rows_by_formula( formula, ds );
  return( callback( formula = formula, data = ds ) );
}


#' Wrapper for R's native lm function
#' 
#' Constructs an instance of EasyLM, which adds some arguments to the native lm function for specifying which variables are continuous or discrete 
#' *** NOTE *** This function is still in early development
#'
#' @export
#' @param ds              (data.frame) Dataset to run regressions on
#' @param varsContinuous  (vector) Converts corresponding columns in dataframe to numeric, then Z-transforms
#' @param varsDiscrete    (vector) Converts corresponding columns in dataframe to factor
#' @family SANDRA
#' @return (function) wrapped lm function, use as lm without specifying data argument
EasyFit = function( 
  ds, 
  varsC = c(), 
  varsD = c()
) {
  ds  = convert_columns( ds, varsC, varsD, zTranform = F );
  dsZ = convert_columns( ds, varsC, varsD, zTranform = T );
  
  this = list(
    ds  = ds,
    dsZ = dsZ,
    
    varsC = varsC,
    varsD = varsD,
    fit_on_full_data = function( formula, zScores = T, callback = lm ) {
      if( zScores ) {
        dsCur = dsZ;
      } else {
        dsCur = ds;
      }
      fit_on_full_data( formula, dsCur, callback );
    }
  );
  return( this );
}