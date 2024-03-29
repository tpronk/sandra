# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Wrapper for R's native merge function for jeft join
#'
#' Performs a 'left join' between two data frames, with some function to append or overwrite duplicate values
#' Returns all rows from the left table (left), with the matching rows in the right table (right). The result is NA in the right side when there is no match.
#' For every column shared by left and right (but not used to match the tables), the values in left are replaced by the values of right if the values of right are not NA.
#' 
#' @export
#' @param x        (data.frame) Left data frame
#' @param y        (data.frame) Right data frame
#' @param by_keys  (vector) Keys to join both data frames on
#' @return (data.frame) Left joined data frames
#' @family sandra::merging
#' @family SANDRA
leftMerge = function( x, y, by_keys )
{
  # Left join x and y on exp and set_id
  merged_out = merge( 
    x = x, 
    y = y, 
    by = by_keys,
    all.x = TRUE 
  );
  
  # Get all matching columns (except by_keys)
  names_x     = names( x );
  names_y     = names( y );
  names_match = names_y[ !is.na( match( names_y, names_x ) ) ];
  for( j in by_keys )
  {
    names_match = names_match[ names_match != j ];
  }
  
  # For every column shared by x and y, the values in x are replaced by the values of y if the values of y are not NA
  for( j in names_match )
  {
    col_x = merged_out[ ,paste( j, "x", sep = "." ) ];
    col_y = merged_out[ ,paste( j, "y", sep = "." ) ];
    col_x[ !is.na( col_y ) ] = col_y[ !is.na( col_y ) ];
    merged_out[ ,j ] = col_x;
    
    # Drop matching columns
    merged_out = data.frame.dropVar( merged_out, paste( j, "x", sep = "." ) );
    merged_out = data.frame.dropVar( merged_out, paste( j, "y", sep = "." ) );
  }
  
  return( merged_out );
}

#' Like leftMerge but supporting left, right, and outer join
#' 
#' @export
#' @param x        (data.frame) Left data frame
#' @param y        (data.frame) Right data frame
#' @param by_keys  (vector) Keys to join both data frames on
#' @param by_keys  (logical) If TRUE, then for duplicates pick left table; if not, then pick right
#' @param ...      Arguments passed on to R's native merge function
#' @return (data.frame) Joined data frames
#' @family sandra::merging
#' @family SANDRA
niceMerge = function( x, y, by_keys, pick_x, ... )
{
  # Left join x and y on exp and set_id
  merged_out = merge( 
    x = x, 
    y = y, 
    by = by_keys,
    ...
  );
  
  # Get all matching columns (except by_keys)
  names_x     = names( x );
  names_y     = names( y );
  names_match = names_y[ !is.na( match( names_y, names_x ) ) ];
  for( j in by_keys )
  {
    names_match = names_match[ names_match != j ];
  }
  
  # For every column shared by x and y, the values in one  are replaced by the other, depending on pick_x
  for( j in names_match )
  {
    col_x = merged_out[ ,paste( j, "x", sep = "." ) ];
    col_y = merged_out[ ,paste( j, "y", sep = "." ) ];
    
    if( pick_x ) {
      col_from = col_x;
      col_to   = col_y;
    } else {
      col_from = col_y;
      col_to   = col_x;
    }
    
    col_to[ !is.na( col_from ) ] = col_from[ !is.na( col_from ) ];
    merged_out[ ,j ] = col_to;
    
    # Drop matching columns
    merged_out = data.frame.dropVar( merged_out, paste( j, "x", sep = "." ) );
    merged_out = data.frame.dropVar( merged_out, paste( j, "y", sep = "." ) );
  }
  
  return( merged_out );
}
