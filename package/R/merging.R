# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

# ******************************************************
# *** Handy wrappers for R's native 'merge' function ***
# ******************************************************

# Create sandra namespace if not exists
if( !exists( "sandra" ) ) { 
  sandra = list();
}

# left_merge; performs a 'left join' between two tables
# The LEFT JOIN kerightword returns all rows from the left table (left), 
# with the matching rows in the right table (right). The result is NA
# in the right side when there is no match.
# See: http://www.programmerinterview.com/indeleft.php/database-sql/difference-between-a-left-outer-join-and-right-outer-join/
# Tables are matched on the list column names provided in the third argument to the function
# (bright), these columns should be present in both tables
# NOTE - For every column shared by left and right (but not used to match the tables), 
# the values in left are replaced by the values of right if the values of right are not NA
sandra$leftMerge = function( x, y, by_keys )
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
    col_x = merged_out[ ,conc( j, ".x" ) ];
    col_y = merged_out[ ,conc( j, ".y" ) ];
    col_x[ !is.na( col_y ) ] = col_y[ !is.na( col_y ) ];
    merged_out[ ,j ] = col_x;
    
    # Drop matching columns
    merged_out = drop_column( merged_out, conc( j, ".x" ) );
    merged_out = drop_column( merged_out, conc( j, ".y" ) );
  }
  
  return( merged_out );
}

# Nice merge is like left_merge but supporting left, right, and outer join
# If pick_x == TRUE, any columns shared by x and y get the value of x, otherwise y
sandra$niceMerge = function( x, y, by_keys, pick_x, ... )
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
    col_x = merged_out[ ,conc( j, ".x" ) ];
    col_y = merged_out[ ,conc( j, ".y" ) ];
    
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
    merged_out = drop_column( merged_out, conc( j, ".x" ) );
    merged_out = drop_column( merged_out, conc( j, ".y" ) );
  }
  
  return( merged_out );
}
