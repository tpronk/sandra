# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Wrapper for R's native reshape( direction = "wide" ) function
#'
#' This wrapper can handle multiple timevars (reshaped to columns dv.timevar1.timevar2)
#' 
#' @param data     (data.frame) Data to reshape
#' @param idvars   (vector) Names of one or more variables in long format that identify multiple records from the same group/individual
#' @param timevars (vector) Names of one or more variables in long format that differentiates multiple records from the same group or individual
#' @return (data.frame) Reshaped data
makeWide = function( data, idvars, timevars ) {
  # Reshape for each timevar
  for( i in length( timevars ) : 1 )  {
    # Current and remaining timevars
    timevar_cur = timevars[  i ];
    timevars    = timevars[ -i ]; 
    
    # Check whether each combination of idvars and timevar produces one row
    rowCounts = niceBy( 
      data,
      c( idvars, timevar_cur ),
      function( result, ds_sub ) {
        result[[ "n" ]] = nrow( ds_sub );
        return( result );
      }
    )
    rowCounts[ ,"n" ] = as.numeric( as.character( rowCounts[ ,"n" ] ) );
    rowCounts = rowCounts[ rowCounts[ ,"n" ] > 1, ];
    
#     if( nrow( rowCounts ) > 0 ) {
#       warning( paste(
#         "sandra::makeWide. Duplicate rows found for columns ",
#         paste( c( idvars, timevar_cur ), sep = ", ", collapse = ", " ),
#         " with values: ",
#         paste(
#           apply( rowCounts, 1, function( row ) { 
#             return( paste(
#               "[",
#               paste( 
#                 as.character( unlist( row[ c( idvars, timevar_cur ) ] ) ),  
#                 collapse = ",", 
#                 sep = "," 
#               ),
#               "]",
#               sep = ""
#             ) )
#           } ),
#           collapse = ","
#         )
#       ) );
#     }
#     
    # Reshape data
    data = reshape(
      data,
      direction = "wide",
      idvar   = c( idvars, timevars ),
      timevar = c( timevar_cur )
    );
  }
  return( data );
}