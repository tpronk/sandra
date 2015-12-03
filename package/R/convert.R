# *********************
# *** SANDRA Convert
# Static functions for converting data types

# Create sandra namespace if not exists
if( !exists( "sandra" ) ) { 
  sandra = list();
}

sandra$convert = list(
  factorToString = function( convertMe ) {
    # Converts each column of a data frame that is of type factor to type character
    # Based on: http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters/2853231#2853231
    #
    # Args:
    #   convertMe: (data.frame) to convert
    #
    # Returns:
    #   (data.frame) converted data frame
    i = sapply( convertMe, is.factor );
    convertMe[i] = lapply( convertMe[i], as.character );
    return( convertMe );
  }  
);