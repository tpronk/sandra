# *********************
# *** SANDRA File IO
# Provides short functions for reading and writing data, given a set of
# paths to folders with original and interim data and a standard way of 
# storing data frames (as tab-separated files).
# NB - SANDRA File IO treats all columns of type factor as character; convert these
# manually to factor if so needed

# Create sandra namespace if not exists
if( !exists( "sandra" ) ) { 
  sandra = list();
}

sandra$io = function( pathOriginal, pathInterim ) {
  # Constructs a SANDRA io object
  #
  # Args:
  #   pathOriginal: (character) Path to original unprocessed data files
  #   pathInterim:  (character) Path to data files as produced by your analyses
  #
  # Returns:
  #   (list) io object
  
  resolvePath = function( original ) {
    # Returns path to original or interim data
    #
    # Args:
    #   original: (logical) If TRUE, return path to original data; if not, return path to interim data
    #
    # Returns:
    #   (character) path
    if( original ) {
      return( pathOriginal );
    }
    return( pathInterim );
  }
  
  readSurveyTool = function( filename, original = F ) {
    # Read a TOP Survey Tool answer file
    #
    # Args:
    #   filename: (character) File to read
    #   original: (logical) If TRUE, return path to original data; if not, return path to interim data
    #
    # Returns:
    #   (character) path      
    pathData = resolvePath( original );
    
    # Read raw data
    stData = read.table(
      conc( dir, filename ),
      sep    = ";",
      header = FALSE,
      fill   = TRUE
    );
    
    # Convert to char
    stData = sandra$convert$factorToString( stData );
    stData = as.matrix( stData );
    
    # Get nice column names (combine labels from row 1 and 2 )
    stDataColumnNames = c( 
      as.vector( stData[ 1, 1:14 ] ),
      as.vector( stData[ 2, 15:length( stData[ 1, ] ) ] )
    )
    
    # Remove top 2 rows
    stData = stData[ 3 : length( stData[ ,1 ] ), ];
    
    # If only one row resulting -> vector, convert back to matrix again
    if( is.vector( stData ) ) {
      stData = matrix( stData, byrow = FALSE, ncol = length( stData ) );
    }  
    
    # Convert to data frame
    stData = data.frame( stData );
    names( stData ) = stDataColumnNames;
    stData = to_char( stData );
    
    # All done, return
    return( stData );
  }
  
  
  # Read a tab separated file (with varnames at the top) 
  # as a data frame filled with strings
  # fileEncoding="UTF-8-BOM",
  readData = function( 
    filename, 
    original= FALSE, 
    sep = "\t", 
    quote = "", 
    comment.char = "",
    stringAsFactors = F,
    ... 
  ) {
    # Read a TOP Survey Tool answer file
    #
    # Args:
    #   filename: (character) File to read
    #   original: (logical) If TRUE, return path to original data; if not, return path to interim data
    #
    # Returns:
    #   (character) path          
    if( source_data ) {
      dir = dir_source;
    } else {
      dir = dir_data;
    }
    
    data = read.table(
      conc( dir, filename ),
      sep   = sep,
      quote = quote,
      comment.char = "",
      header       = header,
      fill   = TRUE,
      stringsAsFactors = F,
      ...
    );
    
    data = sandra$convert$factorToString( data );
    
    return( data );
  }
  
  
  readVector = function( filename ) {
    # Read a vector as a newline separated file
    #
    # Args:
    #   filename: (character) File to read
    #
    # Returns:
    #   (data.frame)
    x = readData(
      filename,
      header = F
    );
    return( as.vector( t( x ) ) );
  }  
  
  # Return properties that we want to have exposed
  return( list(
    resolvePath    = resolvePath,
    readSurveyTool = readSurveyTool,
    readData       = readData,
    readVector     = readVector
  ) );
}