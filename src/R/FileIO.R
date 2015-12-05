# *********************
# *** SANDRA FileIO

# Provides short functions for reading and writing data, given a set of
# paths to folders with original and interim data and a standard way of 
# storing data frames (as tab-separated files).

# Create sandra namespace if not exists
if( !exists( "sandra" ) ) { 
  sandra = list();
}

sandra$FrameworkFileIO = function( pathAnalysis ) {
  return( sandra$FileIO(
    paste( pathAnalysis, "scripts",  sep = "/" ),
    paste( pathAnalysis, "original", sep = "/" ),
    paste( pathAnalysis, "interim",  sep = "/" )
  ) );
}

sandra$FileIO = function( pathScripts, pathOriginal, pathInterim ) {
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
  
  path = function( ... ) {
    # Return terms joined by "/" for constructing path names
    #
    # Args:
    #   ...: (character) Terms to join
    #
    # Returns:
    #   (character) path    
    return( paste( ..., sep="/" ) );
  }
  
  cutExtension = function( filename )  {
    # Returns the filename without extension; everything up to the last dot
    #
    # Args:
    #   filename: (character) Filename to cut extension from
    #
    # Returns:
    #   (character) Filename without extension

    inputSet  = strsplit( filename, "\\." )[[1]];
    filePrefix = paste( inputSet[ 1 : ( length( inputSet ) - 1 ) ], collapse = "." );
    return( filePrefix );
  }
  
  # *** getExtension; 
  getExtension = function( filename ) {
    # Returns the extension of the given filename; everything after the last dot
    #
    # Args:
    #   filename: (character) Filename to get extension from
    #
    # Returns:
    #   (character) Filename extension

    inputSet  = strsplit( filename, "\\." )[[1]];
    # If only one element; no extension, so return empty string
    if( length( inputSet ) == 1 )
      return( "" );
    return( inputSet[ length( inputSet ) ] );
  }
  
  addPostfix = function( filename, ... ) {
    # Returns filename, with .postfix appended (before the extension)
    # For example: "myfile.txt" will be converted to "myfile.postfix.txt".
    #
    # Args:
    #   filename: (character) Filename to add postfix to 
    #   postfix:  (character) Postfix to add to filename
    #
    # Returns:
    #   (character) Filename with postfix
    
    result = cutExtension( filename );
    return( paste(
      result,
      ...,
      getExtension( filename ),
      sep = "."
    ) );
  }  
  
  readSurveyTool = function( filename, original = F ) {
    # Read a TOP Survey Tool answer file
    #
    # Args:
    #   filename: (character) File to read
    #   original: (logical) If TRUE, use path to original data; if not, use path to interim data
    #
    # Returns:
    #   (character) path      
    pathData = resolvePath( original );
    
    # Read raw data
    stData = read.table(
      path( pathData, filename ),
      sep    = ";",
      header = FALSE,
      fill   = TRUE
    );
    
    # stData = sandra$convert$factorToString( stData );
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
  
  readData = function( 
    filename, 
    original= FALSE, 
    sep = "\t", 
    quote = "", 
    comment.char = "",
    header = TRUE,
    fill = TRUE,
    stringsAsFactors = FALSE,
    ... 
  ) {
    # Read a tab separated file (with varnames at the top) 
    #
    # Args:
    #   filename: (character) File to read
    #   original: (logical) If TRUE, use path to original data; if not, use path to interim data
    #   remainder: Arguments passed to read.table with default values
    # Returns:
    #   (data.frame) Data read from file
    
    pathData = resolvePath( original );
    data = read.table(
      path( pathData, filename ),
      sep   = sep,
      quote = quote,
      comment.char = comment.char,
      header = header,
      fill   = fill,
      stringsAsFactors = stringsAsFactors,
      ...
    );
    return( data );
  }

  # Write a data frame to a tab separated file (with varnames at the top) 
  # Note: column names are sorted alphabetically
  writeData = function( 
    filename, 
    output, 
    append = F, 
    sep = "\t",
    quote = F, 
    row.names = F, 
    col.names = T, 
    ...
  )  {
    # Read a TOP Survey Tool answer file
    #
    # Args:
    #   filename: (character) File to write to
    #   output:   (data.frame) Data frame to store
    #   remainder: Arguments passed to write.table with default values
    # Returns:
    #   NULL
    
    # Store output in file
    pathData = resolvePath( FALSE );
    write.table(
      output,
      path( pathData, filename ),
      quote     = quote,
      sep       = sep,
      row.names = row.names,
      append    = append,
      col.names = !append && col.names, # No column names if appending
      ...
    )
  }
  
  existsData = function ( filename, original = FALSE )  {
    # Check if a data file exists (in the data directory)
    #
    # Args:
    #   filename: (character) File to write to
    #   original: (logical) If TRUE, use path to original data; if not, use path to interim data
    #
    # Returns:
    #   (logical) TRUE if data exists; if not, FALSE
    
    pathData = resolvePath( original );
    return( file.exists( path( pathData, filename ) ) );
  }
  
  appendData = function( filename, output, ... ) {
    # Create file if exists, else append data to existing file
    #
    # Args:
    #   filename: (character) File to write to
    #   original: (logical) If TRUE, use path to original data; if not, use path to interim data
    #
    # Returns:
    #   (logical) TRUE if data exists; if not, FALSE
    
    append = existsData( filename );
    writeData(
      filename,
      output,
      append
    );
  }
  
  readVector = function( filename, original = FALSE ) {
    # Read a vector as a newline separated file
    #
    # Args:
    #   filename: (character) File to read
    #
    # Returns:
    #   (data.frame)
    x = readData(
      filename,
      original = original,
      header = F
    );
    return( as.vector( t( x ) ) );
  }  
  
  writeVector = function( filename, output ) {
    # Write a vector to a newline separated file
    #
    # Args:
    #   filename: (character) File to write to
    #   output:   (vector)    Vector to write
    #
    # Returns:
    #   NULL
    
    x = empty_dataframe( c( "colname" ) );
    x[ 1 : length( output ), ] = output;
    
    writeData(
      filename,
      x,    
      col.names = F,
      sort_columns = F
    );
  }  
  
  runScript = function( script ) {
    source( path( pathScripts, script ) );
  }  
  
  # Return exposed properties
  return( list(
    # Values
    pathOriginal   = pathOriginal,
    pathInterim    = pathInterim,
    
    # Helper Functions
    resolvePath    = resolvePath,
    path           = path,
    cutExtension   = cutExtension,
    getExtension   = getExtension,
    addPostfix     = addPostfix,
    
    # Main Functions
    readSurveyTool = readSurveyTool,
    readData       = readData,
    writeData      = writeData,
    existsData     = existsData,
    appendData     = appendData,
    readVector     = readVector,
    writeVector    = writeVector,
    runScript      = runScript
  ) );
}