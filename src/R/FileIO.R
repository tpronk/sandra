# Provides short functions for reading and writing data, given a set of
# paths to folders with original and interim data and a standard way of 
# storing data frames (as tab-separated files).

# ***************************
# *** FileIO constructors ***
# ***************************

#' Constructs a SANDRA FileIO instance, assuming standard analysis, original, and interim directories
#'
#' @param pathAnalysis  (character) Path to your analysis
#' @return (list) FileIO instance
#' @section FileIO
#' @examples
#' # Setup analysis for directory D:/analysis
#' io = FrameworkFileIO( "D:/analysis" );
FrameworkFileIO = function( pathAnalysis ) {
  return( FileIO(
    paste( pathAnalysis, "scripts",  sep = "/" ),
    paste( pathAnalysis, "original", sep = "/" ),
    paste( pathAnalysis, "interim",  sep = "/" )
  ) );
}

#' Constructs a SANDRA FileIO instance
#'
#' @param pathScripts  (character) Path to your analysis scripts
#' @param pathOriginal (character) Path to your original unprocessed data files
#' @param pathInterim  (character) Path to data files as produced by your analyses
#' @section FileIO
#' @return (list) FileIO instance
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
FileIO = function( pathScripts, pathOriginal, pathInterim ) {
  pathScripts  = pathScripts;
  pathOriginal = pathOriginal;
  pathInterim  = pathInterim;
  
  this = list(
    # Values
    pathScripts    = pathScripts,
    pathOriginal   = pathOriginal,
    pathInterim    = pathInterim,
  
    # Main Functions
    resolvePath    = function( ... ) { resolvePath( this, ... ); },
    readSurveyTool = function( ... ) { readSurveyTool( this, ... ); },
    readData       = function( ... ) { readData( this, ... ); },
    writeData      = function( ... ) { writeData( this, ... ); },
    existsData     = function( ... ) { existsData( this, ... ); },
    appendData     = function( ... ) { appendData( this, ... ); },
    readVector     = function( ... ) { readVector( this, ... ); },
    writeVector    = function( ... ) { writeVector( this, ... ); },
    runScript      = function( ... ) { runScript( this, ... );  }
  );
  return( this );
}

# ***************************
# *** FileIO methods      ***
# ***************************

#' Returns path to original or interim data
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param original (logical) If TRUE, return path to original data; if not, return path to interim data
#' @return (character) Path
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' # Returns "D:/original"
#' io$resolvePath( T );
resolvePath = function( this, original ) {
  if( original ) {
    return( this$pathOriginal );
  }
  return( this$pathInterim );
}

#' Read a data.frame from a TOP Survey Tool answer file
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to read
#' @param original (logical) If TRUE, use path to original data; if not, use path to interim data
#' @return (data.frame) Dataset
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$readSurveyTool( "answers.csv" );
readSurveyTool = function( this, filename, original = F ) {
  pathData = this$resolvePath( original );
  
  # Read raw data
  stData = read.table(
    path( pathData, filename ),
    sep    = ";",
    header = FALSE,
    fill   = TRUE
  );
  
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
  # stData = to_char( stData );
  
  # All done, return
  return( stData );
}

#' Read a tab separated file (with varnames at the top row)
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to read
#' @param original (logical) If TRUE, use path to original data; if not, use path to interim data
#' @param remainder: Arguments passed to read.table with default values
#' @return (data.frame) Dataset
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$readData( "data.csv" );
readData = function( 
  this,
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
  pathData = this$resolvePath( original );
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

#' Write a data frame to a tab separated file (with varnames at the top row)
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to write to
#' @param output   (data.frame) Data to write to file
#' @param remainder: Arguments passed to read.table with default values
#' @return NULL
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$writeData( "data.csv", ds );
writeData = function( 
  this,
  filename, 
  output, 
  append = F, 
  sep = "\t",
  quote = F, 
  row.names = F, 
  col.names = T, 
  ...
)  {
  pathData = this$resolvePath( FALSE );
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

#' Check if a file exists in original or interim directory
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to check existance of
#' @param original (logical) If TRUE, return path to original data; if not, return path to interim data
#' @return TRUE if file exists; if not, FALSE
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$existsData( "data.csv" );
existsData = function ( this, filename, original = FALSE )  {
  pathData = this$resolvePath( original );
  return( file.exists( path( pathData, filename ) ) );
}

#' Create file if exists, else append data to existing file
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to write to
#' @param output   (data.frame) Data to write to file
#' @return NULL
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$appendData( "data.csv", ds );
appendData = function( this, filename, output, ... ) {
  append = this$existsData( filename );
  this$writeData(
    filename,
    output,
    append
  );
}

#' Read a vector from a newline separated file
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to read from
#' @param original (logical) If TRUE, use path to original data; if not, use path to interim data
#' @return (vector) Vector
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$readVector( "data.csv" );
readVector = function( this, filename, original ) {
  x = this$readData(
    filename,
    original = original,
    header = F
  );
  return( as.vector( t( x ) ) );
}  

#' Write a vector to a newline separated file
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to write to
#' @param output   (vector) Data to write to file
#' @return NULL
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$writeVector( "data.csv", v );
writeVector = function( this, filename, output ) {
  x = empty_dataframe( c( "colname" ) );
  x[ 1 : length( output ), ] = output;
  
  this$writeData(
    filename,
    x,    
    col.names = F,
    sort_columns = F
  );
}  

#' Run a script from a file in scripts directory
#
#' @param this     (sandra::FileIO) FileIO instance
#' @param script   (character) File with script to run
#' @return NULL
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$runScript( "myScript.R" );
runScript = function( this, script ) {
  source( path( pathScripts, script ) );
}  

# ***************************
# *** Static methods      ***
# ***************************

#' Returns terms joined by "/" for constructing path names
#
#' @param ...      (character) Terms to join
#' @return (character) joined terms
#' @examples
#' # Returns "a/b/c"
#' path( "a", "b", "c" );
path = function( ... ) {
  return( paste( ..., sep="/" ) );
}

#' Returns filename, with postfixes appended, each separated by dots, followed by the filename extension
#
#' @param ...      (character) Filename to append postfixes to
#' @param ...      (character) Postfixes to append to filename
#' @return (character) postfixed filename
#' @examples
#' # Returns "data.a.b.c.csv"
#' addPostfix( "data.csv", "a", "b", "c" );
addPostfix = function( filename, ... ) {
  result = cutExtension( filename );
  return( paste(
    result,
    ...,
    getExtension( filename ),
    sep = "."
  ) );
}  

#' Returns the filename without extension; everything up to the last dot
#
#' @param filename  (character) Filename to cut extension from
#' @return (character) Filename without extension
#' @examples
#' # Returns "data"
#' cutExtension( "data.csv" );
cutExtension = function( filename )  {
  inputSet  = strsplit( filename, "\\." )[[1]];
  filePrefix = paste( inputSet[ 1 : ( length( inputSet ) - 1 ) ], collapse = "." );
  return( filePrefix );
}

#' Returns the extension of the given filename; everything after the last dot
#
#' @param filename  (character) Filename to get extension from
#' @return (character) Filename extension
#' @examples
#' # Returns "csv"
#' getExtension( "data.csv" );
getExtension = function( filename ) {
  inputSet  = strsplit( filename, "\\." )[[1]];
  # If only one element; no extension, so return empty string
  if( length( inputSet ) == 1 )
    return( "" );
  return( inputSet[ length( inputSet ) ] );
}

