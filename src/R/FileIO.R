# Provides short functions for reading and writing data, given a set of
# paths to folders with original and interim data and a standard way of 
# storing data frames (as tab-separated files).

# ***************************
# *** FileIO constructors ***
# ***************************

#' Constructs a SANDRA FileIO instance, assuming standard analysis, original, and interim directories
#'
#' @export
#' @param pathAnalysis  (character) Path to your analysis
#' @param verbose       (logical) If TRUE, check whether each expected subdirectory exists and print that io is loaded
#' @return (list) FileIO instance
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Setup analysis for directory D:/analysis
#' io = FrameworkFileIO( "D:/analysis" );
FrameworkFileIO = function( pathAnalysis, verbose = TRUE ) {
  error = FALSE;
  subdirectories = c(
    paste( pathAnalysis, "scripts",  sep = "/" ),
    paste( pathAnalysis, "original", sep = "/" ),
    paste( pathAnalysis, "interim",  sep = "/" ),
    paste( pathAnalysis, "checksums",  sep = "/" )
  );    
  if( verbose ) {
    for( subdirectory in subdirectories ) {
      if( !file.exists( subdirectory ) ) {
        error = TRUE;
        print( paste(
          "sandra::FrameworkFileIO. Could not find directory '",
          subdirectory,
          "'"
        ) );
      }
    }
  }
  fileIO = FileIO(
    pathAnalysis,
    paste( pathAnalysis, "scripts",  sep = "/" ),
    paste( pathAnalysis, "original", sep = "/" ),
    paste( pathAnalysis, "interim",  sep = "/" ),
    paste( pathAnalysis, "checksums",  sep = "/" )
  );
  if( !error && verbose ) {
    print( 
      "sandra::FrameworkFileIO. Succesfully constructed FileIO",
    );
  }
  return( fileIO );
}

#' Constructs a SANDRA FileIO instance
#'
#' @export
#' @param pathScripts  (character) Path to your analysis scripts
#' @param pathOriginal (character) Path to your original unprocessed data files
#' @param pathInterim  (character) Path to data files as produced by your analyses
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
FileIO = function(pathAnalysis, pathScripts, pathOriginal, pathInterim, pathChecksums) {
  pathAnalysis = pathAnalysis;
  pathScripts  = pathScripts;
  pathOriginal = pathOriginal;
  pathInterim  = pathInterim;
  pathChecksums = pathChecksums;
  
  this = list(
    # Values
    pathAnalysis   = pathAnalysis,
    pathScripts    = pathScripts,
    pathOriginal   = pathOriginal,
    pathInterim    = pathInterim,
    pathChecksums  = pathChecksums,
  
    # Main Functions
    updateChecksum = function( ... ) { updateChecksum( this, ... ); },
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

#' Checks and updates a checksum
#
#' @export
#' @param this      (sandra::FileIO) FileIO instance
#' @param operation (character) String to denote operation to be added in log (read/write)
#' @param curFile   (character) File to check
#' @return NULL
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
updateChecksum = function(this, operation, curFile) {
  # Read or construct checksums
  if (this$existsData("../checksums/checksums.csv")) {
    allChecksums = this$readData("../checksums/checksums.csv", checksum = FALSE);
  } else {
    allChecksums = data.frame.new(c("date", "version", "file", "operation", "checksum"));
  }
  
  # Calculate current checksum
  curChecksum = as.character(md5sum(curFile));
    
  # Decide if we should add a new checkSum
  checksums = allChecksums[allChecksums[,"file"] == curFile,];
  addChecksum = nrow(checksums) == 0;
  if (!addChecksum) {
    oldChecksum = checksums[order(checksums[,"date"], decreasing = TRUE)[1], "checksum"];
    addChecksum = oldChecksum != curChecksum; 
  }
  if (addChecksum) {
    allChecksums[nrow(allChecksums) + 1,] = c(
      format(now(),'%Y-%m-%d %H:%M:%S'),
      packageVersion("sandra"),
      curFile,
      operation,
      curChecksum
    );
    this$writeData(
      "../checksums/checksums.csv",
      allChecksums,
      checksum = FALSE
    );
  }
}

#' Returns path to original or interim data
#
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param original (logical) If TRUE, return path to original data; if not, return path to interim data
#' @return (character) Path
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
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
#'
#' @export
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to read
#' @param original (logical) If TRUE, use path to original data; if not, use path to interim data
#' @return (data.frame) Dataset
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$readSurveyTool( "answers.csv" );
readSurveyTool = function( this, filename, original = FALSE ) {
  pathData = this$resolvePath( original );

  # Read raw data
  stData = read.table(
    path( pathData, filename ),
    sep    = ";",
    header = FALSE,
    fill   = TRUE
  );
  this$updateChecksum("read", path(pathData, filename));  
  
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
#'
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to read
#' @param original (logical) If TRUE, use path to original data; if not, use path to interim data
#' @inheritParams  utils::write.table
#' @param ...      Remaining arguments passed to utils::read.table
#' @return (data.frame) Dataset
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$readData( "data.csv" );
readData = function( 
  this,
  filename, 
  checksum = TRUE,
  original = FALSE, 
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
  if (checksum) {
    this$updateChecksum("read", path(pathData, filename));
  }
  return( data );
}

#' Write a data frame to a tab separated file (with varnames at the top row)
#'
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to write to
#' @param output   (data.frame) Data to write to file
#' @inheritParams utils::write.table
#' @param ...      Remaining arguments passed to utils::write.table
#' @return NULL
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$writeData( "data.csv", ds );
writeData = function( 
  this,
  filename, 
  output, 
  checksum = TRUE,
  append = FALSE, 
  sep = "\t",
  quote = FALSE, 
  row.names = FALSE, 
  col.names = TRUE, 
  ...
)  {
  pathData = this$resolvePath( FALSE );
  write.table(
    output,
    path( pathData, filename ),
    append    = append,    
    sep       = sep,    
    quote     = quote,
    row.names = row.names,
    col.names = !append && col.names, # No column names if appending
    ...
  )
  if (checksum) {
    this$updateChecksum("write", path(pathData, filename));
  }
}

#' Check if a file exists in original or interim directory
#'
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to check existance of
#' @param original (logical) If TRUE, return path to original data; if not, return path to interim data
#' @return TRUE if file exists; if not, FALSE
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$existsData( "data.csv" );
existsData = function ( this, filename, original = FALSE )  {
  pathData = this$resolvePath( original );
  return( file.exists( path( pathData, filename ) ) );
}

#' Create file if exists, else append data to existing file
#'
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to write to
#' @param output   (data.frame) Data to write to file
#' @param ...      Remaining arguments passed to sandra::writeData
#' @return NULL
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$appendData( "data.csv", ds );
appendData = function( this, filename, output, ... ) {
  append = this$existsData( filename );
  this$writeData(
    filename,
    output,
    append,
    ...
  );
}

#' Read a vector from a newline separated file
#'
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to read from
#' @param original (logical) If TRUE, use path to original data; if not, use path to interim data
#' @return (vector) Vector
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
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
#'
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param filename (character) File to write to
#' @param output   (vector) Data to write to file
#' @return NULL
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$writeVector( "data.csv", v );
writeVector = function( this, filename, output ) {
  x = data.frame.new( c( "colname" ) );
  x[ 1 : length( output ), ] = output;
  
  this$writeData(
    filename,
    x,    
    col.names = F
  );
}  

#' Run a script from a file in scripts directory
#'
#' @export
#' @param this     (sandra::FileIO) FileIO instance
#' @param script   (character) File with script to run
#' @return NULL
#' @family sandra::FileIO 
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Create FileIO instance with folders in root of D:
#' io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
#' io$runScript( "myScript.R" );
runScript = function( this, script ) {
  source( path( this$pathScripts, script ) );
}  

# ***************************
# *** Static methods      ***
# ***************************

#' Returns terms joined by "/" for constructing path names
#'
#' @export
#' @param ...      (character) Terms to join
#' @return (character) joined terms
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Returns "a/b/c"
#' path( "a", "b", "c" );
path = function( ... ) {
  return( paste( ..., sep="/" ) );
}

#' Postfix filename
#' 
#' Returns filename, with postfixes appended, each separated by dots, followed by the filename extension
#'
#' @export
#' @param filename (character) Filename to append postfixes to
#' @param ...      (character) Postfixes to append to filename
#' @return (character) postfixed filename
#' @family sandra::file input & output
#' @family SANDRA
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
#'
#' @export
#' @param filename  (character) Filename to cut extension from
#' @return (character) Filename without extension
#' @family sandra::file input & output
#' @family SANDRA
#' @examples
#' # Returns "data"
#' cutExtension( "data.csv" );
cutExtension = function( filename )  {
  inputSet  = strsplit( filename, "\\." )[[1]];
  filePrefix = paste( inputSet[ 1 : ( length( inputSet ) - 1 ) ], collapse = "." );
  return( filePrefix );
}

#' Returns the extension of the given filename; everything after the last dot
#'
#' @export
#' @param filename  (character) Filename to get extension from
#' @return (character) Filename extension
#' @family sandra::file input & output
#' @family SANDRA
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

