# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Affix column names in a data frame
#'
#' @export
#' @param ds       (data.frame) Data frame
#' @param prefix   (character) Prefix to add to column names (name becomes prefix.name)
#' @param postfix  (character) Postfix to add to column names (name becomes name.postfix)
#' @param not      (vector) Column names that should NOT be changed (get no prefix or postfix)
#' @return (data.frame) Data with affixed column names
#' @family sandra::dataFrames
#' @family SANDRA
#' @examples
#' # Create a new data frame
#' ds = data.frame.new( vars = c( "id", "x", "y", "z" ) );
#' # Prefix and postfix all names except "id"
#' data.frame.affixNames( ds, prefix = "pre", postfix = "post", not = "id" );
data.frame.affixNames = function( ds, prefix = NULL, postfix = NULL, not = c() ) {
  indexNot = names( ds ) %in% not;
  newNames = names( ds )[ !indexNot ];
  if( !is.null( prefix ) ) {
    newNames = paste(
      prefix,
      newNames,
      sep = "."
    );
  }
  if( !is.null( postfix ) ) {
    newNames = paste(
      newNames,
      postfix,      
      sep = "."
    );
  }  
  names( ds )[ !indexNot ] = newNames;
  return( ds );
}

#' Drops a variable from a data frame
#'
#' @export
#' @param ds       (data.frame) Data frame
#' @param dropMe   (character) Variable to drop
#' @return (data.frame) ds with variable dropped
#' @family sandra::dataFrames
#' @family SANDRA
#' @examples
#' # Create a new data frame
#' ds = data.frame.new( vars = c( "id", "x", "y", "z" ) );
#' # Drop "id" 
#' data.frame.dropVar( ds, "id" );
data.frame.dropVar = function( ds, dropMe ) {
  matches = names( ds ) == dropMe;
  return( ds[ ,!matches, drop = FALSE ] );
}  

#' Renames a variable of a data frame
#'
#' @export
#' @param ds       (data.frame) Data frame
#' @param oldName  (character) Variable to rename
#' @param newName  (character) New name of variable
#' @return (data.frame) ds with variable renamed
#' @family sandra::dataFrames
#' @family SANDRA
#' @examples
#' # Create a new data frame
#' ds = data.frame.new( vars = c( "id", "x", "y", "z" ) );
#' # Rename "id" to "foo"
#' data.frame.renameVar( ds, "id", "foo" );
data.frame.renameVar = function( ds, oldName, newName ) {
  matches = names( ds ) == oldName;
  names( ds )[ matches ] = newName;
  return( ds );
}

#' Creates a data frame with zero rows
#'
#' @export
#' @param vars     (vector) Names of variables (columns) in data.frame
#' @return (data.frame) 
#' @family sandra::dataFrames
#' @family SANDRA
#' @examples
#' # See sandra::data.frame.affixNames
data.frame.new = function( vars = c() ) {
  output = data.frame( matrix( nrow = 0, ncol = length( vars ) ) );
  names( output ) = vars;
  return( output );
}
