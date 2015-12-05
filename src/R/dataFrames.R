# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Affix column names in a data frame
#'
#' @param ds       (data.frame) Data frame
#' @param prefix   (character) Prefix to add to column names (name becomes prefix.name)
#' @param postfix  (character) Postfix to add to column names (name becomes name.postfix)
#' @param not      (vector) Column names that should NOT be changed (get no prefix or postfix)
#' @return (data.frame) Data with affixed column names
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

data.frame.dropVar = function( ds, dropMe ) {
  matches = names( ds ) == dropMe;
  return( ds[ ,!matches ] );
}  
