# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Installs or loads a SANDRA Analysis Framework
#' 
#' A SANDRA Analysis Framework, which is a folder containing the following:
#' - Load Analysis.R Loads SANDRA and sets up a FileIO for this AnalysisFramework
#' - scripts/        Analysis scripts
#' - original/       Original Data
#' - interim/        Interim Data
#'
#' @param pathAnalasis  (character) Path to analysis directory
#' @return NULL
installAnalysisFramework = function( pathAnalysis = NA ) {
  # Install devtools package
  if( !( "devtools" %in% row.names( installed.packages() ) ) ) {
    install.packages( "devtools" );
  }
  library( "devtools" )
  
  # Install SANDRA from github
  if( !( "sandra" %in% row.names( installed.packages() ) ) ) {
    install_github( "tpronk/SANDRA/package" );
  }
  library( "sandra" )
  
  # *** Prepare Analysis folder
  # Ask for Analysis folder
  if( is.na( pathAnalysis ) ) {
    pathAnalysis = choose.dir(
      caption = "Select folder to install SANDRA Analysis Framework in"
    );
  }
  # Convert backslashes to forward slashes
  pathAnalysis = paste( 
    strsplit( pathAnalysis,  "\\", fixed=T )[[1]],
    collapse="/" 
  );
  
  # *** Create sub-folders
  subfolders = c( "scripts", "original", "interim" );
  for( subfolder in subfolders ) {
    subpath = paste( pathAnalysis, subfolder, sep = "/")
    if( !file.exists( subpath ) ) {
      dir.create( subpath );
    }
  }
  
  # *** Create "Load Analysis.R" script
  loadAnalysisFile = paste( pathAnalysis, "Load Analysis.R", sep = "/" );
  if( !file.exists( loadAnalysisFile ) ) {
    # Construct R statements for loading SANDRA Analysis Framework
    output = c(
      "library( \"sandra\" );",
      paste( "io  = FrameworkFileIO( \"", pathAnalysis,  "\" );", sep = "" )
    );
    # Write statements to "Load Analysis"
    write(
      output,
      loadAnalysisFile
    );
    # Run "Load Analysis.R"
    source(
      loadAnalysisFile
    );
  }
}  