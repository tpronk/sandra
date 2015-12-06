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
  # *** Prepare Analysis folder
  # Ask for Analysis folder
  if( is.na( pathAnalysis ) ) {
    pathAnalysis = choose.dir(
      caption = "Select directory to install SANDRA Analysis Framework in"
    );
  }
  
  # Folder picker canceled? Abort installation
  if( is.na( pathAnalysis ) ) {
    print( "sandra::installAnalysisFramework. No Analysis directory selected; installation of SANDRA Analysis Framework aborted")
    return();
  }
  
  # Convert backslashes to forward slashes
  pathAnalysis = paste( 
    strsplit( pathAnalysis,  "\\", fixed=T )[[1]],
    collapse="/" 
  );
  
  print( paste( 
    "sandra::installAnalysisFramework. Selected directory ",
    pathAnalysis,
    sep = ""
  ) );  
  
  # *** Create sub-folders
  subfolders = c( "scripts", "original", "interim" );
  for( subfolder in subfolders ) {
    subpath = paste( pathAnalysis, subfolder, sep = "/")
    report = c(
      "sandra::installAnalysisFramework. Directory ",
      subpath
    );
    if( !file.exists( subpath ) ) {
      dir.create( subpath );
      report = c( report, " created");
    } else {
      report = c( report, " already exists");
    }
  }
  
  # *** Create "Load SANDRA.R" script
  loadAnalysisFile = paste( pathAnalysis, "Load SANDRA.R", sep = "/" );
  if( !file.exists( loadAnalysisFile ) ) {
    # Construct R statements for loading SANDRA Analysis Framework
    output = c(
      "library( \"sandra\" );",
      paste( "io  = FrameworkFileIO( \"", pathAnalysis,  "\" );", sep = "" )
    );
    # Write statements to "Load SANDRA"
    write(
      output,
      loadAnalysisFile
    );
    print( "sandra::installAnalysisFramework. \"Load SANDRA.R\" created" );
    
    # Run "Load Analysis.R"
    source(
      loadAnalysisFile
    );
  }
}  