# ************************** 
# *** START OF CONFIGURATION

# This file contains the original JASMIN1 encoded data, but the joining algorithms will only use
# derived files such as "jasmin1_data.metadata.csv" located in the "interim" subdirectory
fileSource = "multi_session.csv"; 

# Identifies a participant in metadata
participantID = "UserID";

# Values in this column are used to make "joined" output data wide, so that you get columns like this:
# score.vpt.Session1, score.vpt.Session2, etc.
sessionID = "trainingNr";

# Task scores to join together (files named "jasmin1_data.scores.vpt.csv" etc.)
tasks = c( "vpt", "aat" );

# Artefact filtering; drop any cases with incorrect or duplicate data
dropArtefacts = function( task, ds ) {
  # Remove all non-completed tasks
  ds = ds[ ds[,"lotus_says"] == "task_done", ];
  return(ds);
}

# ************************
# *** END OF CONFIGURATION

dsMetadata = io$readData(
  addPostfix( fileSource, "metadata" )
);

dsAll = NULL; # All datasets joined together
dsCur = NULL; # Current dataset (made wide)

for( task in tasks ) {
  dsCur = io$readData(
    addPostfix( fileSource, "scores", task )
  );
  
  dsCur = dropArtefacts( task, dsCur );
  
  # Add task postfix
  dsCur = data.frame.affixNames( 
    dsCur,
    postfix = task,
    not = "UserID"
  );  
  
  # Convert to wide
  dsCur = makeWide(
    dsCur,
    c( "UserID" ),
    c( paste( sessionID, task, sep = "." ) )
  );
  
  # Join on UserID
  if( is.null( dsAll ) ) {
    dsAll = dsCur;
  } else {
    dsAll = niceMerge(
      dsAll,
      dsCur,
      participantID,
      all.x = TRUE,
      all.y = TRUE
    );
  }  
}

io$writeData(
  addPostfix( fileSource, "joined" ),
  dsAll
); 
