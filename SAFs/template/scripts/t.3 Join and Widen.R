# ************************** 
# *** START OF CONFIGURATION

# This file contains the original JASMIN1 encoded data, but the joining algorithms will only use
# derived files such as "jasmin1_data.metadata.csv" located in the "interim" subdirectory
fileSource = "jasmin2_data.csv"; 

# Identifies a participant in metadata
participantID = "UserID";

# Values in this column are used to make "joined" output data wide, so that you get columns like this:
# score.vpt.Session1, score.vpt.Session2, etc.
sessionID = "Session";

# Tables to use  (files named "jasmin1_data.scores.vpt.csv" etc.)
tables = c("aat", "sciat", "sciat");
tasks  = c("aat", "sciat_approach", "sciat_valence");


# Any processing done on the individual tables before joining
preprocess = function( table, task, ds ) {
  # Recode Sesssion variable: only use the first word (everything before the first space)
  ds[,"Session"] = unlist(lapply(ds[,"Session"], function(x) { return(strsplit(x, " ")[[1]][1]); }));
  
  # Select appropriate rows based on task_name variable (aat, sciat_valence, or sciat_approach)
  if(task != "metadata") {
    ds = ds[ds[,"task_name"] == task,];
  }
  
  # Remove all non-completed tasks (for JASMIN1 data)
  # ds = ds[ ds[,"lotus_says"] == "task_done", ];
  return(ds);
}

# This suffix  identifies a "metadata" file. This suffix is appended to fileSource to 
# identify the metadata file, for example: jasmin2_data.task_start.csv
#   - "metadata" for JASMIN1 and SPRIF1 data
#   - "task_start" for SPRIF1 data
suffixMetadata = "task_start";

# ************************
# *** END OF CONFIGURATION

dsMetadata = io$readData(
  addPostfix( fileSource, suffixMetadata )
);
dsMetadata = preprocess(suffixMetadata, "metadata", dsMetadata)

dsAll = NULL; # All datasets joined together
dsCur = NULL; # Current dataset (made wide)


for( table_i in 1 : length(tables) ) {
  curTable = tables[table_i];
  task = tasks[table_i];
  
  dsCur = io$readData(
    addPostfix( fileSource, "scores", curTable )
  );
  
  dsCur = preprocess( curTable, task, dsCur );
  
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
