# ************************** 
# *** START OF CONFIGURATION

# This file contains the original JASMIN1 encoded data, but the joining algorithms will only use
# derived files such as "jasmin1_data.metadata.csv" located in the "interim" subdirectory
fileSource = "jasmin2_data.csv"; 

# Identifies a participant in metadata
participantID = "UserID";

# Values in this column identify measurement moments in the output data wide, you'll get columns like this:
# ***.Session1, ***.Session2, etc.
sessionID = "Session";

# Postfixes identifying score input files (files named "jasmin2_data.sciat.dscores.csv" etc.)
scores = c("aat.medians", "sciat.dscores", "sciat.dscores");
# Postfixes used to identify each score in output file (and/or filter on)
tasks  = c("aat", "sciat_approach", "sciat_valence");

# Any processing done on the scores before joining
preprocess = function(score, task, ds) {
  # Recode Sesssion variable: only use the first word (everything before the first space)
  ds[,"Session"] = unlist(lapply(ds[,"Session"], function(x) { return(strsplit(x, " ")[[1]][1]); }));
  
  # Select appropriate rows based on value of task_name (aat, sciat_valence, or sciat_approach)
  if(task != "metadata") {
    ds = ds[ds[,"task_name"] == task,];
  }
  
  # Remove all non-completed tasks (for JASMIN1 data)
  # ds = ds[ ds[,"lotus_says"] == "task_done", ];
  return(ds);
}

# ************************
# *** END OF CONFIGURATION

dsMetadata = io$readData(
  addPostfix( fileSource,"metadata")
);
dsMetadata = preprocess("metadata", "metadata", dsMetadata);

dsAll = NULL; # All datasets joined together
dsCur = NULL; # Current dataset (made wide)

for(score_i in 1 : length(scores)) {
  score = scores[score_i];
  task = tasks[score_i];
  
  dsCur = io$readData(
    addPostfix(fileSource, score)
  );
  dsCur = preprocess(score, task, dsCur);
  
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
