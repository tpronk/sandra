# ************************** 
# *** START OF CONFIGURATION

# This file contains JASMIN1 encoded data and is located in the "original" subdirectory
fileSource = "jasmin1_data.csv"; 

# Unique combinations of values in these columns identify a participation in a task
participationID = c( "UserID", "Session" );

# ************************
# *** END OF CONFIGURATION

dsEncoded = io$readData(
  fileSource, 
  original = TRUE,
  encoding = "UTF-8"
);

dsDecoded = decodeJasmin1(
  dsEncoded,
  participationID = participationID,
  verbose = FALSE
);

dsDecoded[["metadata"]] = data.frame.renameVar(
  dsDecoded[["metadata"]], 
  "set_id", 
  "participation_id"
);
dsDecoded[["metadata"]] = data.frame.renameVar(
  dsDecoded[["metadata"]], 
  "taskName", 
  "task_name"
);

io$writeData(
  addPostfix(fileSource, "metadata"),
  dsDecoded[["metadata"]]
);

for (task in names(dsDecoded[["trialdata"]])) {
  dsDecoded[["trialdata"]][[task]] = data.frame.renameVar(
    dsDecoded[["trialdata"]][[task]], 
    "set_id", 
    "participation_id"
  );
  dsDecoded[["trialdata"]][[task]] = data.frame.renameVar(
    dsDecoded[["trialdata"]][[task]], 
    "type", 
    "block_type"
  );  
  io$writeData(
    addPostfix(fileSource, task),
    dsDecoded[["trialdata"]][[task]]
  );
}