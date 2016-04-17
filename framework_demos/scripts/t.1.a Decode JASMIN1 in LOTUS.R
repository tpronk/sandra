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

io$writeData(
  addPostfix( fileSource, "metadata" ),
  dsDecoded[[ "metadata" ]]
);

for( task in names( dsDecoded[["trialdata"]] ) ) {
  io$writeData(
    addPostfix( fileSource, "trialdata", task ),
    dsDecoded[[ "trialdata" ]][[ task ]]
  );
}