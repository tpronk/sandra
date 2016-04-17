# ************************** 
# *** START OF CONFIGURATION

# This file contains SPRIF encoded data and is located in the "original" subdirectory
fileSource = "sprif1_data.csv"; 

# Type of task; trialdata filex gets postfixes with this text
task = "ibt";


# ************************
# *** END OF CONFIGURATION

dsEncoded = io$readData(
  fileSource, 
  sep = ";",
  original = TRUE,
  encoding = "UTF-8"
);

dsDecoded = decodeSprif1(
  dsEncoded
);

io$writeData(
  addPostfix( fileSource, "metadata" ),
  dsDecoded[[ "metadata" ]]
);

io$writeData(
  addPostfix( fileSource, "metadata" ),
  dsDecoded[[ "metadata" ]]
);

io$writeData(
  addPostfix( fileSource, "trialdata", task ),
  dsDecoded[[ "trialdata" ]]
);
