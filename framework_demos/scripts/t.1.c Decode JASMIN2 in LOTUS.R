# ************************** 
# *** START OF CONFIGURATION

# This file contains SPRIF encoded data and is located in the "original" subdirectory
fileSource = "jasmin2_data.csv"; 

# ************************
# *** END OF CONFIGURATION

dsEncoded = io$readData(
  fileSource, 
  original = TRUE,
  encoding = "UTF-8"
);

dsDecoded = decodeJasmin2(dsEncoded);

for (curTable in names(dsDecoded)) {
  io$writeData(
    addPostfix(fileSource, curTable),
    dsDecoded[[curTable]]
  );
}