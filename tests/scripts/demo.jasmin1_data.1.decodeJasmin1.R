# ***********
# *** Decodes JASMIN1 data encoded in LOTUS results files into metadata and trialdata

# *** Start of config
fileSource = "jasmin1_data.csv";
original   = TRUE; # If TRUE, data is in original folder, if not, in interim folder
# *** End of config

# Read data
ds = io$readData( fileSource, original );

# Decode data
dsDecoded = decodeJasmin1(
  ds,
  participationID = c( "UserID" )
);

# Store metadata and trialdata for each task in interim folder
io$writeData(
  io$addPostfix( fileSource, "metadata" ),
  dsDecoded$metadata
);
for( task in names( dsDecoded$trialdata ) ) {
  io$writeData(
    io$addPostfix( fileSource, task, "trialdata" ),
    dsDecoded$trialdata[[ task ]]
  );
}
