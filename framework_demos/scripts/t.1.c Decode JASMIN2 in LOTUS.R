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

dsDecoded = decodeJasmin2(
  dsEncoded, 
  c("task_start", "aat", "sciat", "gonogo", "slideshow", "screen"),  
  verbose = F
);

# Store decoded tables
for (curTable in names(dsDecoded)) {
  # Construct sequence reports and remove duplicates
  if (curTable != "task_start") {
    dsDecoded[[curTable]] = niceBy(
      dsDecoded[[curTable]],
      c("participation_id"),
      function (results, subset) {
        deduplicated = checkAndRemoveJasminDuplicates(subset);
        results = deduplicated[["evlogs"]];
        results[,"sequence_report"] = deduplicated[["sequence_report"]];
        return(results)
      },
      result_type = "data.frame_to_data.frame"
    );
  }
  
  io$writeData(
    addPostfix(fileSource, curTable),
    dsDecoded[[curTable]]
  );
}