# ************************** 
# *** START OF CONFIGURATION

# This file contains SPRIF encoded data and is located in the "original" subdirectory
fileSource = "jasmin2_data.csv"; 

# Which tables to decode from the file, "task_start" should always be the first element
# of this tablesToDecode
tables = c("task_start", "aat", "sciat", "gonogo", "slideshow", "screen");

# ************************
# *** END OF CONFIGURATION

dsEncoded = io$readData(
  fileSource, 
  original = TRUE,
  encoding = "UTF-8"
);

dsDecoded = decodeJasmin2(
  dsEncoded, 
  tables,  
  verbose = F
);

# Store decoded tables
dsDecoded[["task_start"]][,"sequence_report"] = "";
for (curTable in names(dsDecoded)) {
  # Construct sequence reports and remove duplicates
  if (curTable != "task_start") {
    dsDecoded[[curTable]] = niceBy(
      dsDecoded[[curTable]],
      c("participation_id"),
      function (results, subset) {
        deduplicated = checkAndRemoveJasminDuplicates(subset);
        results = deduplicated[["evlogs"]];
        if(deduplicated[["sequence_report"]] != "") {
          dsDecoded[["task_start"]][
            as.character(dsDecoded[["task_start"]][,"participation_id"]) == as.character(results[1,"participation_id"]),
            "sequence_report"
          ] <<- paste(
            paste(curTable,":",deduplicated[["sequence_report"]], sep = ""),            
            dsDecoded[["task_start"]][
              as.character(dsDecoded[["task_start"]][,"participation_id"]) == 
              as.character(results[1,"participation_id"]), "sequence_report"
            ],
            sep = ","
          );
        }
        return(results)
      },
      result_type = "data.frame_to_data.frame"
    );
    io$writeData(
      addPostfix(fileSource, curTable),
      dsDecoded[[curTable]]
    );
  }
}

# Store task_start (with sequence_report);
io$writeData(
  addPostfix(fileSource, "metadata"),
  dsDecoded[["task_start"]]
)