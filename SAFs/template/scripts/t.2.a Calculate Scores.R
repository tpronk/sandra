# ************************** 
# *** START OF CONFIGURATION

# This file contains the original JASMIN1/JASMIN2/SPRIF1 encoded data, but the scoring 
# algorithms will only use derived files such as "jasmin2_data.sciat.csv" located in the
# "interim" folder
fileSource = "jasmin2_data.csv"; 

# Scoring settings
aux_report = c("task_n", "factor_scores");
scorings = list(
  # AAT Median, Wen Si 2017-02-08 & Marilisa Boffo
  medians.aat = list(
    # *** General scoring settings
    data         = "aat", # File containing trialdata
    run_var      = "participation_id", # Each participation has a unique value for run_var
    resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
    rt_var       = "rt", # Response time in ms
    
    # *** Dropping and reporting
    #fast_drop    = fast_drop, # Drop RTs lower than this value
    #slow_drop    = slow_drop, # Drop RTs higher than this value
    #resp_drop    = c("NA", 2, 3, 4), # Drop responses with these values
    #fast_report  = fast_drop, # Count RTs lower than this value in output file as "outlier_fast_n"
    #slow_report  = slow_drop, # Count RTs lower than this value in output file as "outlier_slow_n"
    #resp_report  = c(1, 3), # Count responses with these values as "resp_1_n", "resp_3_n", etc.
    
    # *** Aggregation specific settings
    type  = "aggregation", # Type of scoring; dscore or aggregation    
    # For each participation calculate scores for approach & avoid for test & control
    aggregation_factors = c( "trial_type", "cat" ),
    # Scores are calculated as median RTs
    aggregation_factor_function = function( rts ) {
      return( median( rts ) )
    },
    # For each participation, calculate difference between approach and avoid scores
    aggregation_run_function = function( wide ) {
      if (!all(c("score.test.avoid","score.test.approach", "score.control.avoid", "score.control.approach") %in% names(wide))) {
        return (NA);
      }
      score = (wide["score.test.avoid"] - wide["score.test.approach"]) - (wide["score.control.avoid"] - wide["score.control.approach"]);
      return(score);
    },
    # Additional variables to report
    aux_report = aux_report,
    factor_filter = list(
      resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
      rt_var       = "rt", # Response time in ms
      fast_drop    = fast_drop, # Drop RTs lower than this value
      slow_drop    = slow_drop, # Drop RTs higher than this value
      resp_drop    = c("NA", 2, 3, 4), # Drop responses with these values
      fast_report  = fast_drop, # Count RTs lower than this value in output file as "outlier_fast_n"
      slow_report  = slow_drop, # Count RTs lower than this value in output file as "outlier_slow_n"
      resp_report  = c(1, 3)  # Count responses with these values as "resp_1_n", "resp_3_n", etc.
    ),
    # Factor scores to report
    factor_report = c(
      "score.control.approach","score.test.approach","score.control.avoid","score.test.avoid",
      "task_n.control.approach","task_n.test.approach","task_n.control.avoid","task_n.test.avoid",
      "resp_1_n.control.approach","resp_1_n.test.approach","resp_1_n.control.avoid","resp_1_n.test.avoid",
      "resp_3_n.control.approach","resp_3_n.test.approach","resp_3_n.control.avoid","resp_3_n.test.avoid",
      "outlier_slow_n.control.approach","outlier_slow_n.test.approach","outlier_slow_n.control.avoid","outlier_slow_n.test.avoid",
      "outlier_fast_n.control.approach","outlier_fast_n.test.approach","outlier_fast_n.control.avoid","outlier_fast_n.test.avoid"
    ),
    
    # Function for filtering and/or recoding trial data before it's fed into the scoring algorithm
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      # NB - Filter on assessments blocks etc. here
      ds = ds[!is.na(ds[,"block_type"]) & ds[,"block_type"] == "assess",];
      #g <<- ds;
      return(ds);
    }
  ),
  # AAT D-Score Test, Wen Si 2017-02-08 & Marilisa Boffo
  d2sd_test.aat = list(
    # *** General scoring settings
    data         = "aat", # File containing trialdata
    run_var      = "participation_id", # Each participation has a unique value for run_var
    resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
    rt_var       = "rt", # Response time in ms
    
    # *** Dropping and reporting
    fast_report  = fast_drop,
    slow_report  = slow_drop,
    fast_drop    = fast_drop, # Drop RTs lower than this value
    slow_drop    = slow_drop, # Drop RTs higher than this value
    resp_drop    = c("NA", 3, 4), # Drop responses with these values
    
    # *** D-Score specific settings
    type         = "dscore", # Type of scoring; dscore or aggregation    
    comp_var     = "trial_type", # Identifies congruent and incongruent blocks
    comp_levels  = c("approach", "avoid"), # A positive score is 'towards' first element of comp_levels
    resp_correct = 1, # Value of resp_var that identifies correct responses
    resp_penalty = c(2), # Value of resp_var that identifies responses whose RT should be penalized
    rt_penalty   = "2sd", # Type of RT penalty
    aux_report   = c("correct_mean"), # Additional variables to report
    
    # Function for filtering and/or recoding trial data before it's fed into the scoring algorithm
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      # NB - Filter on assessments blocks etc. here
      ds = ds[!is.na(ds[,"block_type"]) & ds[,"block_type"] == "assess" & ds[,"cat"] == "test",];
      
      return(ds);
    }
  ),
  # AAT D-Score Control, Wen Si 2017-02-08 & Marilisa Boffo
  d2sd_control.aat = list(
    # *** General scoring settings
    data         = "aat", # File containing trialdata
    run_var      = "participation_id", # Each participation has a unique value for run_var
    resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
    rt_var       = "rt", # Response time in ms
    
    # *** Dropping and reporting
    fast_report  = fast_drop,
    slow_report  = slow_drop,
    fast_drop    = fast_drop, # Drop RTs lower than this value
    slow_drop    = slow_drop, # Drop RTs higher than this value
    resp_drop    = c("NA", 3, 4), # Drop responses with these values
    
    # *** D-Score specific settings
    type         = "dscore", # Type of scoring; dscore or aggregation    
    comp_var     = "trial_type", # Identifies congruent and incongruent blocks
    comp_levels  = c("approach", "avoid"), # A positive score is 'towards' first element of comp_levels
    resp_correct = 1, # Value of resp_var that identifies correct responses
    resp_penalty = c(2), # Value of resp_var that identifies responses whose RT should be penalized
    rt_penalty   = "2sd", # Type of RT penalty
    aux_report   = c("correct_mean"), # Additional variables to report
    
    # Function for filtering and/or recoding trial data before it's fed into the scoring algorithm
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      # NB - Filter on assessments blocks etc. here
      ds = ds[!is.na(ds[,"block_type"]) & ds[,"block_type"] == "assess" & ds[,"cat"] == "control",];
      
      return(ds);
    }
  ),
  # VPT Median probe on top, Wen Si 2017-02-08 & Marilisa Boffo
  medians_top.vpt = list(
    # *** General scoring settings
    data         = "vpt", # File containing trialdata
    run_var      = "participation_id", # Each participation has a unique value for run_var
    resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
    rt_var       = "rt", # Response time in ms
    
    # *** Dropping and reporting
    fast_drop    = fast_drop, # Drop RTs lower than this value
    slow_drop    = slow_drop, # Drop RTs higher than this value
    resp_drop    = c("NA", 2, 3, 4), # Drop responses with these values
    fast_report  = fast_drop, # Count RTs lower than this value in output file as "outlier_fast_n"
    slow_report  = slow_drop, # Count RTs lower than this value in output file as "outlier_slow_n"
    resp_report  = c(1, 3), # Count responses with these values as "resp_1_n", "resp_3_n", etc.
    
    # *** Aggregation specific settings
    type  = "aggregation", # Type of scoring; dscore or aggregation    
    # For each participation calculate scores for approach & avoid for test & control
    aggregation_factors = c("patt"),
    # Scores are calculated as median RTs
    aggregation_factor_function = function (rts) {
      return (median(rts))
    },
    # For each participation, calculate difference between approach and avoid scores
    aggregation_run_function = function(wide) {
      score = (wide["score.no"] - wide["score.yes"]);
      return(score);
    },
    # Additional variables to report
    aux_report = aux_report,
    
    # Function for filtering and/or recoding trial data before it's fed into the scoring algorithm
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      # NB - Filter on assessments blocks etc. here
      ds = ds[!is.na(ds[,"block_type"]) & ds[,"block_type"] == "assess" & ds[,"keep"] == "yes",];
      
      return(ds);
    }
  ),  
  # VPT Median probe after, Wen Si 2017-02-08 & Marilisa Boffo
  medians_after.vpt = list(
    # *** General scoring settings
    data         = "vpt", # File containing trialdata
    run_var      = "participation_id", # Each participation has a unique value for run_var
    resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
    rt_var       = "rt", # Response time in ms
    
    # *** Dropping and reporting
    fast_drop    = fast_drop, # Drop RTs lower than this value
    slow_drop    = slow_drop, # Drop RTs higher than this value
    resp_drop    = c("NA", 2, 3, 4), # Drop responses with these values
    fast_report  = fast_drop, # Count RTs lower than this value in output file as "outlier_fast_n"
    slow_report  = slow_drop, # Count RTs lower than this value in output file as "outlier_slow_n"
    resp_report  = c(1, 3), # Count responses with these values as "resp_1_n", "resp_3_n", etc.
    
    # *** Aggregation specific settings
    type  = "aggregation", # Type of scoring; dscore or aggregation    
    # For each participation calculate scores for approach & avoid for test & control
    aggregation_factors = c("patt"),
    # Scores are calculated as median RTs
    aggregation_factor_function = function (rts) {
      return (median(rts))
    },
    # For each participation, calculate difference between approach and avoid scores
    aggregation_run_function = function(wide) {
      score = (wide["score.no"] - wide["score.yes"]);
      return(score);
    },
    # Additional variables to report
    aux_report = aux_report,
    
    # Function for filtering and/or recoding trial data before it's fed into the scoring algorithm
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      # NB - Filter on assessments blocks etc. here
      ds = ds[!is.na(ds[,"block_type"]) & ds[,"block_type"] == "assess" & !is.na(ds[,"keep"]) & ds[,"keep"] == "no",];
      
      return(ds);
    }
  ),    
  
  # VPT D-Score probe on top, Wen Si 2017-02-08 & Marilisa Boffo
  d2sd_top.vpt = list(
    # *** General scoring settings
    data         = "vpt", # File containing trialdata
    run_var      = "participation_id", # Each participation has a unique value for run_var
    resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
    rt_var       = "rt", # Response time in ms
    
    # *** Dropping and reporting
    fast_drop    = fast_drop, # Drop RTs lower than this value
    slow_drop    = slow_drop, # Drop RTs higher than this value
    resp_drop    = c("NA", 3, 4), # Drop responses with these values
    
    # *** D-Score specific settings
    type         = "dscore", # Type of scoring; dscore or aggregation    
    comp_var     = "patt", # Identifies congruent and incongruent blocks
    comp_levels  = c("yes", "no"), # A positive score is 'towards' first element of comp_levels
    resp_correct = 1, # Value of resp_var that identifies correct responses
    resp_penalty = c(2), # Value of resp_var that identifies responses whose RT should be penalized
    rt_penalty   = "2sd", # Type of RT penalty
    aux_report   = c(), # Additional variables to report
    
    # Function for filtering and/or recoding trial data before it's fed into the scoring algorithm
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      # NB - Filter on assessments blocks etc. here
      ds = ds[!is.na(ds[,"block_type"]) & ds[,"block_type"] == "assess" & ds[,"keep"] == "yes",];
      
      return(ds);
    }
  ),
  
  # VPT D-Score probe after, Wen Si 2017-02-08 & Marilisa Boffo
  d2sd_after.vpt = list(
    # *** General scoring settings
    data         = "vpt", # File containing trialdata
    run_var      = "participation_id", # Each participation has a unique value for run_var
    resp_var     = "response", # Identifies responses, coded in (1 = correct, 2 = incorrect, 3 = timeout, 4 = invalid, NA = disrupted)
    rt_var       = "rt", # Response time in ms
    
    # *** Dropping and reporting
    fast_drop    = fast_drop, # Drop RTs lower than this value
    slow_drop    = slow_drop, # Drop RTs higher than this value
    resp_drop    = c("NA", 3, 4), # Drop responses with these values
    
    # *** D-Score specific settings
    type         = "dscore", # Type of scoring; dscore or aggregation    
    comp_var     = "patt", # Identifies congruent and incongruent blocks
    comp_levels  = c("yes", "no"), # A positive score is 'towards' first element of comp_levels
    resp_correct = 1, # Value of resp_var that identifies correct responses
    resp_penalty = c(2), # Value of resp_var that identifies responses whose RT should be penalized
    rt_penalty   = "2sd", # Type of RT penalty
    aux_report   = c(), # Additional variables to report
    
    # Function for filtering and/or recoding trial data before it's fed into the scoring algorithm
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      # NB - Filter on assessments blocks etc. here
      ds = ds[!is.na(ds[,"block_type"]) & ds[,"block_type"] == "assess" & ds[,"keep"] == "no",];
      
      return(ds);
    }
  ),
  
  iat_d2sd = list(
    data         = "iat", # File containing trialdata
    type         = "dscore",
    
    run_var      = "participation_id",
    resp_var     = "response",
    rt_var       = "rt",
    comp_var     = "recoded_block",
    comp_levels  = c( "tar1att1", "tar1att2" ), # Bias calculated towards first element
    
    fast_drop    = 300,
    fast_report  = 300,
    
    slow_drop    = 3000,
    slow_report  = 3000,
    
    sd_drop      = 1.5,
    sd_report    = 1.5,  
    
    resp_drop    = c( "NA", 0, 3, 4 ),
    resp_report  = c( 1, 3 ),
    
    resp_correct = 1,  
    resp_penalty = c( 2 ),
    rt_penalty   = "2sd",
    
    aux_report   = c(),
    
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      
      # Recode tar1att1_1 and tar1att1_2 to tar1att1 etc.
      ds[,"recoded_block"] = ds[,"block_type"];
      ds[ds[,"recoded_block"] == "tar1att1_1", "recoded_block"] = "tar1att1";
      ds[ds[,"recoded_block"] == "tar1att1_2", "recoded_block"] = "tar1att1";
      ds[ds[,"recoded_block"] == "tar1att2_1", "recoded_block"] = "tar1att2";
      ds[ds[,"recoded_block"] == "tar1att2_2", "recoded_block"] = "tar1att2";
      
      return(ds);
    }    
  ),
  
  stiat_d2sd = list(
    data         = "stiat", # File containing trialdata
    type         = "dscore",
    
    run_var      = "participation_id",
    resp_var     = "response",
    rt_var       = "rt",
    comp_var     = "block_type",
    comp_levels  = c( "tar1att1", "tar1att2" ), # Bias calculated towards first element
    
    fast_drop    = 300,
    fast_report  = 300,
    
    slow_drop    = 3000,
    slow_report  = 3000,
    
    sd_drop      = 1.5,
    sd_report    = 1.5,  
    
    resp_drop    = c( "NA", 0, 3, 4 ),
    resp_report  = c( 1, 3 ),
    
    resp_correct = 1,  
    resp_penalty = c( 2 ),
    rt_penalty   = "2sd",
    
    aux_report   = c(),
    
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      return(ds);
    }    
  ),
  
  iat_d600 = list(
    data         = "iat", # File containing trialdata
    type         = "dscore",
    
    run_var      = "participation_id",
    resp_var     = "response",
    rt_var       = "rt",
    comp_var     = "recoded_block",
    comp_levels  = c( "tar1att1", "tar1att2" ), # Bias calculated towards first element
    
    fast_drop    = 300,
    fast_report  = 300,
    
    slow_drop    = 3000,
    slow_report  = 3000,
    
    sd_drop      = 1.5,
    sd_report    = 1.5,  
    
    resp_drop    = c( "NA", 0, 3, 4 ),
    resp_report  = c( 1, 3 ),
    
    resp_correct = 1,  
    resp_penalty = c( 2 ),
    rt_penalty   = 600,
    
    aux_report   = c(),
    
    select_data = function (ds) {
      # If data contains one row per attempt, then only use first attempt at each trial
      if ("attempt" %in% names(ds)) {
        ds = ds[ds[,"attempt"] == 0,];
      }
      
      # Recode tar1att1_1 and tar1att1_2 to tar1att1 etc.
      ds[,"recoded_block"] = ds[,"block_type"];
      ds[ds[,"recoded_block"] == "tar1att1_1", "recoded_block"] = "tar1att1";
      ds[ds[,"recoded_block"] == "tar1att1_2", "recoded_block"] = "tar1att1";
      ds[ds[,"recoded_block"] == "tar1att2_1", "recoded_block"] = "tar1att2";
      ds[ds[,"recoded_block"] == "tar1att2_2", "recoded_block"] = "tar1att2";
      
      return(ds);
    }    
  )
)
  
# ************************
# *** END OF CONFIGURATION


dsMetadata = io$readData(
  addPostfix( fileSource, "metadata" )
);
dsMetadata = dsMetadata[,c(
  "participation_id",
  metadataVars
)];

if (exists("scoringsOverride")) {
  selectedScorings = scorings[scoringsOverride];
} else {
  selectedScorings = scorings;
}

for(scoring_i in names(selectedScorings)) {
  scoring = selectedScorings[[scoring_i]];
  if (io$existsData(
    addPostfix(fileSource, scoring[["data"]])
  )) {
    #print(addPostfix(fileSource, scoring[["data"]]));
    dsTrialdata = io$readData(
      addPostfix(fileSource, scoring[["data"]])
    );
    # Merge with metadata
    dsTrialdata = leftMerge(
      dsTrialdata,    
      dsMetadata,
      c("participation_id")
    )    
    dsTrialdata = scoring[["select_data"]](dsTrialdata);
    
    dsRawScores = calculateScores(
      scoring[[ "type" ]],
      dsTrialdata,
      scoring
    );
    
    # Merge with metadata
    dsScores = leftMerge(
      dsRawScores,    
      dsMetadata,
      c("participation_id")
    )
    
    io$writeData(
      addPostfix(fileSource, scoring_i),
      dsScores
    );  
  }
}

