# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Drops rows and reports about rows in data based on response or rt
#'
#' @export
#' @param result      (list) Results so far (see ?nice_by).
#' @param ds_subset   (data.frame) Data to apply dropping and reporting to
#' @param settings    (list) See table below
#' @return (list) Element "ds_subset" contains data with dropped responses; "result" contains responses and RTs that are reported
#' @details 
#' The table below shows an overview of elements in settings and their` meaning.
#' \tabular{lllll}{
#'   \strong{Name} \tab \strong{Type} \tab \strong{Required?} \tab  \strong{Example Value} \tab \strong{Description} \cr
#'   resp_drop \tab vector \tab No \tab \code{c("NA",0,3,4)} \tab Drop all responses with these values. \cr
#'   resp_report \tab vector \tab No \tab \code{c(1)} \tab Report number of responses with these values (per value individually) as "resp_<value>_n". \cr
#'   fast_drop \tab integer \tab No \tab \code{300} \tab Drop all response times lower than this value. \cr
#'   fast_report \tab integer \tab No \tab \code{300} \tab Report number of response times lower than this value in scores as "outlier_fast_n". This occurs after dropping rows according to resp_drop. \cr
#'   slow_drop \tab integer \tab No \tab \code{3000} \tab Drop all response times higher than this value. \cr
#'   slow_report \tab integer \tab No \tab \code{3000} \tab Report number of response times higher than this value in scores as "outlier_slow_n". This occurs after dropping rows according to resp_drop.\cr
#'   sd_drop \tab numeric \tab No \tab \code{1.5} \tab Drop all response times higher than mean response times of correct responses + sd_drop * SD, and those lower than mean - sd_drop * SD. \cr
#'   sd_report \tab numeric \tab No \tab \code{1.5} \tab Report number of response times higher than mean response times of correct responses + sd_drop * SD, or those lower than mean - sd_drop * SD in scores as "outlier_sd_n". This occurs after dropping rows according to resp_drop, slow_drop, and fast_drop. \cr
#' }
#' @family SANDRA
dropAndReport =  function( result, ds_subset, settings = list() ) {
  # These settings are used often
  resp_var = settings[[ "resp_var" ]];
  rt_var   = settings[[ "rt_var"   ]];

  # Report no. of trials
  if( "task_n" %in% settings[[ "aux_report" ]] ) {
    result[[ "task_n" ]] = nrow( ds_subset );
  }
  
  # Report responses
  for( resp_report in settings[[ "resp_report" ]] ) {
    if( resp_report == "NA" ) {
      result[[ paste( "resp", resp_report, "n", sep = "_" ) ]] =
        sum( is.na( ds_subset[ ,resp_var ] ) );
    } else {
      result[[ paste( "resp", resp_report, "n", sep = "_" ) ]] =
        sum( ds_subset[ ,resp_var ] == resp_report, na.rm = TRUE );
    }
  }  
  # Drop responses
  for( resp_drop in settings[[ "resp_drop" ]] ) {
    if( resp_drop == "NA" || is.na( resp_drop ) ) {
      ds_subset = ds_subset[ 
        !is.na( ds_subset[ ,resp_var ] ),
        ];
    } else {
      ds_subset = ds_subset[ 
        ds_subset[ ,resp_var ] != resp_drop,
        ];
    }
  }    
  
  # Report fast
  if( !is.null( settings[[ "fast_report" ]] ) ) {
    result[[ "outlier_fast_n" ]] =
      sum( ds_subset[ ,"rt" ] < settings[[ "fast_report" ]] );
  }
  # Drop fast
  if( !is.null( settings[[ "fast_drop" ]] ) ) {
    ds_subset = ds_subset[ 
      !is.na( ds_subset[ ,"rt" ] ) &
        ds_subset[ ,"rt" ] >= settings[[ "fast_drop" ]],
      ];
  }
  
  # Report slow
  if( !is.null( settings[[ "slow_report" ]] ) ) {
    result[[ "outlier_slow_n" ]] =
      sum( ds_subset[ ,"rt" ] > settings[[ "slow_report" ]] );
  }
  # Drop slow
  if( !is.null( settings[[ "slow_drop" ]] ) ) {
    ds_subset = ds_subset[ 
      !is.na( ds_subset[ ,"rt" ] ) &
        ds_subset[ ,"rt" ] <= settings[[ "slow_drop" ]],
      ];
  }  
  
  # Report SD
  if( !is.null( settings[[ "sd_report" ]] ) ) {
    sd_report = settings[[ "sd_report" ]];
    cur_mean = mean( ds_subset[ ,"rt" ] );
    cur_sd   = sd(   ds_subset[ ,"rt" ] );
    sd_fast  = cur_mean - sd_report * cur_sd;
    sd_slow  = cur_mean + sd_report * cur_sd;
    result[[ "outlier_sd_n" ]] =
      sum( 
        ds_subset[ ,"rt" ] > sd_slow
        | ds_subset[ ,"rt" ] < sd_fast
      );
  }
  # Drop SD
  if( !is.null( settings[[ "sd_drop" ]] ) ) {
    sd_drop = settings[[ "sd_drop" ]];
    cur_mean = mean( ds_subset[ ,"rt" ] );
    cur_sd   = sd(   ds_subset[ ,"rt" ] );
    sd_fast  = cur_mean - sd_drop * cur_sd;
    sd_slow  = cur_mean + sd_drop * cur_sd;
    ds_subset = ds_subset[
      (
        ds_subset[ ,"rt" ] <= sd_slow
        & ds_subset[ ,"rt" ] >= sd_fast
      ) | is.na( ds_subset[ ,"rt" ] ),
      ];
  }
  return(list(
    result = result,
    ds_subset = ds_subset
  ));
}
#' Calculates d-scores from trial data
#'
#' @export
#' @param ds          (data.frame) Trial data.
#' @param settings    (list) D-score calculation settings.
#' @param splithalves (integer) If 0, return a single set of d-scores, if > 0, return randomized split-halve reliability averaged over splithalves iterations.
#' @param what        (character) If "all", calculate over full task data, if "first_second" calculate 2 dscores; one for first and one for second halve. Only used if splithalves == 0.
#' @param verbose     (logical) If TRUE, then print debug output.
#' @param ...         Remaining arguments are passed to an internal scoring function. Not used at the moment.
#' @return (data.frame) Calculated d-scores.
#'
#' @details 
#' The table below shows an overview of elements in settings and their meaning.
#' \tabular{lllll}{
#'   \strong{Name} \tab \strong{Type} \tab \strong{Required?} \tab  \strong{Example Value} \tab \strong{Description} \cr
#'   comp_var \tab character \tab Yes \tab \code{"appr"} \tab Variable in trialdata that identifies compatible/incompatible blocks. \cr
#'   comp_levels \tab character \tab Yes \tab \code{c( "yes", "no" )} \tab Vector containing two elements, corresponding to the values of comp_var that identify compatible and incompatible blocks respectively. Bias score is calculated towards first element. \cr
#'   resp_correct \tab character \tab Yes \tab \code{1} \tab Value for correct responses. \cr
#'   resp_penalty \tab vector \tab No \tab \code{c(2)} \tab Responses whose response times are to be replaced by a penalty RT. \cr
#'   rt_penalty \tab character/numeric \tab No \tab \code{"2sd"} \tab Default = "2sd". Use "2sd" to replace RTs of incorrect responses with the mean + 2SD of correct responses for current block. Use any other number to penalize with that number. For example: 600 penalizes with mean + 600. \cr
#'   aux_report \tab vector \tab No \tab \code{c( "correct_n", "task_n" )} \tab Additional variables to report. See separate section covering aux_report. \cr
#' }
#'
#' The table below shows an overview of elements in aux_report and their meaning.
#' \tabular{lllll}{
#'   \strong{Name} \tab \strong{Scope} \tab \strong{Description} \tab \cr
#'   correct_n \tab block \tab Number of correct responses \cr
#'   correct_mean \tab block \tab Mean or RT of correct responses \cr
#'   correct_sd \tab block \tab SD of RT of correct responses \cr
#'   penalty \tab block \tab RT penalty \cr
#'   adjusted_mean \tab block \tab Mean RT after applying penalty \cr
#'   inclusive_sd \tab task \tab SD of response times for compatible and incompatible blocks taken together \cr
#'   task_n \tab task \tab Number of trials in task \cr
#' }
#'
#' @seealso \code{\link[sandra]{dropAndReport}} for additional settings that can be used for 
#' dropping and reporting responses and response times before scores are calculated.
#' 
#' @family SANDRA
#' @examples
#' See: SANDRA/framework_demos/scripts/0.t.2 Calculate Scores.R
calculateDScores = function( ds, settings, splithalves = 0, splithalf_method = "pearson", verbose = F, what = "all", ... ) {
  # ***********************
  # *** Inner Functions ***
  # ***********************
  
  # Calculate one dscore from a ds_subset of dataset (for one run of a task)
  one_dscore = function( result, ds_subset, settings = list() ) {
    # g_ds_subset <<- ds_subset;
    
    # These settings are used often
    comp_var = settings[[ "comp_var" ]];
    resp_var = settings[[ "resp_var" ]];
    rt_var   = settings[[ "rt_var"   ]];
    ds_subset[ ,rt_var ] = suppressWarnings( as.numeric( ds_subset[ ,rt_var ] ) );

    # Drop and report responses and rt's
    temp = dropAndReport( result, ds_subset, settings );
    result = temp[["result"]];
    ds_subset = temp[["ds_subset"]];
    
    
    if( !("split_var" %in% names( scorings[[task]] ) ) ) {
      result = merge(
        result,
        single_dscore( result, ds_subset, settings )
      );
    } else {
      g_settings <<-settings;
      g_ds_subset <<- ds_subset;
      g_result <<- result;
      settings = g_settings;
      ds_subset = g_ds_subset;
      result = g_result;      
      
      splitVar    = settings[["split_var"]];
      splitLevels = unique( ds_subset[ ,splitVar ] );
      scores      = c();
      for( splitLevel in splitLevels ) {
        ds_subset_split = ds_subset[ ds_subset[,splitVar] == splitLevel, ];
        new_result = single_dscore( list(), ds_subset_split, settings );
        scores = c( scores, new_result[["score"]]  )
        new_result = data.frame.affixNames( new_result, NULL, splitLevel );
        result = merge( result, new_result );
      }
      result[["score"]] = mean(scores);
    }
    return(result);
  }      
    
  single_dscore = function( result, ds_subset, settings = list() ) {
    # These settings are used often
    comp_var = settings[[ "comp_var" ]];
    resp_var = settings[[ "resp_var" ]];
    rt_var   = settings[[ "rt_var"   ]];
    
    # ***********
    # *** Apply penalty to RT and calculate means
    adjusted_means = list();
    for( comp_level in settings[[ "comp_levels" ]] ) {
      adjusted_means[[ comp_level ]] = NA;
      ds_subset_current = ds_subset[
        ds_subset[ ,comp_var ] == comp_level &
        ds_subset[ ,resp_var ] == settings[[ "resp_correct" ]],
      ];
      
      # N of correct responses
      if( "correct_n" %in% settings[[ "aux_report" ]] ) {
        result[[ paste( "correct_n", comp_level, sep = "." ) ]] = nrow( ds_subset_current );
      }    
      
      # Mean of RT of correct responses
      correct_mean = mean( ds_subset_current[ ,rt_var ], na.rm = T );
      if( "correct_mean" %in% settings[[ "aux_report" ]] ) {
        result[[ paste( "correct_mean", comp_level, sep = "." ) ]] = correct_mean;
      }
      
      # SD of RT of correct responses
      correct_sd   = sd( ds_subset_current[ ,rt_var ], na.rm = T );
      if( "correct_sd" %in% settings[[ "aux_report" ]] ) {
        result[[ paste( "correct_sd", comp_level, sep = "." ) ]] = correct_sd;
      }
      
      # Calculate penalty
      if( settings[[ "rt_penalty" ]] == "2sd" ) {
        penalty = correct_mean + 2 * correct_sd;
      } else {
        penalty = correct_mean +  settings[[ "rt_penalty" ]];
      }
      if( "penalty" %in% settings[[ "aux_report" ]] ) {
        result[[ paste( "penalty", comp_level, sep = "." ) ]] = penalty;
      }    
  
      # Apply penalty to each resp_penalty
      for( resp_penalty in settings[[ "resp_penalty" ]] ) {
        if( resp_penalty == "NA" ) {
          resp_to_penalize = is.na( ds_subset[ ,resp_var ] );
        } else {
          resp_to_penalize = ds_subset[ ,resp_var ] == resp_penalty;
        }
        g_resp_to_penalize <<- resp_to_penalize;
        # print( "penalize before" );
        if( length( resp_to_penalize ) > 0 && !is.na( resp_to_penalize ) ) {
          ds_subset[
            ds_subset[ ,comp_var ] == comp_level &
            resp_to_penalize,
            rt_var
          ] = penalty;
        }
        # print( "penalize after" );
      }    
      
      # Calculate mean adjusted for penalty
      # print( "adjusted before" );
      adjusted_means[[ comp_level ]] = mean(
        ds_subset[
          ds_subset[ ,comp_var ] == comp_level,
          rt_var
        ], na.rm = T    
      );
      # print( "adjusted after" );
      if( "adjusted_mean" %in% settings[[ "aux_report" ]] ) {
        result[[ paste( "adjusted_mean", comp_level, sep = "." ) ]] = adjusted_means[[ comp_level ]];
      }        
    }
    
    # ***********
    # *** Calculate inclusive SD and dscore
    inclusive_sd = sd( ds_subset[ ,rt_var ], na.rm = T );
    if( "inclusive_sd" %in% settings[[ "aux_report" ]] ) {
      result[[ "inclusive_sd" ]] = inclusive_sd;
    }         
    dscore = (
      adjusted_means[[ settings[[ "comp_levels"]][2] ]] -
      adjusted_means[[ settings[[ "comp_levels"]][1] ]] 
    ) / inclusive_sd;
    result[[ "score" ]] = dscore;
  
    return( result );
  }
  
  # Calculates two d-scores; one on each split halve of ds_subset
  one_dscore_split = function( result, ds_subset, settings = list() ) {
    comp_var = settings[[ "comp_var" ]];
    ds_subset[ ,"split" ] = NA;
    for( comp_level in settings[[ "comp_levels" ]] ) {
      n_split = sum( ds_subset[ ,comp_var ] == comp_level );
      ds_subset[ ds_subset[ ,comp_var ] == comp_level, "split" ] = sample( 
        rep( 1:2, ceiling( n_split / 2 ) )
      )[ 1 : n_split ];
    }
    
    for( split_i in 1:2 ) {
      ds_subset_split = ds_subset[ ds_subset[ ,"split" ] == split_i, ];
      result_split = one_dscore( result, ds_subset_split, settings );
      result[[ paste( "score", split_i, sep = "_" ) ]] = result_split[[ "score" ]];
    }
    
    return( result );
  }
  
  # Calculates two d-scores; one on first halve, one on second halve of ds_subset
  dscore_first_second = function( result, ds_subset, settings = list() ) {
    # g_ds_subset <<- ds_subset;
    comp_var = settings[[ "comp_var" ]];
    ds_subset[ ,"split" ] = NA;
    for( comp_level in settings[[ "comp_levels" ]] ) {
      n_split = sum( ds_subset[ ,comp_var ] == comp_level );
      n_split_1 = ceiling( n_split / 2 );
      n_split_2 = n_split - n_split_1;
      ds_subset[ ds_subset[ ,comp_var ] == comp_level, "split" ] = c(
        rep( 1, n_split_1 ),
        rep( 2, n_split_2 )
      );
    }
    
    for( split_i in 1:2 ) {
      ds_subset_split = ds_subset[ ds_subset[ ,"split" ] == split_i, ];
      result_split = one_dscore( result, ds_subset_split, settings );
      result[[ paste( "score", split_i, sep = "_" ) ]] = result_split[[ "score" ]];
    }
    
    return( result );
  }
  
  # *****************************************
  # *** End of inner function definitions ***
  # *****************************************
  
  # Calculate dscores for multiple runs of a task as identified by "run_var"

  # Select only compatible and incompatible from ds
  ds = ds[
    ds[ ,settings[[ "comp_var" ]] ] == settings[[ "comp_levels" ]][1] |
    ds[ ,settings[[ "comp_var" ]] ] == settings[[ "comp_levels" ]][2],
  ];

  # splithalves < 1? calculate dscore over full data, 
  # else calculate splithalve correlation splithalves times
  if( splithalves < 1 ) {
    # *** No split_halve, calculate and return a single (set of) d-scores
    # What dscore function to use
    dscore_function = switch(
      what,
      # Calculate one score over full task data
      all = one_dscore,
      # Calculate two scores: one for first and one for second halve of task
      first_second = dscore_first_second
    );
    return( niceBy(
      ds,
      c( settings[[ "run_var" ]] ),
      dscore_function,
      settings = settings,
      verbose = verbose,
      ...
    ) );  
  } else {
    # *** Else, calculate splithalves split_halve times
    # Correlations to average
    cors = c();
    for( split_halve_i in 1 : splithalves ) {
      # Run split_halve
      split_result = niceBy(
        ds,
        c( settings[[ "run_var" ]] ),
        one_dscore_split,
        settings = settings,
        verbose = verbose,
        ...
      );
      # DEBUG
      g_split_result <<- split_result;
      # Calculate correlation, add to list
      split_cor = cor( 
        suppressWarnings( as.numeric( split_result[ ,"score_1" ] ) ), 
        suppressWarnings( as.numeric( split_result[ ,"score_2" ] ) ),
        use = "pairwise.complete.obs",
        method = splithalf_method
      );
      cors = c( cors, split_cor );
      if( verbose ) { 
        print( paste( 
          Sys.time(), ", dscores, split ", 
          split_halve_i, "/", splithalves, ", r = ", split_cor,
          sep = ""
        ) ); 
      }
    }
    # DEBUG
    # g_cors <<- cors;
    # Return mean of correlations
    return( mean( cors ) );
  }
}

applyAggregation = function( result, ds_subset, settings ) {
  # Calculate individualResult by applying settings[["aggregation_factor_function"]] to each set of RTs as
  # selected by unique combinations of settings[["aggregation_factors"]]
  individualResult = niceBy(
    ds_subset,
    settings[["aggregation_factors"]],
    function( result, ds_subset ) {
      result[["score"]] = settings[["aggregation_factor_function"]](
        ds_subset[,settings[["rt_var"]]]
      );
      return(result);
    }
  );
  
  # Mage wide and convert to numeric
  if("dummy_id" %in% names(individualResult)) {
    stop("Column named 'dummy_id' found in aggregation function output; this column name cannot be used")
  }
  individualResult[,"dummy_id"] = 1;
  individualResult[,"score"] = as.numeric(as.character(individualResult[,"score"]));
  wide = makeWide(
    individualResult, 
    c("dummy_id"), 
    settings[["aggregation_factors"]]
  );
  
  # Apply aggregation_run_function on scores
  result[["score"]] = settings[["aggregation_run_function"]](wide);
  
  # If aux_report features "factor_scores" report aggregation_factor_function output
  if("factor_scores" %in% settings[["aux_report"]]) {
    wide = data.frame.dropVar(wide,"dummy_id");
    for(i in names(wide)) {
      result[[i]] = wide[1,i];
    }
  }
  
  return(result)
}

oneScore = function( result, ds_subset, settings ) {
  resp_var = settings[[ "resp_var" ]];
  rt_var   = settings[[ "rt_var"   ]];
  
  # Drop and report responses and rt's
  ds_subset[ ,rt_var ] = suppressWarnings( as.numeric( ds_subset[ ,rt_var ] ) );
  temp = dropAndReport( result, ds_subset, settings );
  result = temp[["result"]];
  ds_subset = temp[["ds_subset"]];
  result = applyAggregation( result, ds_subset, settings );
  return(result);
}


# Calculates two d-scores; one on each split halve of ds_subset
oneScoreSplit = function( result, ds_subset, settings, verbose = F ) {
  ds_debug <<- ds_subset;
  ds_subset = niceBy(
    ds_subset,
    settings[["aggregation_factors"]],
    function( result, ds_subset ) {
      debug_ds<<-ds_subset;
      ds_subset[ ,"split" ] = sample( 
        rep( 1:2, ceiling( nrow(ds_subset) / 2 ) )
      )[ 1 : nrow(ds_subset) ];      
      return(ds_subset);
    },
    result_type = "data.frame_to_data.frame",
    verbose = verbose
  );

  for( split_i in 1:2 ) {
    ds_subset_split = ds_subset[ ds_subset[ ,"split" ] == split_i, ];
    result_split = oneScore( result, ds_subset_split, settings );
    result[[ paste( "score", split_i, sep = "_" ) ]] = result_split[[ "score" ]];
  }  
  return(result);
}


#' Calculates scores from trial data using the 'aggregation' approach
#'
#' @export
#' @param ds          (data.frame) Trial data.
#' @param settings    (list) D-score calculation settings.
#' @param splithalves (integer) If 0, return a single set of d-scores, if > 0, return randomized split-halve reliability averaged over splithalves iterations.
#' @param verbose     (logical) If TRUE, then print debug output.
#' @param ...         Remaining arguments are passed to an internal scoring function. Not used at the moment.
#' @return (data.frame) Calculated d-scores.
#'
#' @details 
#' The table below shows an overview of elements in settings and their meaning.
#' \tabular{lllll}{
#'   \strong{Name} \tab \strong{Type} \tab \strong{Required?} \tab  \strong{Example Value} \tab \strong{Description} \cr
#'   aggregation_factors \tab character \tab Yes \tab \code{} \tab Apply aggregation_factor_function to each subset of ds formed by unique combinations of values for these variables \cr
#'   aggregation_factor_function \tab character \tab Yes \tab \code{} \tab Function applied to calculate aggregated scores (e.g. 'median rt for correct responses') \cr
#'   aggregation_run_function \tab character \tab Yes \tab \code{} \tab Function applied to output of aggregation_factor_function (e.g. difference of medians between red and blue conditions) \cr
#' }
#' @seealso \code{\link[sandra]{dropAndReport}} for additional settings that can be used for 
#' dropping and reporting responses and response times before scores are calculated.
#' 
#' @family SANDRA
#' @examples
#' See: SANDRA/framework_demos/scripts/0.t.2 Calculate Scores.R
#' @export
calculateAggregation = function( ds, settings, splithalves = 0, splithalf_method = "pearson", verbose = F, ... ) {
  # splithalves < 1? calculate scores over full data, 
  # else calculate splithalve correlation splithalves times
  if( splithalves < 1 ) {
    return( niceBy(
      ds,
      c( settings[[ "run_var" ]] ),
      oneScore,
      verbose = verbose,    
      settings = settings,
      ...
    ) ); 
  } else {
    # Correlations to average
    cors = c();
    for( split_halve_i in 1 : splithalves ) {
      # Run split_halve
      split_result = niceBy(
        ds,
        c( settings[[ "run_var" ]] ),
        oneScoreSplit,
        verbose = verbose,
        settings = settings,       
        ...
      );
      # DEBUG
      g_split_result <<- split_result;
      # Calculate correlation, add to list
      split_cor = cor( 
        suppressWarnings( as.numeric( as.character( split_result[ ,"score_1" ] ) ) ), 
        suppressWarnings( as.numeric( as.character( split_result[ ,"score_2" ] ) ) ),
        use = "pairwise.complete.obs",
        method = splithalf_method
      );
      cors = c( cors, split_cor );
      if( verbose ) { 
        print( paste( 
          Sys.time(), ", dscores, split ", 
          split_halve_i, "/", splithalves, ", r = ", split_cor,
          sep = ""
        ) ); 
      }
    }
    # DEBUG
    g_cors <<- cors;
    # Return mean of correlations
    return( mean( cors ) );
  }    
}


#' Calculates scores from trial data
#'
#' @export
#' @param ds          (data.frame) Trial data.
#' @param settings    (list) D-score calculation settings.
#' @param what        (character) If "dscore", calculate d-scores, else, use the 'aggregation' approach (like for difference of medians etc.)
#' @param splithalves (integer) If 0, return a single set of d-scores, if > 0, return randomized split-halve reliability averaged over splithalves iterations.
#' @param splithalf_method (character) Type of correlation calculated for splithalf reliability. Valid values: "pearson", "kendall", or "spearman".
#' @param verbose     (logical) If TRUE, then print debug output.
#' @param ...         Remaining arguments are passed to an internal scoring function. Not used at the moment.
#' @return (data.frame) Calculated d-scores.
#'
#' @details 
#' The table below shows an overview of elements in settings and their meaning.
#' \tabular{lllll}{
#'   \strong{Name} \tab \strong{Type} \tab \strong{Required?} \tab  \strong{Example Value} \tab \strong{Description} \cr
#'   run_var \tab character \tab Yes \tab \code{"set_id"} \tab Variable in trialdata that identifies a participation. \cr
#'   resp_var \tab character \tab Yes \tab \code{"response"} \tab Variable in trialdata that identifies responses (being correct, incorrect, etc.). \cr
#'   rt_var \tab character \tab Yes \tab \code{"rt"} \tab Variable in trialdata that identifies response times. \cr
#' }
#' @seealso \code{\link[sandra]{dropAndReport}} for additional settings that can be used for 
#' dropping and reporting responses and response times before scores are calculated.
#' @seealso \code{\link[sandra]{calculateDScores}} for additional settings that can be used for 
#' d-score calculations
#' @seealso \code{\link[sandra]{calculateAggregation}} for additional settings that can be used for 
#' the 'aggregation' approach to scoring
#' 
#' @family SANDRA
#' @examples
#' See: SANDRA/framework_demos/scripts/0.t.2 Calculate Scores.R
#' @export
calculateScores = function( type, ds, settings, splithalves = 0, splithalf_method = "pearson", verbose = F, ... ) {
  if( type == "dscore" ) { 
    return( calculateDScores(
      ds, settings, splithalves, splithalf_method, verbose, ...
    ) );
  } else {
    return( calculateAggregation(
      ds, settings, splithalves, splithalf_method, verbose, ...
    ) );
  }
}
  
