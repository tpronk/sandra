# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Calculates d-scores from trial data
#'
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
#'   run_var \tab character \tab Yes \tab \code{"set_id"} \tab Variable in trialdata that identifies a participation. \cr
#'   resp_var \tab character \tab Yes \tab \code{"response"} \tab Variable in trialdata that identifies responses (being correct, incorrect, etc.). \cr
#'   rt_var \tab character \tab Yes \tab \code{"rt"} \tab Variable in trialdata that identifies response times. \cr
#'   comp_var \tab character \tab Yes \tab \code{"appr"} \tab Variable in trialdata that identifies compatible/incompatible blocks. \cr
#'   comp_levels \tab character \tab Yes \tab \code{c( "yes", "no" )} \tab Vector containing two elements, corresponding to the values of comp_var that identify compatible and incompatible blocks respectively. Bias score is calculated towards first element. \cr
#'   fast_drop \tab integer \tab No \tab \code{300} \tab Drop all response times lower than this value. \cr
#'   fast_report \tab integer \tab No \tab \code{300} \tab Report number of response times lower than this value in scores. \cr
#'   slow_drop \tab integer \tab No \tab \code{3000} \tab Drop all response times higher than this value. \cr
#'   slow_report \tab integer \tab No \tab \code{3000} \tab Report number of response times higher than this value in scores. \cr
#'   sd_drop \tab numeric \tab No \tab \code{1.5} \tab Drop all response times higher than mean response times of correct responses + sd_drop * SD, and those lower than mean - sd_drop * SD. \cr
#'   sd_report \tab numeric \tab No \tab \code{1.5} \tab Report number of response times higher than mean response times of correct responses + sd_drop * SD, or those lower than mean - sd_drop * SD, in scores. \cr
#'   resp_drop \tab vector \tab No \tab \code{c("NA",0,3,4)} \tab Drop all responses with these values. \cr
#'   resp_report \tab vector \tab No \tab \code{c(1)} \tab Report number of responses with these values (per value individually). \cr
#'   resp_correct \tab character \tab Yes \tab \code{1} \tab Value for correct responses. \cr
#'   resp_penalty \tab vector \tab No \tab \code{c(2)} \tab Responses whose response times are to be replaced by a penalty RT. \cr
#'   rt_penalty \tab character \tab No \tab \code{"2sd"} \tab Default = "2sd". Use "2sd" to replace RTs of incorrect responses with the mean + 2SD of correct responses for current block. Use "d600" to penalize with mean + 600. \cr
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
#' @family SANDRA
#' @examples
#' See: SANDRA/framework_demos/scripts/processCbm.jasmin1_data.R
calculateDScores = function( ds, settings, splithalves = 0, what = "all", verbose = F, ... ) {

  # ***********************
  # *** Inner Functions ***
  # ***********************
  
  # Calculate one dscore from a ds_subset of dataset (for one run of a task)
  one_dscore = function( result, ds_subset, settings = list() ) {
    g_ds_subset <<- ds_subset;
    
    # These settings are used often
    comp_var = settings[[ "comp_var" ]];
    resp_var = settings[[ "resp_var" ]];
    rt_var   = settings[[ "rt_var"   ]];
    ds_subset[ ,rt_var ] = suppressWarnings( as.numeric( ds_subset[ ,rt_var ] ) );
    
    # ***********
    # *** Reporting 
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
          sum( ds_subset[ ,resp_var ] == resp_report );
      }
    }  
    # Drop responses
    for( resp_drop in settings[[ "resp_drop" ]] ) {
      if( resp_drop == "NA" ) {
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
      result[[ paste( "fast", settings[[ "fast_report" ]], sep = "_" ) ]] =
        sum( ds_subset[ ,"rt" ] < settings[[ "fast_report" ]] );
    }
    # Drop fast
    if( !is.null( settings[[ "fast_drop" ]] ) ) {
      ds_subset = ds_subset[ 
          ds_subset[ ,"rt" ] >= settings[[ "fast_drop" ]] 
        | is.na( ds_subset[ ,"rt" ] ),
      ];
    }
      
    # Report slow
    if( !is.null( settings[[ "slow_report" ]] ) ) {
      result[[ paste( "slow", settings[[ "slow_report" ]], sep = "_" ) ]] =
        sum( ds_subset[ ,"rt" ] > settings[[ "slow_report" ]] );
    }
    # Drop slow
    if( !is.null( settings[[ "slow_drop" ]] ) ) {
      ds_subset = ds_subset[ 
        ds_subset[ ,"rt" ] <= 
            settings[[ "slow_drop" ]] 
          | is.na( ds_subset[ ,"rt" ] ),
        ];
    }  
    
    # Report SD
    if( !is.null( settings[[ "sd_report" ]] ) ) {
      sd_report = settings[[ "sd_report" ]];
      cur_mean = mean( ds_subset[ ,"rt" ] );
      cur_sd   = sd(   ds_subset[ ,"rt" ] );
      sd_fast  = cur_mean - sd_report * cur_sd;
      sd_slow  = cur_mean + sd_report * cur_sd;
      result[[ paste( "sd", settings[[ "sd_report" ]], sep = "_" ) ]] =
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
    
    # ***********
    # *** Apply penalty to RT and calculate means
    adjusted_means = list();
    for( comp_level in settings[[ "comp_levels" ]] ) {
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
      correct_sd   = sd(   ds_subset_current[ ,rt_var ], na.rm = T );
      if( "correct_sd" %in% settings[[ "aux_report" ]] ) {
        result[[ paste( "correct_sd", comp_level, sep = "." ) ]] = correct_sd;
      }
      
      # Calculate penalty
      if( settings[[ "rt_penalty" ]] == "d600" ) {
        penalty = correct_mean + 600;
      } else {
        penalty = correct_mean + 2 * correct_sd;
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
        ds_subset[
          ds_subset[ ,comp_var ] == comp_level &
          resp_to_penalize,
          rt_var
        ] = penalty;
      }    
      
      # Calculate mean adjusted for penalty
      adjusted_means[[ comp_level ]] = mean(
        ds_subset[
          ds_subset[ ,comp_var ] == comp_level,
          rt_var
        ], na.rm = T    
      );
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
    result[[ "dscore" ]] = dscore;
  
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
      result[[ paste( "dscore", split_i, sep = "_" ) ]] = result_split[[ "dscore" ]];
    }
    
    return( result );
  }
  
  # Calculates two d-scores; one on first halve, one on second halve of ds_subset
  dscore_first_second = function( result, ds_subset, settings = list() ) {
    comp_var = settings[[ "comp_var" ]];
    ds_subset[ ,"split" ] = NA;
    for( comp_level in settings[[ "comp_levels" ]] ) {
      n_split = sum( ds_subset[ ,comp_var ] == comp_level );
      ds_subset[ ds_subset[ ,comp_var ] == comp_level, "split" ] = c(
        rep( 1, ceiling( n_split / 2 ) ),
        rep( 2, ceiling( n_split / 2 ) )
      );
    }
    
    for( split_i in 1:2 ) {
      ds_subset_split = ds_subset[ ds_subset[ ,"split" ] == split_i, ];
      result_split = one_dscore( result, ds_subset_split, settings );
      result[[ paste( "dscore", split_i, sep = "_" ) ]] = result_split[[ "dscore" ]];
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
      # Calculate correlation, add to list
      split_cor = cor( 
        suppressWarnings( as.numeric( split_result[ ,"dscore_1"] ) ), 
        suppressWarnings( as.numeric( split_result[ ,"dscore_2" ] ) )
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
    # Return mean of correlations
    return( mean( cors ) );
  }
}
  
