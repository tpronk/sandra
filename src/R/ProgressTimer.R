#' Constructs a SANDRA ProgressTimer
#' 
#' Times how long a series of steps takes, estimates time remaining, and can display this
#' via a graphical progress bar
#'
#' @param stepTotal         (integer) Total number of steps
#' @param progressTitle     (character) Title of progress bar. If NULL, no progress bar is displayed
#' @param stepStart         (integer) First step
#' @param reportingInterval (integer) Number of seconds between updating progress bar
#' @param ...               Remaining arguments passed on to tcltk::tkProgressBar
#' @return (ProgressTimer)
#' @family sandra::ProgressTimer
#' @family SANDRA
ProgressTimer = function( stepTotal, progressTitle = "Progress", stepStart = 1, reportingInterval = 10, ... ) {
  this = new.env();
  
  this$stepTotal = stepTotal;
  this$progressTitle = progressTitle;  
  this$stepStart = stepStart;
  this$reportingInterval = reportingInterval;
  
  if( !is.null( this$progressTitle ) ) {
    this$progressBar = tkProgressBar(
      title = "Estimating time remaining...",
      label = this$progressTitle,
      min   = this$stepStart, 
      max   = this$stepTotal,
      ...
    );
  }
  
  this$timeStart = as.numeric( proc.time()[ "elapsed" ] );
  this$timeLast = this$timeStart;
  this$reportProgress = function( ... ) { reportProgress( this, ... ); };
  this$timeRemaining = function( ... ) { timeRemaining( this, ... ); };
  this$timeSpent = function( ... ) { timeSpent( this, ... ); };
  this$close = function( ... ) { close( this, ... ); };
  return( this );
}

#' Report progress
#' 
#' If progress bar is displayed, and it is time to update it, then do so
#'
#' @param stepCurrent       (integer) Number of current step
#' @return NULL
#' @family sandra::ProgressTimer
#' @family SANDRA
reportProgress = function( this, stepCurrent ) {
  if( !is.null( this$progressTitle ) ) {
    timeCurrent = as.numeric( proc.time()[ "elapsed" ] );
    if( timeCurrent - this$timeLast > this$reportingInterval ) {
      left  = as.character( round( seconds_to_period( this$timeRemaining( stepCurrent ) ) ) );
      spent = as.character( round( seconds_to_period( this$timeSpent() ) ) );
      setTkProgressBar(
        this$progressBar,
        value = stepCurrent,
        title = paste(
          left,
          spent,
          sep = ", "
        ),
        label = paste(
          this$progressTitle,
          ". ",
          "Remaining: ",
          left,
          ". Spent: ",
          spent,
          sep = ""
        )
      );
      this$timeLast = timeCurrent;
    }
  }
}

#' Estimate number of seconds remaining
#' 
#' @param stepCurrent       (integer) Number of current step
#' @return (numeric)
#' @family sandra::ProgressTimer
#' @family SANDRA
timeRemaining = function( this, stepCurrent ) {
  timeCurrent = as.numeric( proc.time()[ "elapsed" ] );
  timePerSet = ( timeCurrent - this$timeStart ) / ( stepCurrent - this$stepStart );
  timeRemaining = ( this$stepTotal - stepCurrent ) * timePerSet;
  return( timeRemaining );
}

#' Report time spent (since start of ProgressTimer)
#' 
#' @return (numeric)
#' @family sandra::ProgressBar 
timeSpent = function( this ) {
  return( as.numeric( proc.time()[ "elapsed" ] ) - this$timeStart );
}

#' Close progress bar
#' 
#' @return NULL
#' @family sandra::ProgressTimer
#' @family SANDRA
close = function() {
  close( this$progressBar );
}