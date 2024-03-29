% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProgressTimer.R
\name{ProgressTimer}
\alias{ProgressTimer}
\title{Constructs a SANDRA ProgressTimer}
\usage{
ProgressTimer(stepTotal, progressTitle = "Progress", stepStart = 1,
  reportingInterval = 10, width = 500, ...)
}
\arguments{
\item{stepTotal}{(integer) Total number of steps}

\item{progressTitle}{(character) Title of progress bar.}

\item{stepStart}{(integer) First step}

\item{reportingInterval}{(integer) Number of seconds between updating progress bar}

\item{width}{(integer) Width of progress bar in pixels}
}
\value{
(ProgressTimer)
}
\description{
Times how long a series of steps takes, estimates time remaining, and can display this
via a graphical progress bar
}
\seealso{
Other SANDRA: \code{\link{EasyFit}}, \code{\link{FileIO}},
  \code{\link{FrameworkFileIO}}, \code{\link{addPostfix}},
  \code{\link{appendData}},
  \code{\link{calculateAggregation}},
  \code{\link{calculateDScores}},
  \code{\link{calculateScores}},
  \code{\link{checkAndRemoveJasminDuplicates}},
  \code{\link{cutExtension}},
  \code{\link{data.frame.affixNames}},
  \code{\link{data.frame.dropVar}},
  \code{\link{data.frame.new}},
  \code{\link{data.frame.renameVar}},
  \code{\link{decodeJasmin1}},
  \code{\link{decodeJasmin2Legacy}},
  \code{\link{decodeJasmin2}}, \code{\link{decodeSprif1}},
  \code{\link{done}}, \code{\link{dropAndReport}},
  \code{\link{existsData}}, \code{\link{getExtension}},
  \code{\link{installAnalysisFramework}},
  \code{\link{leftMerge}}, \code{\link{makeWide}},
  \code{\link{niceBy}}, \code{\link{niceMerge}},
  \code{\link{path}}, \code{\link{readData}},
  \code{\link{readSurveyTool}}, \code{\link{readVector}},
  \code{\link{reportProgress}}, \code{\link{resolvePath}},
  \code{\link{runScript}}, \code{\link{timeRemaining}},
  \code{\link{updateChecksum}}, \code{\link{writeData}},
  \code{\link{writeVector}}

Other sandra::ProgressTimer: \code{\link{done}},
  \code{\link{reportProgress}}, \code{\link{timeRemaining}}
}

