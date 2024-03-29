% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FileIO.R
\name{writeVector}
\alias{writeVector}
\title{Write a vector to a newline separated file}
\usage{
writeVector(this, filename, output)
}
\arguments{
\item{this}{(sandra::FileIO) FileIO instance}

\item{filename}{(character) File to write to}

\item{output}{(vector) Data to write to file}
}
\description{
Write a vector to a newline separated file
}
\examples{
# Create FileIO instance with folders in root of D:
io = FileIO( "D:/scripts", "D:/original", "D:/interim" );
io$writeVector( "data.csv", v );
}
\seealso{
Other SANDRA: \code{\link{EasyFit}}, \code{\link{FileIO}},
  \code{\link{FrameworkFileIO}},
  \code{\link{ProgressTimer}}, \code{\link{addPostfix}},
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
  \code{\link{updateChecksum}}, \code{\link{writeData}}

Other sandra::file input & output: \code{\link{FileIO}},
  \code{\link{FrameworkFileIO}}, \code{\link{addPostfix}},
  \code{\link{appendData}}, \code{\link{cutExtension}},
  \code{\link{existsData}}, \code{\link{getExtension}},
  \code{\link{path}}, \code{\link{readData}},
  \code{\link{readSurveyTool}}, \code{\link{readVector}},
  \code{\link{resolvePath}}, \code{\link{runScript}},
  \code{\link{updateChecksum}}, \code{\link{writeData}}

Other sandra::FileIO: \code{\link{FileIO}},
  \code{\link{FrameworkFileIO}}, \code{\link{appendData}},
  \code{\link{existsData}}, \code{\link{readData}},
  \code{\link{readSurveyTool}}, \code{\link{readVector}},
  \code{\link{resolvePath}}, \code{\link{runScript}},
  \code{\link{updateChecksum}}, \code{\link{writeData}}
}

