# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Decodes SPRIF data stored in a LOTUS results file into trial data
#'
#' @export
#' @param ds              (data.frame) LOTUS results file 
#' @param participationID    (vector) Columns in ds for which each unique combination of values defines one participation
#' @param verbose            (logical) If TRUE, then print debug output
#' @param timeReportInterval (integer) If 0, don't print time remaining. Else, during set conversion, print an estimate of time remaining every timeReportInterval seconds
#' @param set_id_from        (integer) Value of set_id to start processing task data with
#' @param colRunID           (character) Name of LOTUS RunID column
#' @param colName            (character) Name of LOTUS Name column
#' @param colValue           (character) Name of LOTUS Value column
#' @return (data.frame) Calculated d-scores
#' @family SANDRA
#' @examples
#' See: SANDRA/framework_demos/scripts/t.1.a Decode JASMIN1.R
decodeSprif = function(
  ds,
  sprifColumn = "Value",
  filterColumn = NULL,
  filterValue = NULL,
  sprifColSep = "\t",
  sprifRowSep = "|",
  startsWithRowSep = T,
  varNamesOnFircurRow = F,
  sprifSkipRows = 0
) {
  # Escape sprifRowSep if it is a pipe
  if( sprifRowSep == "|" ) {
  	sprifRowSepEscaped = "\\|";
  } else {
  	sprifRowSepEscaped = sprifRowSep;
  }

  # Number of current converted row
  setCounter    = 1;
  # EOP output (as vector )
  outputVector  = c();
  
  # Only select rows that match filter (if filter is specified)
  if( is.null( filterColumn ) ) {
    rowsToConvert = 1 : nrow( ds );
  } else {
    rowsToConvert = which( ds[ ,filterColumn ] == filterValue );
  }
  
  # Convert each row in answers
  for( i in rowsToConvert )
  {
  	# Get answer row and eop data
  	curRow = ds[ i, ];
  	sprifData = curRow[ eopIndex ];
  	
  			# Should we check on startsWithRowSep?
  			valid = T;
  			if( startsWithRowSep )
  			{
  				valid = substr( sprifData, 1, 1 ) == sprifRowSep;
  			}
  		
  			# only continue if eop data is not empty and starts with sprifColSep
  			if( 
  				   nchar( sprifData ) >  1
  				&& valid
  			)
  			{
  				if( startsWithRowSep ) 
  				{
  					# Convert eop data to vector
  					eopVector   = strsplit( as.character( sprifData ), sprifColSep )[[1]];
  			
  					# Check if eopColumns exists, else guess number of columns and construct eopColumns
  					if( !exists( "eopColumns" ) )
  					{
  						eopColumnCount = which( eopVector == sprifRowSep )[2] - 2;
  						eopColumns = paste( "X", 1 : eopColumnCount, sep = "" )
  						report(	conc( 
  												"No eopColumns were specified. I guess that the number of eopColumns is ",
  							eopColumnCount,
  							". eopColumns will be named X1, X2, etc."
  						) );
  					}
  			
  					eopRowCount = length( eopVector ) / ( length( eopColumns ) + 1 );
  			
  					# Check if eopRowCount is whole number
  					if( eopRowCount != floor( eopRowCount  ) )
  					{
  						report(	conc( 
  							"Error 8 - Number of elements in eop data not whole multiple of the number of elements in (eopColumns + 1). ",
  							"Number of elements in eop data: ",
  							length( eopVector ),
  							". Number of elements in eopColumns: ",
  							length( eopColumns )
  						) );
  					}
  			
  					report(	conc( 
  						eopRowCount,
  						" rows in eop data" 
  					) );
  				
  					
  					# Convert to matrix
  					eopMatrix = matrix(
  						eopVector,
  						byrow = TRUE,
  						ncol  = length( eopColumns ) + 1
  					)
  			
  					# Check if only delimiters in first column
  					if( sum( eopMatrix[ ,1 ] == sprifRowSep ) < length( eopMatrix[ ,1 ] ) )
  					{
  						oneErrorRow = which( eopMatrix[ ,1 ] != sprifRowSep )[1];
  			
  						report(	conc( 
  							"Warning 9 - sprifRowSep expected. ",
  							"The row below shows row no. ",
  							oneErrorRow,
  							" of sprifData. The first element of this row was expected to be an sprifRowSep.\n",
  							paste( eopMatrix[ oneErrorRow, ], collapse = sprifColSep )
  						) );
  						
  					}
  			
  					# Check if no delimiters in value fields
  					if( sum( eopMatrix[ ,2 : ncol( eopMatrix ) ] == sprifRowSep ) > 0 )
  					{
  						oneErrorRow = which( 
  							eopMatrix[ ,2 : ncol( eopMatrix ) ] == sprifRowSep,
  							arr.ind = TRUE
  						 )[ 1, "row" ];
  			
  						report(	conc( 
  							"Warning 10 - sprifRowSep found in value field. ",
  							"The row below shows row no. ",
  							oneErrorRow,
  							" of sprifData. This row contains sprifRowSeps in other fields than the first.\n",
  							paste(eopMatrix[ oneErrorRow, ], collapse = sprifColSep )
  						) );
  						
  					}
  				} else {
  					# Not startsWithRowSep
  					eopRows = strsplit( as.character( sprifData ), sprifRowSep )[[1]];
  					eopColumnCount = length( strsplit( as.character( eopRows ), sprifColSep )[[1]] );
  					
  					# Check if eopColumns exists, else guess number of columns and construct eopColumns
  					if( !exists( "eopColumns" ) )
  					{
  						# Use first row as var names
  						if( varNamesOnFircurRow )
  						{
  							report( conc( "No eopColumns were specified. Using first row to determine variable names. ",
  								eopColumnCount,
  								" variables found."
  							) );
  
  							eopColumns = strsplit( as.character( eopRows[1]), sprifColSep )[[1]];
  						} else {
  							eopColumnCount = length( strsplit( as.character( eopRows[1]), sprifColSep )[[1]] );
  							eopColumns = paste( "X", 1 : eopColumnCount, sep = "" )
  							report(	conc( 
  													"No eopColumns were specified. I guess that the number of eopColumns is ",
  								eopColumnCount,
  								". eopColumns will be named X1, X2, etc."
  							) );
  						}
  					}
  					
  					# Convert to matrix
  					eopMatrix = matrix(
  						NA,
  						ncol  = eopColumnCount,
  						nrow  = length( eopRows )
  					);
  					for( k in 1 : length( eopRows ) )
  					{
  						eopMatrix[ k, ] = strsplit( as.character( eopRows[k]), sprifColSep )[[1]]
  					}
  				}
  					
  				# Create converted matrix, with setCounter and prefix repeated each row
  				convertedMatrix = matrix(
  					NA,
  					ncol = 
  						length( outputPrefix ) +	
  						length( eopColumns ), 
  					nrow = nrow( eopMatrix )
  				);					
  				
  		
  				# Fill columns 1 to n with outputPrefix
  				convertedMatrix[ ,( 1 : length( outputPrefix ) ) ] = 
  					rep( 
  						outputPrefix, 
  						rep( 
  							nrow( convertedMatrix ),  
  							length( outputPrefix )
  						)
  					);
  		
  				# Fill columns n+1 and beyond with eopMatrix (without the rowDelimiter)
  				startCol = 1;
  				if( startsWithRowSep )
  				{
  					startCol = 2;
  				}
  					
  				convertedMatrix[ ,( length( outputPrefix ) + 1 ) : ncol( convertedMatrix ) ] = 
  					eopMatrix[ ,startCol : ncol( eopMatrix ) ];
  		
  				# Skip rows (if any)
  				dataLeft = T;
  				if( sprifSkipRows > 0 )
  				{
  					report( conc(
  						"Skipping first ",
  						sprifSkipRows,
  						" rows of sprifData.\n"
  					) );
  
  					if( sprifSkipRows >= nrow( convertedMatrix ) )
  					{
  						dataLeft = F;
  						report( "No data left to store.\n" );						
  					} else {
  						convertedMatrix = convertedMatrix[ ( sprifSkipRows + 1 ) : nrow( convertedMatrix ), ];
  					}
  				}
  				
  				# Add values to outputVector
  				if( dataLeft )
  				{
  					outputVector = c(
  						outputVector,
  						t( convertedMatrix )
  					);
  				}
  						
  				setCounter = setCounter + 1;
  			} else {
  				# Report empty row
  				report(	conc( 
  					"No EOP data; row did not contain values or did not start with an sprifRowSep. ",
  					"The following value was present in the eop column: ",
  					sprifData
  				) );
  			}
  		}
  	}
  }
  
  
  # ********************
  # *** Finish up
  
  # *** Set up output
  
  report( "*** Setting up output" );
  
  # Setup variable names
  variableNames = c(
  	"setCounter"
  );
  
  if( exists( "stEncodedNames" ) )
  {
  	variableNames = c(
  		variableNames,
  		stEncodedNames
  	)
  }
  
  variableNames = c(
  	variableNames,
  	eopColumns
  );
  
  # Convert outputvector to matrix
  converted = matrix(
  	c(
  		outputVector
  	),
  	ncol  = ncol( convertedMatrix ),
  	byrow = TRUE
  );
  
  # Store in output
  output = data.frame( converted );
  names( output ) = variableNames;
  
  # *** All done!
  report( "Done!" );
  
  
  
