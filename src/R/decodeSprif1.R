# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Decodes SPRIF data stored in a LOTUS results file into trialdata and metadata
#'
#' @export
#' @param ds             (data.frame) LOTUS results file 
#' @param sprifColumn    (character) Column in ds that contains SPRIF data
#' @param sprifVars      (vector) Variables logged for each trial. In NULL, the number of variables is guessed and named "X1", "X2", etc.
#' @param sprifColSep    (character) Character separating columns in SPRIF data
#' @param sprifRowSep    (character) Character separating rows in SPRIF data
#' @param verbose        (logical) If TRUE, then print debug output
#' @return (list) Decoded trialdata and metadata
#' @family SANDRA
#' @examples
#' See: SANDRA/framework_demos/scripts/t.1.b Decode SPRIF in LOTUS.R
#' See: SANDRA/tutorials/2. Decoding Trial Data, Scoring Tasks, and Widening.docx
decodeSprif1 = function(
  ds,
  sprifColumn = "Value",
  sprifVars = NULL,
  sprifColSep = "\t",
  sprifRowSep = "|",
  verbose = F
) {
  # Output: trialdata and metadata
  trialdata = NULL;
  colsLotus = names( ds[ names( ds ) != sprifColumn ] );
  metadata = data.frame.new( vars = c( colsLotus, "set_id", "sequence_report" ) );
  
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
  
  # Convert each row in answers
  for( i in 1 : nrow( ds ) ) {
    if( verbose ) { print( paste( 
      "Row ",
      i,
      " in results file",
      sep = ""
    ) ); }    
    
    sequence_report = c();
    
  	# Get answer row and eop data
  	curRow = ds[ i, ];
  	sprifData = curRow[ sprifColumn ];
  	
		# only continue if eop data is not empty and starts with sprifColSep
		if( nchar( sprifData ) > 1 && substr( sprifData, 1, 1 ) == sprifRowSep ) {
		  sprifVector = strsplit( as.character( sprifData ), sprifColSep )[[1]];

			# Check if sprifVars exists, else guess number of columns and construct sprifVars
			if( is.null( sprifVars ) ) {
				sprifColCount = which( sprifVector == sprifRowSep )[2] - 2;
				sprifVars = paste( "X", 1 : sprifColCount, sep = "" )
				if( verbose ) { print( paste( 
          "No sprifVars were specified. I guess that the number of sprifVars is ",
					sprifColCount,
					". sprifVars will be named X1, X2, etc.",
					sep = ""
  			) ); }
			}
      sprifRowCount = length( sprifVector ) / ( length( sprifVars ) + 1 );
		
			# If sprifRowCount is not a whole number, report in sequence_report
			if( sprifRowCount != floor( sprifRowCount  ) ) {
			  sequence_report = c( sequence_report, "incomplete")
			  if( verbose ) { print( paste( 
			    "incomplete. ",
					"Number of elements in SPRIF data: ",
					length( sprifVector ),
					". Number of elements in sprifVars: ",
					length( sprifVars ),
					sep = ""
  			) ); }
			}
      
      if( verbose ) { print( paste( 
				sprifRowCount,
				"rows in eop data" 
			) ); }

      if (sprifRowCount == 1 ) {
        print("Only 1 row; skipped");
      } else {
  			# Convert to matrix
  			sprifMatrix = matrix(
  				sprifVector,
  				byrow = TRUE,
  				ncol  = length( sprifVars ) + 1
  			);
  		
  			# If not only sprifRowSep in first column, report in sequence_report
  			if( sum( sprifMatrix[ ,1 ] == sprifRowSep ) < length( sprifMatrix[ ,1 ] ) ) {
  			  sequence_report = c( sequence_report, "norowsep");
  			  if( verbose ) {
    				oneErrorRow = which( sprifMatrix[ ,1 ] != sprifRowSep )[1];
    				print( paste( 
    					"norowsep. The row below shows row no. ",
    					oneErrorRow,
    					" of sprifData. The first element of this row was expected to be an sprifRowSep.\n",
    					paste( sprifMatrix[ oneErrorRow, ], collapse = sprifColSep ),
    					sep = ""
    				) );
  			  }
  			}
  		
  			# If any sprifRowSep columns outside of first column, report in sequence_report
  			if( sum( sprifMatrix[ ,2 : ncol( sprifMatrix ) ] == sprifRowSep ) > 0 ) {
  			  sequence_report = c( sequence_report, "rowsepinvar");
  			  if( verbose ) {
    				oneErrorRow = which( 
    					sprifMatrix[ ,2 : ncol( sprifMatrix ) ] == sprifRowSep,
    					arr.ind = TRUE
    				 )[ 1, "row" ];
    	
    				print( paste( 
    					"rowsepinvar. The row below shows row no. ",
    					oneErrorRow,
    					" of sprifData. This row contains sprifRowSeps in other columns than the first.\n",
    					paste(sprifMatrix[ oneErrorRow, ], collapse = sprifColSep ),
    					sep = ""
    				) );
  			  }
  			}
  			
  			# Construct data frame
  			sprifDataframe = data.frame( sprifMatrix[,-1] );
  		  names( sprifDataframe ) = sprifVars;
  			sprifDataframe[ ,"set_id" ] = i;
  			
  			# add to trialdata
  			if( is.null( trialdata ) ) {
  			  trialdata = sprifDataframe;
  			} else {
  			  trialdata = rbind( trialdata, sprifDataframe )
  			}
  		}
		} else {
			# Report invalid row
		  sequence_report = c( sequence_report, "invalid" );
		  if( verbose ) { print( paste( 
				"invalid. Row did not contain values or did not start with an sprifRowSep. ",
				"The following value was present in sprifVar: ",
				sprifData
			) ); }
		}

  	# Add to metadata 
  	metadata[ i, colsLotus ] = curRow[ colsLotus ];
  	metadata[ i, "set_id" ] = i;
  	if( length( sequence_report ) > 0 ) {
  	  metadata[ i, "sequence_report" ] = paste( sequence_report, collapse = "," );
  	} else {
  	  metadata[ i, "sequence_report" ] = "";
  	}  	
  }
  
  return( list(
    trialdata = trialdata,
    metadata = metadata
  ) );
}
  
  
  
