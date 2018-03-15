# Copyright (c) 2015 Thomas Pronk <pronkthomas@gmail.com>
# All rights reserved. No warranty, explicit or implicit, provided.

#' Decodes JASMIN1 data stored in a LOTUS results file into trialdata and metadata
#'
#' @export
#' @param input              (data.frame) LOTUS results file 
#' @param participationID    (vector) Columns in input for which each unique combination of values defines one participation
#' @param verbose            (logical) If TRUE, then print debug output
#' @param timeReportInterval (integer) If 0, don't print time remaining. Else, during set conversion, print an estimate of time remaining every timeReportInterval seconds
#' @param set_id_from        (integer) Value of set_id to start processing task data with
#' @param colRunID           (character) Name of LOTUS RunID column
#' @param colName            (character) Name of LOTUS Name column
#' @param colValue           (character) Name of LOTUS Value column
#' @return  (list) Decoded trialdata and metadata
#' @family SANDRA
#' @examples
#' See: SANDRA/framework_demos/scripts/t.1.a Decode JASMIN1 in LOTUS.R
#' See: SANDRA/tutorials/2. Decoding Trial Data, Scoring Tasks, and Widening.docx
decodeJasmin1 = function(
  input, 
  participationID = c( "UserID" ),
  verbose = FALSE,
  timeReportInterval = 60,
  set_id_from = 1,  
  colRunID = "RunID",
  colName = "Name",
  colValue = "Value"
) {
  # Implements ADPT report function
  report = function( reportMe ) {
    if( verbose ) {
      print( paste( reportMe, sep = "", collapse = "" ) );
    }
  }
  
  # Implements ADPT conc function
  conc = function( ... ) {
    return( paste( ..., sep = "", collapse = "" ) );
  }
  
  
  # *********
  # *** ADPT combine_segments.R & segement_combiner.R functions  
  copy_out_to_in = function( outside, inside, copy_seq = F ) {
    if( !is.null( outside ) && !is.null( inside ) ) {
      # Copy all columns from outside to inside (except seq numbers if copy_seq = F)
      candidates = names( outside ) != "from_seq" & names( outside ) != "to_seq";
      if( !copy_seq )
      {
        out_to_in = names( outside )[ candidates ];
      } else {
        out_to_in = names( outside )
        # print( out_to_in );
      }
      
      inside[ ,names( outside )[ candidates ] ] = NA;
      
      # Runs convert function providing indexes for outside and inside
      all_inside = c(
        as.numeric( na.omit( inside[ ,"from_seq" ] ) ),
        as.numeric( na.omit( inside[ ,"to_seq" ] ) )
      );
      
      if( length( all_inside ) == 0 )
      {
        highest_seq_inside = 1;
      } else {
        highest_seq_inside = max( all_inside );
      }
      
      org_inside_to = inside[ ,"to_seq" ];
      no_to  = is.na( inside[ ,"to_seq" ] );
      inside[ no_to, "to_seq" ] = highest_seq_inside + 1;
      
      # Replace missing for outside
      highest_seq_overall = max( c(
        highest_seq_inside,
        as.numeric( na.omit( outside[ ,"from_seq" ] ) ),
        as.numeric( na.omit( outside[ ,"to_seq" ] ) )
      ) );
      org_outside_to = outside[ ,"to_seq" ];
      no_to = is.na( outside[ ,"to_seq" ] );
      outside[ no_to, "to_seq" ] = highest_seq_overall + 1;
      
      # Run conversion
      for( i in 1:nrow( outside ) )
      {
        outside_from = outside[ i, "from_seq" ];
        outside_to   = outside[ i,   "to_seq" ];
        
        # No outside to? Set to last row of inside if last row of outside, else report warning
        if( is.na( outside_to ) )
        {
          outside_to = max( na.omit( inside[ ,"to_seq" ] ) );
          report( conc( "Warning: NA found for outside_to. At row: ", i ) );
        }
        
        # Get all rows of inside
        matches = 
          as.numeric( inside[ ,"from_seq" ] ) >= as.numeric( outside_from ) &
          as.numeric( inside[ ,"to_seq" ] )   <= as.numeric( outside_to );
        
        # Indexes of inside rows
        matches = which( matches );
        
        # Run conversion
        if( length( matches ) > 0 )
        {
          inside[ matches, out_to_in ] = outside[ i, out_to_in ];
        }
      }
      
      # Restore NA's
      inside[  ,"to_seq" ] = org_inside_to;
      outside[ ,"to_seq" ] = org_outside_to;

      return( inside );
    } else {
      # No (outside AND inside), determine what to return for next segmentation step
      if( !is.null( inside ) )
      {
        return( inside );
      }
      
      if( !is.null( outside ) )
      {
        return( outside );
      }		
      
      return( NULL );    
    }
  }

  # *********
  # *** ADPT Segmenter.R functions
  
  # Find an event with source evSource, type evType, search in logs
  # return index of first matching row at or beyond startPos
  find_event = function( logs, start_pos, evSource = NULL, evType = NULL, evName = NULL )
  {
    matches = rep( T, nrow( logs ) );
    
    # Filter on source, type, and name
    if( !is.null( evSource ) )
    {
      matches = matches &  logs[ ,"source" ] == evSource;
    }
    if( !is.null( evType ) )
    {
      matches = matches &  logs[ ,"type" ] == evType;
    }	
    if( !is.null( evName ) )
    {
      matches = matches &  logs[ ,"name" ] == evName;
    }	
    
    match_events = which( matches );
    if( length( match_events ) == 0 )
    {
      return( NULL );
    }
    match_pos    = match_events[ match_events >= start_pos ];
    if( length( match_pos ) == 0 )
    {
      return( NULL );
    }	
    
    return( min( match_pos ) );
  }
  
  find_next_event = function( events, logs, start_index )
  {
    indexes = c();
    for( event in events )
    {
      index = find_event( 
        logs, 
        start_index, 
        evSource = event[[ "source" ]], 
        evType   = event[[ "type"   ]],
        evName   = event[[ "name"   ]]
      );
      indexes = c( indexes, index );
    }
    if( is.null( indexes ) )
    {
      return( NULL );
    }
    return( min( indexes ) );
  }
  
  # Segment data into events
  run_mop = function( events, logs, output, first_pos, last_pos )
  {
    start_index = first_pos;
    while( !is.null( start_index ) && start_index <= last_pos )
    {
      # Find next event
      start_index = find_next_event( 
        events[[ "start" ]],
        logs, 
        start_index
      );
      
      if( !is.null( start_index )  )
      {
        stop_index = find_next_event( 
          events[[ "stop" ]],
          logs, 
          start_index + 1
        );			
        
        # Only process if seq numbers are different
        if( !is.null( stop_index ) && logs[ start_index, "sequence_number" ] != logs[ stop_index, "sequence_number" ] )
        {
          output = events[[ "callback" ]](
            logs,
            start_index,
            stop_index, 
            output,
            events[[ "name" ]]
          );
        }
        
        if( is.null( stop_index )  )
        {
          start_index = NULL;
        } else {
          start_index = stop_index;
        }
      }
    }
    
    return( output );
  }
  
  # *********
  # *** ADPT process_jasmin/parse_data_<level>.R functions
  
  parse_data_evlogs = function( evlogs ) {
    # Requires: evlogs
    report("*** parse_data_evlogs started" );
    
    # sequence_report: short list of strange things in sequence numbers
    sequence_report = c();
    
    # Setup column types
    numeric_columns = c( "time", "sequence_number" );
    for( i in numeric_columns )
    {
      evlogs[ ,i ] = as.numeric( evlogs[ ,i ] )
    }
    
    # Sort sequence
    seq_sorted = sort( evlogs[ ,"sequence_number" ] );
    
    # Check for duplicates or missing via lagged difference
    seq_diff = diff( seq_sorted );
    
    # Any difference == 0? Report duplicates
    if( sum( seq_diff == 0 ) > 0 )
    {
      duplicates = seq_sorted[ which( seq_diff == 0 ) ]
      report( conc( "parse_data_evlogs. Duplicate sequence_numbers for: ", paste( unique( duplicates ), collapse=", " ) ) );
      # Check if the duplicates are consistent (have the same value)
      inconsistents = c();
      for( i in 1 : length( duplicates ) )
      {
        inconsistent_found = F;
        
        duplicate_evlogs = evlogs[ evlogs[ ,"sequence_number" ] == duplicates[i], ]
        # Check row 1 with 2, 2 with 3 etc.
        for( j in 1 : ( nrow( duplicate_evlogs ) - 1 ) )
        {
          # If any column differs, they are inconsistent
          if( sum( duplicate_evlogs[ 1, ] == duplicate_evlogs[ 2, ] ) < ncol( duplicate_evlogs ) )
          {
            inconsistent_found = T;
          }
        }
        
        if( inconsistent_found )
        {
          inconsistents = c( inconsistents, duplicates[i] );
        }
      }
      
      # Log if any are inconsistent
      if( length( inconsistents ) > 0 )
      {
        report( conc( "parse_data_evlogs. Inconsistent data for sequence_numbers: ", paste( unique( inconsistents ), collapse=", " ) ) );
        sequence_report = c( sequence_report, "inconsistent" );
      }
    }
    
    # Any difference > 1? Report missing
    if( sum( seq_diff > 1 ) > 0 )
    {
      missing = seq_sorted[ which( seq_diff > 1 ) ]
      missing = paste( unique( missing ), collapse=", " );
      report( conc( "parse_data_evlogs. Missing sequence_numbers after: ", missing ) );
      sequence_report = c( sequence_report, "missing" );	
    }
    
    # Sort sequence_numbers by time and compute relative time
    evlogs = evlogs[ order( evlogs[ ,"sequence_number" ] ), ];
    evlogs[ ,"rel_time" ] = evlogs[ ,"time" ] - evlogs[ 1,"time" ];
    
    # Check on negative time differences
    if( sum( evlogs[ ,"rel_time" ] < 0 ) > 0 )
    {
      neg_time = seq_sorted[ which( evlogs[ ,"rel_time" ] < 0 ) ];
      neg_time = paste( unique( neg_time ), collapse=", " );
      report( conc( "parse_data_evlogs. Negative time difference at sequence_numbers: ", neg_time ) );
      sequence_report = c( sequence_report, "negtime" );
    }
    
    sequence_report = paste( sequence_report, collapse = " " );

    report( "*** parse_data_evlogs done" );
    return( list( 
      sequence_report = sequence_report,
      evlogs = evlogs
      
    ) );
  }
  
  parse_data_block = function( evlogs ) {
    output = NULL;
    
    report( conc( "*** parse_data_block started on ", Sys.time() ) );
    
    # *************
    # *** Output columns
    
    # Setup default output
    columns = c( "from_seq", "to_seq" );
    
    # Add custom columns based on trial_init and trial_response
    
    # Config via block_start
    task_init = find_event( evlogs, 1, evSource = "TaskManager", evType = "block_start");
    json      = evlogs[ task_init, "value" ];
    
    extra_columns = tryCatch( 
      { names( fromJSON( json ) ) },	 
      error = function( e ) {}
    );
  
    if( is.null( extra_columns ) )
    {
      extra_columns = "block_value";
    }
    
    columns    = c( columns, extra_columns );
    
    # Setup output
    output = data.frame( matrix( nrow = 0, ncol = length( columns ) ) );
    names( output ) = columns;
    
    # Setup event timer callback
    events= list();
    events[[ "callback" ]] = function( logs, from_index, to_index, output, name )
    {
      cur_row = nrow( output ) + 1;
      output[ cur_row, ] = NA;
      from = logs[ from_index, ];
      output[ cur_row, "from_seq"  ] = from[ "sequence_number" ];
      
      # Decode block vars
      json = as.character( from[ "value" ] );
      val = tryCatch( {
        fromJSON( json )
      }, error = function( e ) {
      } );
      
      if( !is.null( names( val ) ) )
      {
        for( i in names( val ) )
        {
          output[ cur_row, i ] = val[[ i ]];
        }
      } else {
        output[ cur_row, "block_value" ] = val;
      }
      
      if( !is.null( to_index ) )
      {
        to = logs[ to_index, ];
        output[ cur_row, "to_seq"  ] = to[ "sequence_number" ];
      }
      
      return( output );
    }
    
    
    # Triggered on trial events
    events[[ "name"  ]] = "trial";
    
    events[[ "start" ]] = list();
    events[[ "start" ]][[1]] = list();
    events[[ "start" ]][[1]][[ "source" ]] = "TaskManager";
    events[[ "start" ]][[1]][[ "type"   ]] = "block_start";
    
    events[[ "stop" ]] = list();
    events[[ "stop" ]][[1]] = list();
    events[[ "stop" ]][[1]][[ "source" ]] = "TaskManager";
    events[[ "stop" ]][[1]][[ "type"   ]] = "block_next";
    output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
    
    # Order output by sequence_number
    output = output[ order( output[ ,"from_seq" ] ), ];
    
    # Add set_id
    output[ 1 : nrow( output ),"set_id" ] = set_id;
    
    report( conc( "*** parse_data_block done" ) );
    return( output );
  }
  
  parse_data_event = function( evlogs ) {
    output = NULL;
    
    report( conc( "*** parse_data_event started on ", Sys.time() ) );
    
    # Setup output
    event_columns = c( 
      "event", 
      "from_seq", "from_name", "from_timelog", 
      "from_timeevt", "from_timeout",
      "to_seq", "to_name", "to_timelog", 
      "to_timeevt", "to_reason", "to_responses",
      "to_response_type", "to_response_key", "to_response_rt", "to_response_label"
    );
    output = data.frame( matrix( nrow = 0, ncol = length( event_columns ) ) );
    names( output ) = event_columns;
    
    # Setup event timer callback
    events= list();
    events[[ "callback"     ]] = function( logs, from_index, to_index, output, name )
    {
      cur_row = nrow( output ) + 1;
      
      output[ cur_row, ] = NA;
      output[ cur_row, "event" ] = name;
      
      # Decode from
      from = logs[ from_index, ];
      output[ cur_row, "from_seq"     ] = from[ "sequence_number" ];	
      output[ cur_row, "from_name"    ] = from[ "name" ];	
      output[ cur_row, "from_timelog" ] = from[ "rel_time" ];
      
      # Try to decode JSON in from
      json = from[ "value" ];
      val = tryCatch( fromJSON( as.character( json ) ), error=function( e ){ return( NULL ); } )
      if( !is.null( val ) )
      {
        # If "event_start", then decode eventManager data
        if( from[ "type" ] == "event_start" )
        {
          output[ cur_row, "from_timeevt" ] = val[[ "start"   ]];
          output[ cur_row, "from_timeout" ] = val[[ "timeout" ]];
        }
      }
      
      # Decode to				
      if( !is.null( to_index ) )
      {	
        to   = logs[ to_index, ];
        output[ cur_row, "to_seq"     ] = to[ "sequence_number" ];	
        output[ cur_row, "to_name"    ] = to[ "name" ];	
        output[ cur_row, "to_timelog" ] = to[ "rel_time" ];
        
        # Check if event names match
        if( from[ "name" ] != to[ "name" ] )
        {
          report( conc( 
            "From and to names do not match at sequence_number ",
            from[ "sequence_number" ],
            " and ",
            to[ "sequence_number" ]
          ) );
        }
        
        
        # Try to decode JSON in to
        json = to[ "value" ];
        val = tryCatch( fromJSON( as.character( json ) ), error=function( e ){ return( NULL ); } )
        if( !is.null( val ) )
        {
          # If "event_start", then decode eventManager data		
          if( to[ "type" ] == "event_stop" )
          {
            output[ cur_row, "to_timeevt" ] = val[[ "stop"    ]];
            output[ cur_row, "to_reason"  ] = val[[ "reason"  ]];
            
            # Response data
            output[ cur_row, "to_responses" ] = length( val[[ "responses" ]] );
            response = val[[ "response" ]];
            if( !is.null( response ) )
            {
              # On a NULL label set to "NA"
              if( is.null( response[[ "label" ]] ) )
              {
                response[[ "label" ]] = "NA";
              }
              
              output[ cur_row, "to_response_type"  ] = response[[ "type"  ]];
              output[ cur_row, "to_response_key"   ] = response[[ "key"   ]];
              output[ cur_row, "to_response_rt"    ] = response[[ "rt"    ]];
              output[ cur_row, "to_response_label" ] = response[[ "label" ]];
            }
          }
        }
      }
      return( output );
    }
    
    
    # init_task: time from task config to initial blur event_
    events[[ "name"  ]] = "init_task";
    
    events[[ "start" ]] = list();
    events[[ "start" ]][[1]] = list();
    events[[ "start" ]][[1]][[ "source" ]] = "TaskManager";
    events[[ "start" ]][[1]][[ "type"   ]] = "config";
    
    events[[ "stop" ]] = list();
    events[[ "stop" ]][[1]] = list();
    events[[ "stop" ]][[1]][[ "source" ]] = "FocusManager";
    events[[ "stop" ]][[1]][[ "type"   ]] = "blurred";
    
    output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
    
    # blurred: how long dit each blur last? 
    events[[ "name"  ]] = "blurred";
    
    events[[ "start" ]] = list();
    events[[ "start" ]][[1]] = list();
    events[[ "start" ]][[1]][[ "source" ]] = "FocusManager";
    events[[ "start" ]][[1]][[ "type"   ]] = "blurred";
    
    events[[ "stop" ]] = list();
    events[[ "stop" ]][[1]] = list();
    events[[ "stop" ]][[1]][[ "source" ]] = "Dialog";
    events[[ "stop" ]][[1]][[ "type"   ]] = "clicked";
    
    output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
    
    # event: All Events from EventManager
    events[[ "name"  ]] = "event";
    
    events[[ "start" ]] = list();
    events[[ "start" ]][[1]] = list();
    events[[ "start" ]][[1]][[ "source" ]] = "EventManager";
    events[[ "start" ]][[1]][[ "type"   ]] = "event_start";
    
    events[[ "stop" ]] = list();
    events[[ "stop" ]][[1]] = list();
    events[[ "stop" ]][[1]][[ "source" ]] = "EventManager";
    events[[ "stop" ]][[1]][[ "type"   ]] = "event_stop";
    
    output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
    
    
    # Add set_id
    output[ ,"set_id" ] = set_id;
    
    report( conc( "*** parse_data_event done" ) );
    return( output );
  }
  
  parse_data_task = function( evlogs ) {
    output = NULL;
    
    report( conc( "*** parse_data_task started on ", Sys.time() ) );
    
    # *************
    # *** Output columns
    
    # Setup default output
    columns = c( "from_seq", "to_seq" );
    
    # Add custom columns based on trial_init and trial_response
    
    # Config via task_init
    task_init = find_event( evlogs, 1, evSource = "TaskManager", evType = "task_init");
    json      = evlogs[ task_init, "value" ];
    val = tryCatch( fromJSON( as.character( json ) ), error=function( e ){ return( NULL ); } )
    if( is.null( val ) )
    {
      report( conc( "No JSON data in task_init; conversion halted" ) );
    } else {
      columns    = c( columns, names( val ) );
      
      # Setup output
      output = data.frame( matrix( nrow = 0, ncol = length( columns ) ) );
      names( output ) = columns;
      
      # Setup event timer callback
      events= list();
      events[[ "callback" ]] = function( logs, from_index, to_index, output, name )
      {
        cur_row = nrow( output ) + 1;
        output[ cur_row, ] = NA;
        from = logs[ from_index, ];
        output[ cur_row, "from_seq"  ] = from[ "sequence_number" ];
        
        # Decode trial vars
        json = as.character( from[ "value" ] );
        val  = fromJSON( json );
        for( i in names( val ) )
        {
          output[ cur_row, i ] = val[[ i ]];
        }
        
        return( output );
      }
      
      
      # Triggered on trial events
      events[[ "name"  ]] = "trial";
      
      events[[ "start" ]] = list();
      events[[ "start" ]][[1]] = list();
      events[[ "start" ]][[1]][[ "source" ]] = "TaskManager";
      events[[ "start" ]][[1]][[ "type"   ]] = "task_init";
      
      events[[ "stop" ]] = list();
      events[[ "stop" ]][[1]] = list();
      events[[ "stop" ]][[1]][[ "source" ]] = "TaskManager";
      events[[ "stop" ]][[1]][[ "type"   ]] = "task_done";
      output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
      
      # Order output by sequence_number
      output = output[ order( output[ ,"from_seq" ] ), ];
      
      # Add set_id
      output[ 1 : nrow( output ),"set_id" ] = set_id;
      
    }
    
    report( conc( "*** parse_data_task done" ) );
    return( output );
  }
  
  parse_data_trialdone = function( evlogs ) {
    output = NULL;
    
    report( conc( "*** parse_data_trialdone started on ", Sys.time() ) );
    
    # *************
    # *** Output columns
    
    # Setup default output
    columns = c( 
      "from_seq", "to_seq", "event",
      "block", "trial"
    );
    
    # Initially, assume we'll continue processing
    continue = T;
    
    # Add custom columns based on trial_init and trial_response
    
    # Config via trial_init
    trial_init = find_event( evlogs, 1, evSource = "TaskManager", evType = "trial_init");
    if( is.null( trial_init ) )
    {
      report( conc( "parse_data_trialdone. trial_init not found" ) );
      continue = F;
    } else {
      json       = evlogs[ trial_init, "value" ];
      val        = fromJSON( json );
      columns    = c( columns, names( val ) );
    }
    
    # Config via trial_done
    trial_done = find_event( evlogs, 1, evSource = "Task", evType = "trial_done");
    if( is.null( trial_done ) )
    {
      report( conc( "parse_data_trialdone. trial_done not found" ) );
      continue = F;
    } else {
      json       = evlogs[ trial_done, "value" ];
      val        = tryCatch( fromJSON( as.character( json ) ), error=function( e ){ return( NULL ); } )
      if( !is.null( val ) )
      {
        columns    = c( columns, names( val ) );
      }
    }	
    
    if( !continue ) {
      return( NULL );
    } else {
      output = data.frame( matrix( nrow = 0, ncol = length( columns ) ) );
      names( output ) = columns;
      
      # Setup event timer callback
      events= list();
      events[[ "callback" ]] = function( logs, from_index, to_index, output, name )
      {
        cur_row = nrow( output ) + 1;
        output[ cur_row, ] = NA;
        output[ cur_row, "event"     ] = name;
        
        from = logs[ from_index, ];
        output[ cur_row, "from_seq"  ] = from[ "sequence_number" ];
        
        # Decode block and trial
        json = as.character( from[ "name" ] );
        val  = fromJSON( json );
        output[ cur_row, "block" ] = val[1];
        output[ cur_row, "trial" ] = val[2];
        
        # Decode trial
        json = as.character( from[ "value" ] );
        val  = fromJSON( json );
        for( i in names( val ) )
        {
          output[ cur_row, i ] = val[[ i ]];
        }
        
        if( !is.null( to_index ) )
        {
          to = logs[ to_index, ];
          output[ cur_row, "to_seq"  ] = to[ "sequence_number" ];
          
          # Try to decode JSON in to
          json = to[ "value" ];
          val = tryCatch( fromJSON( as.character( json ) ), error=function( e ){ return( NULL ); } )
          if( !is.null( val ) )
          {
            for( i in names( val ) )
            {
              output[ cur_row, i ] = val[[ i ]];
            }					
          }
        }
        
        return( output );
      }
    }
    
    # Triggered on trial events
    events[[ "name"  ]] = "trial";
    
    events[[ "start" ]] = list();
    events[[ "start" ]][[1]] = list();
    events[[ "start" ]][[1]][[ "source" ]] = "TaskManager";
    events[[ "start" ]][[1]][[ "type"   ]] = "trial_init";
    
    events[[ "stop" ]] = list();
    events[[ "stop" ]][[1]] = list();
    events[[ "stop" ]][[1]][[ "source" ]] = "TaskManager";
    events[[ "stop" ]][[1]][[ "type"   ]] = "trial_init";
    events[[ "stop" ]][[2]] = list();
    events[[ "stop" ]][[2]][[ "source" ]] = "Task";
    events[[ "stop" ]][[2]][[ "type"   ]] = "trial_done";
    
    output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
    
    
    # Add set_id
    output[ ,"set_id" ] = set_id;
    
    report( "*** parse_data_trialdone done" );
    return( output );
  }
  
  parse_data_trialnext = function( evlogs ) {
    output = NULL;
    
    report( conc( "*** parse_data_trialnext started on ", Sys.time() ) );
    
    # *************
    # *** Output columns
    
    # Setup default output
    columns = c( 
      "from_seq", "to_seq", "event",
      "block", "trial"
    );
    
    # Add custom columns based on trial_init and trial_response
    
    # Config via trial_init
    trial_init = find_event( evlogs, 1, evSource = "TaskManager", evType = "trial_init");
    
    # If there is any trial_init event
    if( is.null( trial_init ) )
    {
      report( conc( "parse_data_trialnext. trial_init not found" ) );
    } else {
      json       = evlogs[ trial_init, "value" ];
      val        = fromJSON( json );
      columns    = c( columns, names( val ) );
      
      output = data.frame( matrix( nrow = 0, ncol = length( columns ) ) );
      names( output ) = columns;
      
      # Setup event timer callback
      events= list();
      events[[ "callback" ]] = function( logs, from_index, to_index, output, name )
      {
        cur_row = nrow( output ) + 1;
        output[ cur_row, ] = NA;
        output[ cur_row, "event"     ] = name;
        
        from = logs[ from_index, ];
        output[ cur_row, "from_seq"  ] = from[ "sequence_number" ];
        
        # Decode block and trial
        json = as.character( from[ "name" ] );
        val  = fromJSON( json );
        output[ cur_row, "block" ] = val[1];
        output[ cur_row, "trial" ] = val[2];
        
        # Decode trial
        json = as.character( from[ "value" ] );
        val  = fromJSON( json );
        for( i in names( val ) )
        {
          output[ cur_row, i ] = val[[ i ]];
        }
        
        if( !is.null( to_index ) )
        {
          to = logs[ to_index, ];
          output[ cur_row, "to_seq"  ] = to[ "sequence_number" ];
          
          # Try to decode JSON in to
          json = to[ "value" ];
          val = tryCatch( fromJSON( as.character( json ) ), error=function( e ){ return( NULL ); } )
          if( !is.null( val ) )
          {
            for( i in names( val ) )
            {
              output[ cur_row, i ] = val[[ i ]];
            }					
          }
        }
        
        return( output );
      }
      
      
      # Triggered on trial events
      events[[ "name"  ]] = "trial";
      
      events[[ "start" ]] = list();
      events[[ "start" ]][[1]] = list();
      events[[ "start" ]][[1]][[ "source" ]] = "TaskManager";
      events[[ "start" ]][[1]][[ "type"   ]] = "trial_init";
      
      events[[ "stop" ]] = list();
      events[[ "stop" ]][[1]] = list();
      events[[ "stop" ]][[1]][[ "source" ]] = "TaskManager";
      events[[ "stop" ]][[1]][[ "type"   ]] = "trial_init";
      events[[ "stop" ]][[2]] = list();
      events[[ "stop" ]][[2]][[ "source" ]] = "TaskManager";
      events[[ "stop" ]][[2]][[ "type"   ]] = "trial_next";
      
      output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
      
      
      # Add set_id
      output[ ,"set_id" ] = set_id;
    }
    
    report( "*** parse_data_trialnext done" );
    return( output );
  }
  
  parse_data_trialresp = function( evlogs ) {
    output = NULL;
    
    report( conc( "*** parse_data_trialresp started on ", Sys.time() ) );
    
    # *************
    # *** Output columns
    
    # Setup default output
    columns = c( 
      "from_seq", "to_seq", "event",
      "block", "trial"
    );
    
    # Add custom columns based on trial_init and trial_response
    
    # Continue if trial_init and trial_response are there
    continue = T;
    
    # Config via trial_init
    trial_init = find_event( evlogs, 1, evSource = "TaskManager", evType = "trial_init");
    if( is.null( trial_init ) )
    {
      report( conc( "parse_data_trialnext. trial_init not found" ) );
      continue = F;
    } else {
      json       = evlogs[ trial_init, "value" ];
      val        = fromJSON( json );
      columns    = c( columns, names( val ) );
    }
    
    # Response via trial_response
    trial_response = find_event( evlogs, 1, evSource = "TaskManager", evType = "trial_response" );
    if( is.null( trial_response ) )
    {
      report( conc( "parse_data_trialnext. trial_response not found" ) );
      continue = F;
    } else {
      json       = evlogs[ trial_response, "value" ];
      val        = fromJSON( json );
      columns    = c( columns, names( val ) );
    }	
    
    if( continue )
    {
      output = data.frame( matrix( nrow = 0, ncol = length( columns ) ) );
      names( output ) = columns;
      
      # Setup event timer callback
      events= list();
      events[[ "callback" ]] = function( logs, from_index, to_index, output, name )
      {
        cur_row = nrow( output ) + 1;
        output[ cur_row, ] = NA;
        output[ cur_row, "event"     ] = name;
        
        from = logs[ from_index, ];
        output[ cur_row, "from_seq"  ] = from[ "sequence_number" ];
        
        # Decode block and trial
        json = as.character( from[ "name" ] );
        val  = fromJSON( json );
        output[ cur_row, "block" ] = val[1];
        output[ cur_row, "trial" ] = val[2];
        
        # report( conc( "parse_data_trialnext. block ", val[1], ", trial ", val[2] ) );
        
        # Decode trial
        json = as.character( from[ "value" ] );
        val  = fromJSON( json );
        for( i in names( val ) )
        {
          output[ cur_row, i ] = val[[ i ]];
        }
        
        if( !is.null( to_index ) )
        {
          to = logs[ to_index, ];
          output[ cur_row, "to_seq"  ] = to[ "sequence_number" ];
          
          
          # Decode response
          json = as.character( to[ "value" ] );
          val  = fromJSON( json );
          for( i in names( val ) )
          {
            if( is.null( val[[ i ]] ) )
            {
              val[[ i ]] = "NA";
            }
            output[ cur_row, i ] = val[[ i ]];
          }
        }
        
        return( output );
      }
      
      
      # Triggered on trial events
      events[[ "name"  ]] = "trial";
      
      events[[ "start" ]] = list();
      events[[ "start" ]][[1]] = list();
      events[[ "start" ]][[1]][[ "source" ]] = "TaskManager";
      events[[ "start" ]][[1]][[ "type"   ]] = "trial_init";
      
      events[[ "stop" ]] = list();
      events[[ "stop" ]][[1]] = list();
      events[[ "stop" ]][[1]][[ "source" ]] = "TaskManager";
      events[[ "stop" ]][[1]][[ "type"   ]] = "trial_init";
      events[[ "stop" ]][[2]] = list();
      events[[ "stop" ]][[2]][[ "source" ]] = "TaskManager";
      events[[ "stop" ]][[2]][[ "type"   ]] = "trial_response";
      
      output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
      
      # Add set_id
      output[ ,"set_id" ] = set_id;
    }
    
    report( conc( "*** parse_data_trialresp done" ) );
    return( output );
  }
  
  parse_data_trialrespmulti = function( evlogs ) {
    output = NULL;
    
    report( conc( "*** parse_data_trialrespmulti started on ", Sys.time() ) );
    
    # *************
    # *** Output columns
    
    # Setup default output
    columns = c( 
      "from_seq", "to_seq", "event", "from_type", "to_type",
      "block", "trial"
    );
    
    # Add custom columns based on trial_init and trial_response
    
    # Continue if trial_init and trial_response are there
    continue = T;
    
    # Config via trial_init
    trial_init = find_event( evlogs, 1, evSource = "TaskManager", evType = "trial_init");
    if( is.null( trial_init ) )
    {
      report( conc( "parse_data_trialnext. trial_init not found" ) );
      continue = F;
    } else {
      json       = evlogs[ trial_init, "value" ];
      val        = fromJSON( json );
      columns    = c( columns, names( val ) );
    }
    
    # Response via trial_response
    trial_response = find_event( evlogs, 1, evSource = "TaskManager", evType = "trial_response" );
    if( is.null( trial_response ) )
    {
      report( conc( "parse_data_trialrespmulti. trial_response not found" ) );
      continue = F;
    } else {
      json       = evlogs[ trial_response, "value" ];
      val        = fromJSON( json );
      # always add response and rt columns
      val[[ "rt" ]] = 0;
      val[[ "response" ]] = 0;
      columns    = c( columns, names( val ) );
    }	
    
    if( continue )
    {
      # output
      output = data.frame( matrix( nrow = 0, ncol = length( columns ) ) );
      names( output ) = columns;
      
      # temporary storage
      temprow = data.frame( matrix( nrow = 1, ncol = length( columns ) ) );
      names( temprow ) = columns;
      
      # Setup event timer callback
      events= list();
      events[[ "callback" ]] = function( logs, from_index, to_index, output, name )
      {
        temprow[ 1, ] = NA;
        temprow[ 1, "event"     ] = name;
        
        from = logs[ from_index, ];
        temprow[ 1, "from_type" ] = from[ "type" ];
        temprow[ 1, "from_seq"  ] = from[ "sequence_number" ];
        
        # Decode block and trial
        json = as.character( from[ "name" ] );
        val  = fromJSON( json );
        temprow[ 1, "block" ] = val[1];
        temprow[ 1, "trial" ] = val[2];
        
        
        # Decode trial
        json = as.character( from[ "value" ] );
        val  = fromJSON( json );
        for( i in names( val ) )
        {
          temprow[ 1, i ] = val[[ i ]];
        }
        
        if( !is.null( to_index ) )
        {
          to = logs[ to_index, ];
          temprow[ 1, "to_type" ] = to[ "type" ];
          temprow[ 1, "to_seq"  ] = to[ "sequence_number" ];
          
          # Decode response
          json = as.character( to[ "value" ] );
          val  = fromJSON( json );
          for( i in names( val ) )
          {
            temprow[ 1, i ] = val[[ i ]];
          }
        }
        
        # Decide if we should store this row; only if to_type is a trial_response 
        if( temprow[ 1, "to_type" ] == "trial_response" )
        {
          cur_row = nrow( output ) + 1;	
          output[ cur_row, ] = temprow;
        }			
        return( output );
      }
      
      
      # Triggered on trial events
      events[[ "name"  ]] = "trial";
      
      events[[ "start" ]] = list();
      events[[ "start" ]][[1]] = list();
      events[[ "start" ]][[1]][[ "source" ]] = "TaskManager";
      events[[ "start" ]][[1]][[ "type"   ]] = "trial_init";
      events[[ "start" ]][[2]] = list();
      events[[ "start" ]][[2]][[ "source" ]] = "TaskManager";
      events[[ "start" ]][[2]][[ "type"   ]] = "trial_response";	
      
      events[[ "stop" ]] = list();
      events[[ "stop" ]][[1]] = list();
      events[[ "stop" ]][[1]][[ "source" ]] = "TaskManager";
      events[[ "stop" ]][[1]][[ "type"   ]] = "trial_init";
      events[[ "stop" ]][[2]] = list();
      events[[ "stop" ]][[2]][[ "source" ]] = "TaskManager";
      events[[ "stop" ]][[2]][[ "type"   ]] = "trial_response";
      
      output = run_mop( events, evlogs, output, 1, nrow( evlogs ) )
      
      # Add set_id
      output[ ,"set_id" ] = set_id;
    }
    
    report( conc( "*** parse_data_trialrespmulti done" ) );
    
    return( output );
  }

  # *****************************************
  # *** End of inner function definitions ***
  # *****************************************

  # # DEBUG; move locals to global
  # g_input <<- input
  # # DEBUG; set locals
  # input = g_input;
  # participationID = c( "UserID" );
  # verbose = FALSE;
  # timeReportInterval = 60;
  # set_id_from = 1;
  # colRunID = "RunID";
  # colName = "Name";
  # colValue = "Value"
  
  if( timeReportInterval > 0 ) {
    print( "sandra::decodeJasmin1 started. For big datasets it can take a while before I start providing an estimate of time remaining." );
  }
  
  # Construct colUserID = "participationID" as combination of column values in participationID
  report( conc( "sandra::decodeJasmin1. Construct participationID" ) );    
  colUserID = "participationID";
  input[ ,"participationID" ] = apply(
    input,
    1,
    function( row ) {
      return(
        paste( row[ participationID ], collapse = "_" )
      );
    }
  ); 
  
  # Check if input was properly formatted
  if( length( names( input ) ) == 1 ) {
    print( "Jasmin1Decoder$decode. Warning: I found one variable name at the first row. Are you sure the is tab (\\t) separated?")
  }
  
  # cols_lotus are all columns except RunID, Name, and Value
  cols_lotus = names( input );
  for( i in c( colName, colValue ) )
  {
    col_drop = which( cols_lotus == i );
    if( length( col_drop ) > 0 )
    {
      cols_lotus = cols_lotus[ -col_drop ];
    }
  }
  
  # Custom data encoded in task_start event
  i = which( input[ ,colName ] == "task_start" )[1];
  value = tryCatch ( 
    fromJSON( input[ i, colValue ] ), 
    error = function( e ){ 
      return( NULL );
    }
  );
  if (is.null(value)) {
    print(paste("decodeJasmin1.R 1184. Invalid JSON found at row.", i));
    print(input[ i, colValue ]);
  }
  
  #value = fromJSON( input[ i, colValue ] );
  cols_custom = names( value );
  
  # default columns in metadata
  cols_metadata = c( cols_lotus, cols_custom, "run_from", "run_to", "set_id", "lotus_says", "event_count", "sequence_report" );
  metadata = data.frame( matrix( nrow = 0, ncol = length( cols_metadata ) ) );
  names( metadata ) = cols_metadata;
  
  # Sort by colUserID, then by colRunID 
  input = input[ with( input, order( input[ ,colUserID ], input[, colRunID ] ) ), ];
  
  # Indexes of task_start and task_done
  indexes_task_start = which( input[ ,colName ] == "task_start" )
  indexes_task_done  = which( input[ ,colName ] == "task_start" | input[ ,colName ] == "task_done" | input[ ,colName ] == "task_error" )
  
  # Build up sets
  report( conc( "sandra::decodeJasmin1. Construct sets" ) );    
  sets = list();
  # Find matching task_done for each task_start
  for( i in indexes_task_start )
  {
    # Assume a matching done
    match_found = T;
    
    later_start = indexes_task_start[ indexes_task_start > i ];
    later_done  = indexes_task_done[  indexes_task_done  > i ];
    
    # Is there a done?
    if( length( later_done ) == 0 )
    {
      next_done = nrow( input ) + 1;
    } else {
      # Yes, find next one
      next_done = min( later_done );
    }
    
    # If matching done, add to set
    if( match_found )
    {
      sets[[ length( sets ) + 1 ]] = c( i, next_done );
    }
  }
  report( conc( "sandra::decodeJasmin1. ", length( sets ), " sets found" ) );
  
  # *********
  # *** Convert sets and construct trialdata
  trialdata = list();
  timer = ProgressTimer( length( sets ), stepStart = set_id_from );
  for( set_id in set_id_from : length( sets ) ) {
    timer$reportProgress( set_id );

    sequence_report = c();
    nwarnings = length( warnings() );
    
    report( conc( "sandra::decodeJasmin1. Processing set ", set_id, " of ", length( sets ) ) );
    metadata[ set_id, "run_from"   ] = input[ sets[[set_id]][1], colRunID ];
    metadata[ set_id, "run_to"     ] = input[ sets[[set_id]][2], colRunID ];
    metadata[ set_id, "set_id"     ] = set_id;
    metadata[ set_id, cols_lotus   ] = input[ sets[[set_id]][1], cols_lotus ];
    metadata[ set_id, "lotus_says" ] = input[ sets[[set_id]][2], colName ];

    # custom data
    value = tryCatch(
      fromJSON( input[ sets[[set_id]][1], colValue ] ),
      error = function(cond) {
        stop( paste(
          "sandra::decodeJasmin1 failed at row",
          sets[[set_id]][1],
          "error message was",
          cond,
          ", data was ",
          input[ sets[[set_id]][1], colValue ]
        ) );
      }
    );
    
    task_id = NULL;
    for( j in names( value ) )
    {
      custom_value = value[[ j ]];
      if( is.null( custom_value ) )
      {
        custom_value = "NULL";
      }
      metadata[ set_id, j ] = custom_value;
      if( j == "taskName" )
      {
        task_id = custom_value;
      }
    }
    
    # No taskName found? try to guess it from "config"
    if (is.null(task_id) && "config" %in% names(value)) {
      taskCandidates = c("aat", "vpt");
      for (taskCandidate in taskCandidates) {
        if (length(grep(taskCandidate, value[["config"]], fixed = TRUE)) > 0) {
          task_id = taskCandidate;
        }
      }
    }
    
    # Still no task_id? report warning
    if (is.null(task_id)) {
      stop("No task_id found");
    }
    
    report( "sandra::decodeJasmin1. Preparing evlogs" );
    
    # Prepare evlogs
    cols_evlogs = c( "source", "type", "name", "value", "time", "sequence_number" );
    evlogs      = data.frame( matrix( nrow = 0, ncol = length( cols_evlogs ) ) );
    names( evlogs ) = cols_evlogs;
    
    for( j in ( sets[[set_id]][1] + 1 ) : ( sets[[set_id]][2] - 1 ) )
    {
      # Fill evlogs
      if( input[ j, colName ] == "jasmin" )
      {
        values = fromJSON( input[ j, colValue ] );
        cur_row = nrow( evlogs ) + 1;
        evlogs[ cur_row, "source" ]          = values[[ "source" ]];
        evlogs[ cur_row, "type" ]            = values[[ "type"   ]];
        evlogs[ cur_row, "time" ]            = values[[ "time"   ]];
        evlogs[ cur_row, "sequence_number" ] = values[[ "sequence_number" ]];
        evlogs[ cur_row, "value"           ] = toJSON( values[[ "value" ]] );
        
        if( is.character( values[[ "name"   ]] ) )
        {
          evlogs[ cur_row, "name" ] = values[[ "name" ]];
        } else {
          evlogs[ cur_row, "name" ] = toJSON( values[[ "name" ]] );
        }
      }
    }
    
    metadata[ set_id, "event_count" ] = nrow( evlogs );
    if (metadata[ set_id, "event_count" ] >= 2) {
      metadata[ set_id, "duration"] = evlogs[nrow(evlogs), "time"] - evlogs[1, "time"];
    } else {
      metadata[ set_id, "duration"] = NA;
    }
    
    report( conc( "sandra::decodeJasmin1. event_count: ", metadata[ set_id, "event_count" ] ) );
    report( conc( "sandra::decodeJasmin1. lotus_says:  ", metadata[ set_id, "lotus_says"  ] ) );
    
    # Run conversions (if task_done)
    metadata[ set_id, "sequence_report" ] = "";
    
    if( !is.na( metadata[ set_id, "lotus_says" ] ) && metadata[ set_id, "lotus_says" ] == "task_done" )
    {
      result = parse_data_evlogs( evlogs );
      evlogs = result$evlogs;
      sequence_report = c( sequence_report, result$sequence_report );
      
      data_task        = parse_data_task( evlogs );
      data_block       = parse_data_block( evlogs );
      data_trialnext   = parse_data_trialnext( evlogs );
      if( task_id == "aat" ) {
        data_trialdone = parse_data_trialdone( evlogs );
      } else {
        data_trialdone = NULL;
      }
      data_trialresp   = parse_data_trialresp( evlogs );
      # data_event       = parse_data_event( evlogs );
      
      # *** Copy outside vars to inside
      data_outside = data_task
      data_outside = copy_out_to_in( 
        data_outside, 
        data_block
      );
      data_outside = copy_out_to_in( 
        data_outside, 
        data_trialnext
      );
      data_outside = copy_out_to_in( 
        data_outside, 
        data_trialdone
      );
      data_outside = copy_out_to_in( 
        data_outside, 
        data_trialresp
      );
      # data_outside = copy_out_to_in( 
      #   data_outside, 
      #   data_event,
      # );

      # If only one trial filled with NA, invalid data
      if( nrow( data_outside ) == 1 && all( is.na( data_outside ) ) ) {
        sequence_report = c( sequence_report, "invalid" );
      } else {
        # add data_outside to trialdata
        if( !( task_id %in% names( trialdata ) ) ) {
          trialdata[[ task_id ]] = data_outside;
        } else {
          # Check whether data_outside and trialdata contain same columns, else report warning
          if( !( 
               length( names( trialdata[[ task_id ]] ) ) == length( names( data_outside ) ) 
            && all( names( trialdata[[ task_id ]] )      == names( data_outside ) )
          ) ) {
            print( conc(
              "sandra::decodeJasmin1. Warning: different columns in trialdata of ",
              task_id,
              " collected so far and trialdata in current set" 
            ) );
            print( conc(
              "metadata: ",
              paste( metadata[ set_id, ], collapse = ", " )
            ) );
            print( conc(
              "existing trialdata columns: ",
              paste( names( trialdata[[ task_id ]] ), collapse = ", " )
            ) );
            print( conc(
              "current trialdata columns: ",
              paste( names( data_outside ), collapse = ", " )
            ) );          
          }
          
          trialdata[[ task_id ]][
              ( nrow( trialdata[[ task_id ]] ) + 1 ) 
            : ( nrow( trialdata[[ task_id ]] ) + nrow( data_outside ) ), 
          ] = data_outside;
        }      
      }
    }
    
    if( length( warnings() ) > nwarnings ) {
      print( paste( "sandra::decodeJasmin1. There were warnings at set_id ", set_id ) );
    }
    
    if( length( sequence_report ) > 0 ) {
      metadata[ set_id, "sequence_report" ] = paste( sequence_report, collapse = "," );
    } else {
      metadata[ set_id, "sequence_report" ] = "";
    }
  }   
  for (task_id in names(trialdata)) {
    ds = trialdata[[task_id]];
    
    # Remove duplicate trials
    ds = ds[
      order(
        ds[,"set_id"],
        ds[,"from_seq"]
      ),
      ];
    g_trial = -1;
    duplicate = apply(
      ds,
      1,
      function(dsr) {
        result = dsr["trial"] == g_trial;
        g_trial <<- dsr["trial"];
        return(result);
      }
    );
    ds = ds[!duplicate,];  
    trialdata[[task_id]] = ds;
  }
  timer$done();
  return( list( 
    metadata  = metadata,
    trialdata = trialdata 
  ) );  
}
