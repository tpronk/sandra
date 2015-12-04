# *********************
# *** SANDRA Jasmin1Decoder

# Decodes JASMIN1 data stored in a LOTUS results file into trial data

# Create sandra namespace if not exists
if( !exists( "sandra" ) ) { 
  sandra = list();
}

sandra$Jasmin1Decoder = list(
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
  },
  
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
  },
  
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
  },

  decode = function( 
    input, 
    participationID = c( "UserID" ),
    colRunID = "RunID",
    colName   = "Name",
    colValue  = "Value",
    interim_folder = "jasmin1_interim",
    output_folder  = "jasmin1_output",
    source_file    = "jasmin1_data.csv"
  ) {
    # Construct colUserID = "ParticipationID" as combination of colsParticipation
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
    
    # Check if source_file was properly formatted
    if( length( names( input ) ) == 1 ) {
      print( "sandra$Jasmin1Decoder$decode. Warning: I found one variable name at the first row. Are you sure the is tab (\\t) separated?")
    }
    
    # cols_lotus are all columns except RunID, Name, and Value
    cols_lotus = names( input );
    for( i in c( colRunID, colName, colValue ) )
    {
      col_drop = which( cols_lotus == i );
      if( length( col_drop ) > 0 )
      {
        cols_lotus = cols_lotus[ -col_drop ];
      }
    }
    
    # Custom data encoded in task_start event
    i = which( input[ ,colName ] == "task_start" )[1];
    value = fromJSON( input[ i, colValue ] );
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
    
    
    # Setup interim and output directory
    dir_interim = io$path( io$pathInterim, interim_folder );
    dir.create( dir_interim, showWarning = FALSE );
    filename = io$path( io$pathInterim, source_file );
    
    dir_output = io$path( io$pathInterim, output_folder );
    dir.create( dir_output, showWarning = FALSE );
    
    print( paste( "process_lotus. ", length( sets ), " sets found" ) );
    
    # *********
    # *** Convert sets
    for( set_id in 1:length( sets ) )
    {
      nwarnings = length( warnings() );
      
      print( paste( "process_lotus. Processing set ", set_id, " of ", length( sets ) ) );
      metadata[ set_id, "run_from"   ] = input[ sets[[set_id]][1], colRunID ];
      metadata[ set_id, "run_to"     ] = input[ sets[[set_id]][2], colRunID ];
      metadata[ set_id, "set_id"     ] = set_id;
      metadata[ set_id, cols_lotus   ] = input[ sets[[set_id]][1], cols_lotus ];
      metadata[ set_id, "lotus_says" ] = input[ sets[[set_id]][2], colName ];
      
      # custom data
      value = fromJSON( input[ sets[[set_id]][1], colValue ] );
      
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
      
      print( paste( "process_lotus. Preparing evlogs" ) );
      
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
      
      print( paste( "process_lotus. event_count: ", metadata[ set_id, "event_count" ] ) );
      print( paste( "process_lotus. lotus_says:  ", metadata[ set_id, "lotus_says"  ] ) );
      
      # Run conversions (if task_done)
      metadata[ set_id, "sequence_report" ] = "";
      if( !is.na( metadata[ set_id, "lotus_says" ] ) && metadata[ set_id, "lotus_says" ] == "task_done" )
      {
        run_module( "process_jasmin/parse_data_evlogs.R" );
        metadata[ set_id, "sequence_report" ] = sequence_report;
        run_module( "process_jasmin/parse_data_task.R" );
        run_module( "process_jasmin/parse_data_block.R" );
        run_module( "process_jasmin/parse_data_trialnext.R" );
        if( task_id == "aat" )
        {
          run_module( "process_jasmin/parse_data_trialdone.R" );
        }
        run_module( "process_jasmin/parse_data_trialresp.R" );
        #run_module( "process_jasmin/parse_data_trialrespmulti.R" );
        run_module( "process_jasmin/parse_data_event.R" );
        
        run_module( "process_jasmin/combine_segments.R" );
      }
      
      if( length( warnings() ) > nwarnings )
      {
        print( paste( "process_lotus. There were warnings at set_id ", set_id ) );
      }
    }    
  }
);
