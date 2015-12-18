# Config
pathSandraSource   = "D:/wamp/www/sandra/trunk/src";
pathSandraCompiled = "D:/wamp/www/sandra/trunk/dist";
# End of Config

# Generate man pages
library( "roxygen2" )
roxygen2::roxygenise( pathSandraSource )

# Install package
library( "devtools" )
# build( 
#   pkg  = pathSandraSource,
#   path = pathSandraCompiled
# );
install( 
  pathSandraSource,
  local = FALSE,
  keep_source = TRUE
);

# Load it and report version
library( "sandra" );
packageVersion( "sandra" )
calculateDScores