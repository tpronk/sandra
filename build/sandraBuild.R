# Config
pathSandraSource   = "D:/wamp/www/sandra/branches/tpronk_dev/src";
pathSandraCompiled = "D:/wamp/www/sandra/branches/tpronk_dev/dist";
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
packageVersion( "sandra" );
