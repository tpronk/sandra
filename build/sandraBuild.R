# Config
pathSandraSource   = "D:/wamp/www/sandra/trunk/src";
pathSandraCompiled = "D:/wamp/www/sandra/trunk/dist";
# End of Config

# Install package
library( "devtools" )
build( 
  pkg  = pathSandraSource,
  path = pathSandraCompiled
  );
install( pathSandraSource );

# Generate man
library( "sandra" );
library( "roxygen2" )
roxygen2::roxygenise( pathSandraSource )

packageVersion( "sandra" )
?calculateDScores