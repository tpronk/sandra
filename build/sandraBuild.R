# Config
pathSandraSource   = "D:/wamp/www/sandra/trunk/src";
pathSandraCompiled = "D:/wamp/www/sandra/trunk/dist";
# End of Config

# Generate man
library( "roxygen2" )
roxygen2::roxygenise( pathSandraSource )

# Install package
library( "devtools" )
# build( 
#   pkg  = pathSandraSource,
#   path = pathSandraCompiled
#   );
install( pathSandraSource );

# *** Load it and show version number
library( "sandra" );
packageVersion( "sandra" )

?calculateDScores