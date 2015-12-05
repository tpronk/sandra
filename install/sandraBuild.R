# *** Build package
library( "devtools" )
pathSandra = "D:/wamp/www/sandra/trunk/src";
build( pathSandra );
install( pathSandra );

# *** Load it and show version number
rm( sandra );
library( "sandra" );
packageVersion( "sandra" )
