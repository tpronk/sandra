# *** Build package
library( "devtools" )
build(   "D:/wamp/www/sandra/trunk/src" );
install( "D:/wamp/www/sandra/trunk/src" );

# *** Load it and show version number
rm( sandra );
library( "sandra" );
packageVersion( "sandra" )
