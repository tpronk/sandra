# *** Build package
library( "devtools" )
build(   "D:/wamp/www/sandra/trunk/package" );
install( "D:/wamp/www/sandra/trunk/package" );

# *** Load it and show version number
rm( sandra );
library( "sandra" );
packageVersion( "sandra" )
