library( "devtools" )
build(   "D:/wamp/www/sandra/trunk/package" );
install( "D:/wamp/www/sandra/trunk/package" );

# *** Load it and check if some properties work
library( "sandra" );
packageVersion( "sandra" )


io = sandra$io(
  "pathOriginal",
  "pathInterim"
);
io$readData