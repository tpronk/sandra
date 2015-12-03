# *** Build and install the sandra package
#install.packages( "devtools" );
#library( "devtools" )
setwd( "D:/wamp/www/mirte/sandra/package")
build()
install();

# *** Load it and check if some properties work
library( "sandra" );
io = sandra$io(
  "pathOriginal",
  "pathInterim"
);
io$readData