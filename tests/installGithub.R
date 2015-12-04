# *** Install the sandra package
install.packages( "devtools" );
library( "devtools" )
install_github( "tpronk/SANDRA/package" );

# *** Load it and check if some properties work
library( "sandra" );
io = sandra$io(
  "pathOriginal",
  "pathInterim"
);
io$readData