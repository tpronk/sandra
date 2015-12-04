# Install & load package devtools
install.packages( "devtools" );
library( "devtools" )

# Install & load package SANDRA
install_github( "tpronk/SANDRA/package" );
library( "sandra" );

# Install SANDRA Analysis Framework
sandra$install();
