# Install & load package devtools
install.packages( "devtools" );
library( "devtools" )

# Install & load package SANDRA
install_github( "tpronk/SANDRA/src", force = TRUE );
library( "sandra" );

# SAF Version
safVersion = 1.01;

# Install SANDRA Framework
installAnalysisFramework(
  safVersion
  # Enter path to analysis folder here (without trailing slash)
  # Only for Windows: leave empty to get a folder picker
);
