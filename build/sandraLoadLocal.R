# Load sandra R scripts from a local directory
pathSandra = "D:/wamp/www/sandra/trunk/src/R";
library( "rjson" )
library( "lubridate" )
library( "tcltk" )
library( "tools" )
modules = dir( pathSandra, pattern = "[.]R$" );
for( m in modules ) {
  source( paste( pathSandra, m, sep = "/" ) );
}
