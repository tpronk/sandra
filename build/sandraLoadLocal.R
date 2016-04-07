# Load sandra R scripts from a local directory
pathSandra = "D:/wamp/www/sandra/branches/tpronk_dev/src/R";
library( "rjson" )
library( "lubridate" )
library( "tcltk" )
modules = dir( pathSandra, pattern = "[.]R$" );
for( m in modules ) {
  source( paste( pathSandra, m, sep = "/" ) );
}
