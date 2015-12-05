pathSandra = "D:/wamp/www/sandra/trunk/src/R";
modules = dir( pathSandra, pattern = "[.]R$" );
for( m in modules ) {
  source( paste( pathSandra, m, sep = "/" ) );
}
