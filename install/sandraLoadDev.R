pathSandra = "D:/wamp/www/sandra/trunk/package/R";
modules = dir( pathSandra );
for( m in modules ) {
  source( paste( pathSandra, m, sep = "/" ) );
}
