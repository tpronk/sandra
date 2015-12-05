pathSandra = "D:/wamp/www/sandra/trunk/src/R";

rm( sandra );
modules = dir( pathSandra );
for( m in modules ) {
  source( paste( pathSandra, m, sep = "/" ) );
}
