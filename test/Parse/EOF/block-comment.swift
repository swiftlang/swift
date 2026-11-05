// RUN: %target-typecheck-verify-swift

/* unterminated block comment expected-cparser-note{{comment started here}} expected-astgen-error{{unterminated}}
   expected-cparser-error{{unterminated '/*' comment}} {{68-68=*/}}
