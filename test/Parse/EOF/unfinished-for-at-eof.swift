// RUN: %target-typecheck-verify-swift

func fuzz() { for var H
// expected-cparser-error@-1{{expected 'in' after for-each pattern}}
// expected-cparser-error@-2{{expected Sequence expression for for-each loop}} expected-astgen-error@-2{{expected '}' to end function}}
// expected-cparser-error@-3{{expected '{' to start the body of for-each loop}} expected-astgen-error@-3{{expected 'in', expression, and body in 'for' statement}}
// expected-note@-4 {{to match this opening '{'}}
// expected-cparser-error@+1{{expected '}' at end of brace statement}}
