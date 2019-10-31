// RUN: %target-typecheck-verify-swift

func fuzz() { for var H
// expected-error@-1{{expected 'in' after for-each pattern}}
// expected-error@-2{{expected Sequence expression for for-each loop}}
// expected-error@-3{{expected '{' to start the body of for-each loop}}
// expected-note@-4 {{to match this opening '{'}}
// expected-error@+1{{expected '}' at end of brace statement}}
