// RUN: %target-parse-verify-swift

func fuzz() { for var H
// expected-note@-1 {{to match this opening '{'}}
// expected-error@-2{{type annotation missing in pattern}}
// expected-error@-3 2 {{expected ';' in 'for' statement}}
// expected-error@+4{{expected '{' in 'for' statement}}
// expected-error@+3{{expected '}' at end of brace statement}}
// expected-error@+2{{expected condition in 'for' statement}}
// expected-error@+1{{expected expression}}
