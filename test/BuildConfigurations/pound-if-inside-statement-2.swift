// RUN: %target-parse-verify-swift -D BLAH

func foo() { // expected-note {{to match this opening '{'}}
  if true {
  } else { // expected-note {{to match this opening '{'}}
#if BLAH
// expected-error@+3{{expected '}' at end of brace statement}}
// expected-error@+2{{expected '}' at end of brace statement}}
// expected-error@+1{{expected #else or #endif at end of configuration block}}
