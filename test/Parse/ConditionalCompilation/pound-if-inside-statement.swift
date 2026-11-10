// RUN: %target-typecheck-verify-swift

func foo() { // expected-note {{to match this opening '{'}}
  if true {
  } else { // expected-note {{to match this opening '{'}}
#if BLAH // expected-astgen-error {{expected '#endif' in conditional compilation block}} expected-astgen-error {{expected '}' to end 'if' statement}} expected-astgen-error {{expected '}' to end function}}
// expected-cparser-error@+3{{expected '}' at end of brace statement}}
// expected-cparser-error@+2{{expected '}' at end of brace statement}}
// expected-cparser-error@+1{{expected #else or #endif at end of conditional compilation block}}
