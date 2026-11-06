// RUN: %target-typecheck-verify-swift

class C2 { // expected-note {{to match this opening '{'}}
  #if FOO
  func foo() {} // expected-astgen-error {{expected '#endif' in conditional compilation block}} expected-astgen-error {{expected '}' to end class}}
// expected-cparser-error @+2 {{expected '}' in class}}
// expected-cparser-error @+1 {{expected #else or #endif at end of conditional compilation block}}
