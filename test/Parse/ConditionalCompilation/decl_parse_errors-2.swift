// RUN: %target-typecheck-verify-swift

class C2 { // expected-note {{to match this opening '{'}}
  #if FOO
  func foo() {}
// expected-error @+2 {{expected '}' in class}}
// expected-error @+1 {{expected #else or #endif at end of conditional compilation block}}
