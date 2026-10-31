// RUN: %target-typecheck-verify-swift -D FOO

// Test case for statements in #if block in types.

func foo() {}

struct S1 { // expected-cparser-note {{in declaration of 'S1'}}
#if FOO
  return 1; // expected-cparser-error {{expected declaration}} expected-astgen-error {{unexpected code 'return 1' in conditional compilation clause}}
#elseif BAR
  foo(); // expected-cparser-error {{expected 'func' keyword in instance method declaration}} expected-astgen-error {{expected 'func' in function}}
#endif
}
