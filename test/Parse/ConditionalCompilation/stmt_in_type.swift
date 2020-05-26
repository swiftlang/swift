// RUN: %target-typecheck-verify-swift -D FOO

// Test case for statements in #if block in types.

func foo() {}

struct S1 { // expected-note {{in declaration of 'S1'}}
#if FOO
  return 1; // expected-error {{expected declaration}}
#elseif BAR
  foo(); // expected-error {{expected 'func' keyword in instance method declaration}}
#endif
}
