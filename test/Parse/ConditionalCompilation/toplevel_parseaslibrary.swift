// RUN: %target-typecheck-verify-swift -parse-as-library -D FOO

// '-parse-as-library' doesn't allow expressions nor statements in #if blocks.

func foo() {}

#if FOO
  foo() // expected-error {{expressions are not allowed at the top level}}
  // expected-note@-1 {{top-level code is only allowed in 'main.swift'}}
#else
  if true { foo() } // expected-error {{statements are not allowed at the top level}}
#endif
