// RUN: %target-typecheck-verify-swift -parse-as-library -D FOO

// '-parse-as-library' doesn't allow exprssions nor statements in #if blocks.

func foo() {}

#if FOO
  foo() // expected-error {{expressions are not allowed at the top level}}
#else
  if true { foo() } // expected-error {{statements are not allowed at the top level}}
#endif
