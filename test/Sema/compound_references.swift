// RUN: %target-typecheck-verify-swift

func foo(x: Int) {} // expected-note * {{did you mean 'foo(x:)'?}}
let f = foo(_:) // expected-error {{use of unresolved identifier 'foo'}}
