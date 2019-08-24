// RUN: %target-typecheck-verify-swift

func foo(x: Int = 0) {}
let bar = foo  // expected-note {{'bar' declared here}}
bar() // expected-error {{missing argument for parameter #1}}

func foo2(_ x: Int = 0) {}
let baz = foo2 // expected-note {{'baz' declared here}}
baz() // expected-error {{missing argument for parameter #1}}
