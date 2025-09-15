// RUN: %target-typecheck-verify-swift -swift-version 6

// rdar://145593552 - Make sure we don't crash.
func foo(_ x: Int) {} // expected-note {{'foo' declared here}}
foo<Void>("")
// expected-error@-1 {{cannot explicitly specialize global function 'foo'}}
// expected-error@-2 {{cannot convert value of type 'String' to expected argument type 'Int'}}

func bar(x: Int = 0) {} // expected-note {{'bar(x:)' declared here}}
bar<Void>() // expected-error {{cannot explicitly specialize global function 'bar(x:)'}}
