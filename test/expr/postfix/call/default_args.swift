// RUN: %target-parse-verify-swift

func foo(#x: Int = 0) {}
let bar = foo
bar() // expected-error {{missing argument for parameter 'x'}}

func foo2(x: Int = 0) {}
let baz = foo2
baz() // expected-error {{missing argument for parameter #1}}
