// RUN: %target-typecheck-verify-swift

public
#if FOO // expected-error {{expected declaration}}
var val1: Int = 0
#else
var val1: UInt = 1
#endif

struct S2 { // expected-note {{in declaration of 'S2'}}
  @available(*, deprecated)
#if FOO // expected-error {{expected declaration}}
  func fn1() {}
#endif
}

// expected-error@+2 {{expected declaration}}
private
#sourceLocation(file: "test.swift", line: 1)
var val2: Int = 0
#sourceLocation()

// expected-error@+3 {{expected declaration}}
// expected-error@+2 {{#line directive was renamed to #sourceLocation}}
lazy
#line 12 "test.swift"
var val3: Int = 0;
#line

class C { // expected-note {{to match this opening '{'}}

#if os(iOS)
	func foo() {}
} // expected-error{{unexpected '}' in conditional compilation block}}
#else
	func bar() {}
	func baz() {}
} // expected-error{{unexpected '}' in conditional compilation block}}
#endif
// expected-error@+1{{expected '}' in class}}
