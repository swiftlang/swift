// RUN: %target-typecheck-verify-swift

public // expected-astgen-error {{expected declaration after 'public' modifier}}
#if FOO // expected-cparser-error {{expected declaration}}
var val1: Int = 0
#else
var val1: UInt = 1
#endif

struct S2 { // expected-cparser-note {{in declaration of 'S2'}}
  @available(*, deprecated) // expected-astgen-error {{expected declaration after attribute}}
#if FOO // expected-cparser-error {{expected declaration}}
  func fn1() {}
#endif
}

// expected-cparser-error@+2 {{expected declaration}} expected-astgen-error@+2 {{unexpected code '#sourceLocation(file: "test.swift", line: 1)' in variable}}
private
#sourceLocation(file: "test.swift", line: 1)
var val2: Int = 0
#sourceLocation()

// expected-cparser-error@+5 {{expected declaration}}
// expected-error@+4 {{#line directive was renamed to #sourceLocation}}
// expected-astgen-error@+2 {{parameterless closing #sourceLocation() directive without prior opening #sourceLocation(file:,line:) directive}}
// expected-astgen-error@+2 2 {{consecutive statements on a line must be separated by newline or ';'}} expected-astgen-warning@+2 {{#line literal is unused}} expected-astgen-warning@+2 {{integer literal is unused}} expected-astgen-warning@+2 {{string literal is unused}}
lazy
#line 12 "test.swift"
var val3: Int = 0;
#line // expected-cparser-error {{#line directive was renamed to #sourceLocation}} expected-astgen-warning {{#line literal is unused}}

class C { // expected-note {{to match this opening '{'}}

#if os(iOS)
	func foo() {}
} // expected-cparser-error{{unexpected '}' in conditional compilation block}} expected-astgen-error {{unexpected brace in conditional compilation clause}}
#else
	func bar() {}
	func baz() {}
} // expected-cparser-error{{unexpected '}' in conditional compilation block}} expected-astgen-error {{unexpected brace in conditional compilation clause}}
#endif // expected-astgen-error {{expected '}' to end class}}
// expected-cparser-error@+1{{expected '}' in class}}
