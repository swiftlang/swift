// RUN: %target-parse-verify-swift

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

class C { // expected-note 3 {{in declaration of 'C'}}

#if os(iOS)
	func foo() {}
} // expected-error{{expected declaration}} expected-error{{expected #else or #endif at end of conditional compilation block}}
#else
	func bar() {}
	func baz() {}
} // expected-error{{expected #else or #endif at end of conditional compilation block}} expected-error {{expected declaration}}
#endif
// expected-error@+1{{expected declaration}}
