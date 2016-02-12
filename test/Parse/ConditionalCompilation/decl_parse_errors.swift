// RUN: %target-parse-verify-swift

class C {

#if os(iOS)
	func foo() {}
} // expected-error{{expected declaration}} expected-error{{expected #else or #endif at end of conditional compilation block}}
#else
	func bar() {}
	func baz() {}
} // expected-error{{expected #else or #endif at end of conditional compilation block}} expected-error {{expected declaration}}
#endif
// expected-error@+1{{expected declaration}}
