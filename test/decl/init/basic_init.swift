// RUN: %target-parse-verify-swift

class Foo {
  func bar(bar) {} // expected-error{{use of undeclared type 'bar'}}
}

class C {
	var triangle : triangle // expected-error{{use of undeclared type 'triangle'}}

	init() {}
}

typealias t = t // expected-error {{type alias 't' circularly references itself}}
