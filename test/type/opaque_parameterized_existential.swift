// RUN: %target-swift-frontend -disable-availability-checking -typecheck -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -enable-experimental-feature ImplicitSome -typecheck -verify %s

// REQUIRES: asserts

// I do not like nested some type params,
// I do not like them Σam-i-am
protocol P<T> {
  associatedtype T
}

extension Never: P { typealias T = Never }

// I do not like them written clear
func test() -> any P<some P> { fatalError() } // expected-error {{'some' types cannot be used in constraints on existential types}} expected-error {{generic parameter}}
// I do not like them nested here
func test() -> any P<[some P]> { fatalError() } // expected-error {{'some' types cannot be used in constraints on existential types}} expected-error {{generic parameter}}
// I do not like them under questions
func test() -> any P<(some P)??> { fatalError() } // expected-error {{'some' types cannot be used in constraints on existential types}} expected-error {{generic parameter}}
// I do not like meta-type intentions
func test() -> (any P<some P>).Type { fatalError() } // expected-error {{'some' types cannot be used in constraints on existential types}} expected-error {{generic parameter}}
// I do not like them (meta)static-ly
func test() -> any P<some P>.Type { fatalError() } // expected-error {{'some' types cannot be used in constraints on existential types}} expected-error {{generic parameter}}
// I do not like them tupled-three
func test() -> (Int, any P<some P>, Int) { fatalError() } // expected-error {{'some' types cannot be used in constraints on existential types}} expected-error {{generic parameter}}
// I do not like them in generics
struct Wrapper<T> {}
func test() -> any P<Wrapper<some P>> { fatalError() } // expected-error {{'some' types cannot be used in constraints on existential types}} expected-error {{generic parameter}}
// Your attempts to nest them put me in hysterics.
func test(_ x: any P<some P>) {} // expected-error {{'some' types cannot be used in constraints on existential types}}

// No, I do not like nested some type params,
// I do not like them Σam-i-am
