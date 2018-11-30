// RUN: %target-swift-frontend -typecheck -verify -enable-opaque-result-types %s

// TODO: The syntax is only parsed currently, no type checking is implemented
// yet.

protocol P {}
protocol Q {}
extension Int: P, Q {}

class C {}
class D: C, P, Q {}

// TODO: Should be valid

let foo: __opaque P = 1 // FIXME expected-error{{'opaque' types are only implemented}}
func bar() -> __opaque P { // FIXME expected-error{{'opaque' types are only implemented}}
  return 1
}
func bas() -> __opaque P & Q { // FIXME expected-error{{'opaque' types are only implemented}}
  return 1
}
func zim() -> __opaque C { // FIXME expected-error{{'opaque' types are only implemented}}
  return D()
}
func zang() -> __opaque C & P & Q { // FIXME expected-error{{'opaque' types are only implemented}}
  return D()
}

//let zung = {() -> __opaque P in 1 } // FIXME ex/pected-error{{'opaque' types are only implemented}}

// Invalid positions

typealias Foo = __opaque P // expected-error{{'opaque' types are only implemented}}

func blibble(blobble: __opaque P) {} // expected-error{{'opaque' types are only implemented}}

let blubble: () -> __opaque P = { 1 } // expected-error{{'opaque' types are only implemented}}

func blib() -> P & __opaque Q { return 1 } // FIXME expected-error{{'opaque' types are only implemented}} expected-error{{'opaque' should appear at the beginning}}
func blab() -> (P, __opaque Q) { return (1, 2) } // expected-error{{'opaque' types are only implemented}}
func blob() -> (__opaque P) -> P { return { $0 } } // expected-error{{'opaque' types are only implemented}}

// Invalid constraints

let zug: __opaque Int = 1 // FIXME expected-error{{'opaque' types are only implemented}}
let zwang: __opaque () = () // FIXME expected-error{{'opaque' types are only implemented}}
let zwoggle: __opaque (() -> ()) = {} // FIXME expected-error{{'opaque' types are only implemented}}
