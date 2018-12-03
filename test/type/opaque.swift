// RUN: %target-swift-frontend -typecheck -verify -enable-opaque-result-types %s

// TODO: The syntax is only parsed, and opaque decls are synthesized for 
// function decls, but the opaque types are not themselves formed, resolved,
// or checked yet.

protocol P {}
protocol Q {}
extension Int: P, Q {}

class C {}
class D: C, P, Q {}

// TODO: Should be valid

let foo: __opaque P = 1 // FIXME expected-error{{'opaque' types are only implemented}}
var computedFoo: __opaque P {  // FIXME expected-error{{'opaque' types are only implemented}}
  get { return 1 }
  set { _ = newValue + 1 }
}
func bar() -> __opaque P {
  return 1
}
func bas() -> __opaque P & Q {
  return 1
}
func zim() -> __opaque C {
  return D()
}
func zang() -> __opaque C & P & Q {
  return D()
}
func zung() -> __opaque AnyObject {
  return D()
}
func zoop() -> __opaque Any {
  return D()
}

//let zingle = {() -> __opaque P in 1 } // FIXME ex/pected-error{{'opaque' types are only implemented}}

// Invalid positions

typealias Foo = __opaque P // expected-error{{'opaque' types are only implemented}}

func blibble(blobble: __opaque P) {} // expected-error{{'opaque' types are only implemented}}

let blubble: () -> __opaque P = { 1 } // expected-error{{'opaque' types are only implemented}}

func blib() -> P & __opaque Q { return 1 } // FIXME expected-error{{'opaque' should appear at the beginning}}
func blab() -> (P, __opaque Q) { return (1, 2) } // expected-error{{'opaque' types are only implemented}}
func blob() -> (__opaque P) -> P { return { $0 } } // expected-error{{'opaque' types are only implemented}}

// Invalid constraints

let zug: __opaque Int = 1 // FIXME expected-error{{'opaque' types are only implemented}}
let zwang: __opaque () = () // FIXME expected-error{{'opaque' types are only implemented}}
let zwoggle: __opaque (() -> ()) = {} // FIXME expected-error{{'opaque' types are only implemented}}
