// RUN: %target-swift-frontend -typecheck -verify -enable-opaque-result-types %s

// TODO: The syntax is only parsed, and opaque decls are synthesized for 
// function decls, but the opaque types are not themselves formed, resolved,
// or checked yet.

protocol P {
  func paul()
  mutating func priscilla()
}
protocol Q { func quinn() }
extension Int: P, Q { func paul() {}; mutating func priscilla() {}; func quinn() {} }
extension String: P, Q { func paul() {}; mutating func priscilla() {}; func quinn() {} }

class C {}
class D: C, P, Q { func paul() {}; func priscilla() {}; func quinn() {} }

// TODO: Should be valid

let foo: __opaque P = 1 // FIXME expected-error{{'opaque' types are only implemented}}
var computedFoo: __opaque P {  // FIXME expected-error{{'opaque' types are only implemented}}
  get { return 1 }
  set { _ = newValue + 1 }
}
func bar() -> __opaque P {
  return 1 // FIXME expected-error{{convert}}
}
func bas() -> __opaque P & Q {
  return 1 // FIXME expected-error{{convert}}
}
func zim() -> __opaque C {
  return D() // FIXME expected-error{{convert}}
}
func zang() -> __opaque C & P & Q {
  return D() // FIXME expected-error{{convert}}
}
func zung() -> __opaque AnyObject {
  return D() // FIXME expected-error{{convert}}
}
func zoop() -> __opaque Any {
  return D() // FIXME expected-error{{convert}}
}

//let zingle = {() -> __opaque P in 1 } // FIXME ex/pected-error{{'opaque' types are only implemented}}

// Invalid positions

typealias Foo = __opaque P // expected-error{{'opaque' types are only implemented}}

func blibble(blobble: __opaque P) {} // expected-error{{'opaque' types are only implemented}}

let blubble: () -> __opaque P = { 1 } // expected-error{{'opaque' types are only implemented}}

func blib() -> P & __opaque Q { return 1 } // expected-error{{'opaque' should appear at the beginning}} // FIXME expected-error{{convert}}
func blab() -> (P, __opaque Q) { return (1, 2) } // expected-error{{'opaque' types are only implemented}}
func blob() -> (__opaque P) -> P { return { $0 } } // expected-error{{'opaque' types are only implemented}}

// Invalid constraints

let zug: __opaque Int = 1 // FIXME expected-error{{'opaque' types are only implemented}}
let zwang: __opaque () = () // FIXME expected-error{{'opaque' types are only implemented}}
let zwoggle: __opaque (() -> ()) = {} // FIXME expected-error{{'opaque' types are only implemented}}

// Type-checking of expressions of opaque type

func alice() -> __opaque P { return 1 } // FIXME expected-error{{convert}}
func bob() -> __opaque P { return 1 } // FIXME expected-error{{convert}}

func grace<T: P>(_ x: T) -> __opaque P { return x } // FIXME expected-error{{convert}}

func typeIdentity() {
  do {
    var a = alice()
    a = alice()
    a = bob() // expected-error{{}}
    a = grace(1) // expected-error{{}}
    a = grace("two") // expected-error{{}}
  }

  do {
    var af = alice
    af = alice
    af = bob // expected-error{{}}
    af = grace // expected-error{{}}
  }

  do {
    var b = bob()
    b = alice() // expected-error{{}}
    b = bob()
    b = grace(1) // expected-error{{}}
    b = grace("two") // expected-error{{}}
  }

  do {
    var gi = grace(1)
    gi = alice() // expected-error{{}}
    gi = bob() // expected-error{{}}
    gi = grace(2)
    gi = grace("three") // expected-error{{}}
  }

  do {
    var gs = grace("one")
    gs = alice() // expected-error{{}}
    gs = bob() // expected-error{{}}
    gs = grace(2) // expected-error{{}}
    gs = grace("three")
  }

  // The opaque type should conform to its constraining protocols
  do {
    let gs = grace("one")
    var ggs = grace(gs)
    ggs = grace(gs)
  }

  // The opaque type should expose the members implied by its protocol
  // constraints
  // TODO: associated types
  do {
    var a = alice()
    a.paul()
    a.priscilla()
  }
}

