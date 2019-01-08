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
extension Array: P, Q { func paul() {}; mutating func priscilla() {}; func quinn() {} }

class C {}
class D: C, P, Q { func paul() {}; func priscilla() {}; func quinn() {} }

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

func blib() -> P & __opaque Q { return 1 } // expected-error{{'opaque' should appear at the beginning}}
func blab() -> (P, __opaque Q) { return (1, 2) } // expected-error{{'opaque' types are only implemented}}
func blob() -> (__opaque P) -> P { return { $0 } } // expected-error{{'opaque' types are only implemented}}

// Invalid constraints

let zug: __opaque Int = 1 // FIXME expected-error{{'opaque' types are only implemented}}
let zwang: __opaque () = () // FIXME expected-error{{'opaque' types are only implemented}}
let zwoggle: __opaque (() -> ()) = {} // FIXME expected-error{{'opaque' types are only implemented}}

// Type-checking of expressions of opaque type

func alice() -> __opaque P { return 1 }
func bob() -> __opaque P { return 1 }

func grace<T: P>(_ x: T) -> __opaque P { return x }

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

func recursion(x: Int) -> __opaque P {
  if x == 0 {
    return 0
  }
  return recursion(x: x - 1)
}

func noReturnStmts() -> __opaque P { fatalError() } // expected-error{{no return statements}}

func mismatchedReturnTypes(_ x: Bool, _ y: Int, _ z: String) -> __opaque P { // expected-error{{do not have matching underlying types}}
  if x {
    return y // expected-note{{underlying type 'Int'}}
  } else {
    return z // expected-note{{underlying type 'String'}}
  }
}

func jan() -> __opaque P {
  return [marcia(), marcia(), marcia()]
}
func marcia() -> __opaque P {
  return [marcia(), marcia(), marcia()] // expected-error{{defines the opaque type in terms of itself}}
}

protocol R {
  associatedtype S: P, Q // expected-note*{{}}

  func r_out() -> S
  func r_in(_: S)
}

extension Int: R {
  func r_out() -> String {
    return ""
  }
  func r_in(_: String) {}
}

func candace() -> __opaque R {
  return 0
}
func doug() -> __opaque R {
  return 0
}

func gary<T: R>(_ x: T) -> __opaque R {
  return x
}

func sameType<T>(_: T, _: T) {}

func associatedTypeIdentity() {
  let c = candace()
  let d = doug()

  var cr = c.r_out()
  cr = candace().r_out()
  cr = doug().r_out() // expected-error{{}}

  var dr = d.r_out()
  dr = candace().r_out() // expected-error{{}}
  dr = doug().r_out()

  c.r_in(cr)
  c.r_in(c.r_out())
  c.r_in(dr) // expected-error{{}}
  c.r_in(d.r_out()) // expected-error{{}}

  d.r_in(cr) // expected-error{{}}
  d.r_in(c.r_out()) // expected-error{{}}
  d.r_in(dr)
  d.r_in(d.r_out())

  cr.paul()
  cr.priscilla()
  cr.quinn()
  dr.paul()
  dr.priscilla()
  dr.quinn()

  sameType(cr, c.r_out())
  sameType(dr, d.r_out())
  sameType(cr, dr) // expected-error{{}}
  sameType(gary(candace()).r_out(), gary(candace()).r_out())
  sameType(gary(doug()).r_out(), gary(doug()).r_out())
  sameType(gary(doug()).r_out(), gary(candace()).r_out()) // expected-error{{}}
}

/* TODO: diagnostics
struct DoesNotConform {}

func doesNotConform() -> __opaque P {
  return DoesNotConform()
}
*/


