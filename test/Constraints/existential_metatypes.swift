// RUN: %target-parse-verify-swift

protocol P {}

protocol Q {}

protocol PP: P {}

var qp: Q.Protocol
var pp: P.Protocol = qp // expected-error{{'Q' is not identical to 'P'}}

var qt: Q.Type
qt = qp // expected-error{{cannot assign a value of type 'Q.Protocol' to a value of type 'Q.Type'}}
qp = qt // expected-error{{cannot assign a value of type 'Q.Type' to a value of type 'Q.Protocol'}}
var pt: P.Type = qt // expected-error{{type 'Q' does not conform to protocol 'P'}}
pt = pp // expected-error{{cannot assign a value of type 'P.Protocol' to a value of type 'P.Type'}}
pp = pt // expected-error{{cannot assign a value of type 'P.Type' to a value of type 'P.Protocol'}}

var pqt: protocol<P, Q>.Type
pt = pqt
qt = pqt


var pqp: protocol<P, Q>.Protocol
pp = pqp // expected-error{{cannot assign}}
qp = pqp // expected-error{{cannot assign}}

var ppp: PP.Protocol
pp = ppp // expected-error{{cannot assign}}

var ppt: PP.Type
pt = ppt

var at: Any.Type
at = pt

var ap: Any.Protocol
ap = pp // expected-error{{cannot assign}}
ap = pt // expected-error{{cannot assign}}

// Meta-metatypes

protocol Toaster {}
class WashingMachine : Toaster {}
class Dryer : WashingMachine {}
class HairDryer {}

let a: Toaster.Type.Protocol = Toaster.Type.self
let b: Any.Type.Type = Toaster.Type.self
let c: Any.Type.Protocol = Toaster.Type.self // expected-error {{'Toaster' is not identical to 'Any'}}
let d: Toaster.Type.Type = WashingMachine.Type.self
let e: Any.Type.Type = WashingMachine.Type.self
let f: Toaster.Type.Type = Dryer.Type.self
let g: Toaster.Type.Type = HairDryer.Type.self // expected-error {{type 'HairDryer' does not conform to protocol 'Toaster'}}
let h: WashingMachine.Type.Type = Dryer.Type.self // expected-error {{'Dryer' is not identical to 'WashingMachine'}}

func generic<T : WashingMachine>(t: T.Type) {
  let _: Toaster.Type.Type = t.dynamicType
}

// rdar://problem/20780797
protocol P2 {
  init(x: Int)
  var elements: [P2] {get}
}

extension P2 {
  init() { self.init(x: 5) }
}

func testP2(pt: P2.Type) {
  pt.init().elements
}
