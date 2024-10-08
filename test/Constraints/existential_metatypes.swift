// RUN: %target-typecheck-verify-swift

protocol P {}

protocol Q {}

protocol PP: P {}

var qp: Q.Protocol
var pp: P.Protocol = qp // expected-error{{cannot convert value of type '(any Q).Type' to specified type '(any P).Type'}}

var qt: Q.Type
qt = qp // expected-error{{cannot assign value of type '(any Q).Type' to type 'any Q.Type'}}
qp = qt // expected-error{{cannot assign value of type 'any Q.Type' to type '(any Q).Type'}}
var pt: P.Type = qt // expected-error{{cannot convert value of type 'any Q.Type' to specified type 'any P.Type'}}
pt = pp // expected-error{{cannot assign value of type '(any P).Type' to type 'any P.Type'}}
pp = pt // expected-error{{cannot assign value of type 'any P.Type' to type '(any P).Type'}}

var pqt: (P & Q).Type
pt = pqt
qt = pqt


var pqp: (P & Q).Protocol
pp = pqp // expected-error{{cannot assign value of type '(any P & Q).Type' to type '(any P).Type'}}
qp = pqp // expected-error{{cannot assign value of type '(any P & Q).Type' to type '(any Q).Type'}}

var ppp: PP.Protocol
pp = ppp // expected-error{{cannot assign value of type '(any PP).Type' to type '(any P).Type'}}

var ppt: PP.Type
pt = ppt

var at: Any.Type
at = pt

var ap: Any.Protocol
ap = pp // expected-error{{cannot assign value of type '(any P).Type' to type '(any Any).Type'}}
ap = pt // expected-error{{cannot assign value of type 'any P.Type' to type '(any Any).Type'}}

// Meta-metatypes

protocol Toaster {}
class WashingMachine : Toaster {}
class Dryer : WashingMachine {}
class HairDryer {}

let a: Toaster.Type.Protocol = Toaster.Type.self
let b: Any.Type.Type = Toaster.Type.self // expected-error {{cannot convert value of type '(any Toaster.Type).Type' to specified type 'any Any.Type.Type'}}
let c: Any.Type.Protocol = Toaster.Type.self // expected-error {{cannot convert value of type '(any Toaster.Type).Type' to specified type '(any Any.Type).Type'}}
let d: Toaster.Type.Type = WashingMachine.Type.self
let e: Any.Type.Type = WashingMachine.Type.self
let f: Toaster.Type.Type = Dryer.Type.self
let g: Toaster.Type.Type = HairDryer.Type.self // expected-error {{cannot convert value of type 'HairDryer.Type.Type' to specified type 'any Toaster.Type.Type'}}
let h: WashingMachine.Type.Type = Dryer.Type.self // expected-error {{cannot convert value of type 'Dryer.Type.Type' to specified type 'WashingMachine.Type.Type'}}

func generic<T : WashingMachine>(_ t: T.Type) {
  let _: Toaster.Type.Type = type(of: t)
}

// rdar://problem/20780797
protocol P2 {
  init(x: Int)
  var elements: [P2] {get}
}

extension P2 {
  init() { self.init(x: 5) }
}

func testP2(_ pt: P2.Type) {
  pt.init().elements // expected-warning {{expression of type '[any P2]' is unused}}
}

// rdar://problem/21597711
protocol P3 {
  func withP3(_ fn: (P3) -> ())
}

class Something {
  func takeP3(_ p: P3) { }
}


func testP3(_ p: P3, something: Something) {
  p.withP3(Something.takeP3(something))
}

func testIUOToAny(_ t: AnyObject.Type!) {
  let _: Any = t
}

protocol P4<T> {
  associatedtype T
}

protocol Q4<T> {
  associatedtype T
}

protocol PP4<U>: P4<Self.U.T> {
  associatedtype U: P4<U>
}

func parameterizedExistentials() {
  var qp: (any Q4<Int>).Type
  var pp: (any P4<Int>).Type = qp // expected-error{{cannot convert value of type '(any Q4<Int>).Type' to specified type '(any P4<Int>).Type'}}

  var qt: any Q4<Int>.Type
  qt = qp // expected-error{{cannot assign value of type '(any Q4<Int>).Type' to type 'any Q4<Int>.Type'}}
  qp = qt // expected-error{{cannot assign value of type 'any Q4<Int>.Type' to type '(any Q4<Int>).Type'}}
  var pt: any P4<Int>.Type = qt // expected-error{{cannot convert value of type 'any Q4<Int>.Type' to specified type 'any P4<Int>.Type'}}
  pt = pp // expected-error{{cannot assign value of type '(any P4<Int>).Type' to type 'any P4<Int>.Type'}}
  pp = pt // expected-error{{cannot assign value of type 'any P4<Int>.Type' to type '(any P4<Int>).Type'}}

  var ppp: (any PP4<Int>).Type
  pp = ppp // expected-error{{cannot assign value of type '(any PP4<Int>).Type' to type '(any P4<Int>).Type'}}

  var ppt: any PP4<Int>.Type
  pt = ppt // expected-error {{cannot assign value of type 'any PP4<Int>.Type' to type 'any P4<Int>.Type'}}
}
