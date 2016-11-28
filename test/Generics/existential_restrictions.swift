// RUN: %target-typecheck-verify-swift

protocol P { }
@objc protocol OP { }
protocol CP : class { }

@objc protocol SP : OP {
  static func createNewOne() -> SP
}

func fP<T : P>(_ t: T) { }
func fOP<T : OP>(_ t: T) { }
func fOPE(_ t: OP) { }
func fSP<T : SP>(_ t: T) { }
func fAO<T : AnyObject>(_ t: T) { }
func fAOE(_ t: AnyObject) { }
func fT<T>(_ t: T) { }

func testPassExistential(_ p: P, op: OP, opp: OP & P, cp: CP, sp: SP, any: Any, ao: AnyObject) {
  fP(p) // expected-error{{cannot invoke 'fP' with an argument list of type '(P)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAO(p) // expected-error{{cannot invoke 'fAO' with an argument list of type '(P)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAOE(p) // expected-error{{argument type 'P' does not conform to expected type 'AnyObject'}}
  fT(p)

  fOP(op)
  fAO(op)
  fAOE(op)
  fT(op)

  fAO(cp) // expected-error{{cannot invoke 'fAO' with an argument list of type '(CP)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAOE(cp)
  fT(cp)

  fP(opp) // expected-error{{cannot invoke 'fP' with an argument list of type '(OP & P)'}} // expected-note{{expected an argument list of type '(T)'}}
  fOP(opp) // expected-error{{cannot invoke 'fOP' with an argument list of type '(OP & P)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAO(opp) // expected-error{{cannot invoke 'fAO' with an argument list of type '(OP & P)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAOE(opp)
  fT(opp)

  fOP(sp)
  fSP(sp) // expected-error{{cannot invoke 'fSP' with an argument list of type '(SP)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAO(sp)
  fAOE(sp)
  fT(sp)

  fT(any)

  fAO(ao)
  fAOE(ao)
}

class GP<T : P> {}
class GOP<T : OP> {}
class GCP<T : CP> {}
class GSP<T : SP> {}
class GAO<T : AnyObject> {}

func blackHole(_ t: Any) {}

func testBindExistential() {
  blackHole(GP<P>()) // expected-error{{using 'P' as a concrete type conforming to protocol 'P' is not supported}}
  blackHole(GOP<OP>())
  blackHole(GCP<CP>()) // expected-error{{using 'CP' as a concrete type conforming to protocol 'CP' is not supported}}
  blackHole(GAO<P>()) // expected-error{{type 'P' does not conform to protocol 'AnyObject'}}
  blackHole(GAO<OP>())
  blackHole(GAO<CP>()) // expected-error{{using 'CP' as a concrete type conforming to protocol 'AnyObject' is not supported}}
  blackHole(GSP<SP>()) // expected-error{{'SP' cannot be used as a type conforming to protocol 'SP' because 'SP' has static requirements}}
  blackHole(GAO<AnyObject>())
}

// rdar://problem/21087341
protocol Mine {}
class M1: Mine {}
class M2: Mine {}
extension Collection where Iterator.Element : Mine {
    final func takeAll() {}
}

func foo() {
  let allMine: [Mine] = [M1(), M2(), M1()]
  // FIXME: we used to have a better diagnostic here -- the type checker
  // would admit the conformance Mine : Mine, and later when computing
  // substitutions, a specific diagnostic was generated. Now the
  // conformance is rejected because Mine is not @objc, and we hit the
  // generic no overloads error path. The error should actually talk
  // about the return type, and this can happen in other contexts as well;
  // <rdar://problem/21900971> tracks improving QoI here.
  allMine.takeAll() // expected-error{{using 'Mine' as a concrete type conforming to protocol 'Mine' is not supported}}
}
