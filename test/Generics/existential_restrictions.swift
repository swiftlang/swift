// RUN: %target-parse-verify-swift

protocol P { }
@objc protocol OP { }
protocol CP : class { }

@objc protocol SP : OP {
  static func createNewOne() -> SP
}

func fP<T : P>(t: T) { }
func fOP<T : OP>(t: T) { }
func fOPE(t: OP) { }
func fSP<T : SP>(t: T) { }
func fAO<T : AnyObject>(t: T) { }
func fAOE(t: AnyObject) { }
func fT<T>(t: T) { }

func testPassExistential(p: P, op: OP, opp: protocol<OP, P>, cp: CP, sp: SP, any: Any, ao: AnyObject) {
  fP(p) // expected-error{{cannot invoke 'fP' with an argument list of type '(P)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAO(p) // expected-error{{cannot invoke 'fAO' with an argument list of type '(P)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAOE(p) // expected-error{{cannot invoke 'fAOE' with an argument list of type '(P)'}} // expected-note{{expected an argument list of type '(AnyObject)'}}
  fT(p)

  fOP(op)
  fAO(op)
  fAOE(op)
  fT(op)

  fAO(cp) // expected-error{{cannot invoke 'fAO' with an argument list of type '(CP)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAOE(cp)
  fT(cp)

  fP(opp) // expected-error{{cannot invoke 'fP' with an argument list of type '(protocol<OP, P>)'}} // expected-note{{expected an argument list of type '(T)'}}
  fOP(opp) // expected-error{{cannot invoke 'fOP' with an argument list of type '(protocol<OP, P>)'}} // expected-note{{expected an argument list of type '(T)'}}
  fAO(opp) // expected-error{{cannot invoke 'fAO' with an argument list of type '(protocol<OP, P>)'}} // expected-note{{expected an argument list of type '(T)'}}
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

func blackHole(t: Any) {}

func testBindExistential() {
  blackHole(GP<P>()) // expected-error{{protocol type 'P' does not conform to protocol 'P' because 'P' is not declared @objc}}
  blackHole(GOP<OP>())
  blackHole(GCP<CP>()) // expected-error{{protocol type 'CP' does not conform to protocol 'CP' because 'CP' is not declared @objc}}
  blackHole(GAO<P>()) // expected-error{{protocol type 'P' does not conform to protocol 'AnyObject' because 'P' is not declared @objc}}
  blackHole(GAO<OP>())
  blackHole(GAO<CP>()) // expected-error{{protocol type 'CP' does not conform to protocol 'AnyObject' because 'CP' is not declared @objc}}
  blackHole(GSP<SP>()) // expected-error{{protocol type 'SP' does not conform to protocol 'SP' because 'SP' defines static methods}}
  blackHole(GAO<AnyObject>())
}

// rdar://problem/21087341
protocol Mine {}
class M1: Mine {}
class M2: Mine {}
extension CollectionType where Generator.Element : Mine {
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
  // <rdar://problem/21248136> tracks improving QoI here.
  allMine.takeAll() // expected-error{{cannot invoke 'takeAll' with no arguments}}
}
