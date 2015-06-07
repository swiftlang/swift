// RUN: %target-parse-verify-swift

protocol P { }
@objc protocol OP { }
protocol CP : class { }

@objc protocol SP : OP {
  static func createNewOne() -> SP
}

func fP<T : P>(t: T) { } // expected-note 2{{in call to function 'fP'}}
func fOP<T : OP>(t: T) { } // expected-note{{in call to function 'fOP'}}
func fOPE(t: OP) { }
func fSP<T : SP>(t: T) { } // expected-note{{in call to function 'fSP'}}
func fAO<T : AnyObject>(t: T) { } // expected-note 3{{in call to function 'fAO'}}
func fAOE(t: AnyObject) { }
func fT<T>(t: T) { }

func testPassExistential(p: P, op: OP, opp: protocol<P, OP>, cp: CP, sp: SP, any: Any, ao: AnyObject) {
  fP(p) // expected-error{{generic parameter 'T' is constrained to a non-@objc protocol type 'P' and cannot be bound to an existential type}}
  fAO(p) // expected-error{{generic parameter 'T' cannot be bound to an existential type containing a non-@objc protocol type 'P'}}
  fAOE(p) // expected-error{{cannot invoke 'fAOE' with an argument list of type '(P)'}} // expected-note{{expected an argument list of type '(AnyObject)'}}
  fT(p)

  fOP(op)
  fAO(op)
  fAOE(op)
  fT(op)

  fAO(cp) // expected-error{{generic parameter 'T' cannot be bound to an existential type containing a non-@objc protocol type 'CP'}}
  fAOE(cp)
  fT(cp)

  fP(opp) // expected-error{{generic parameter 'T' is constrained to a non-@objc protocol type 'P' and cannot be bound to an existential type}}
  fOP(opp) // expected-error{{generic parameter 'T' cannot be bound to an existential type containing a non-@objc protocol type 'P'}}
  fAO(opp) // expected-error{{generic parameter 'T' cannot be bound to an existential type containing a non-@objc protocol type 'P'}}
  fAOE(opp)
  fT(opp)

  fOP(sp)
  fSP(sp) // expected-error{{generic parameter 'T' is constrained to a protocol type with static methods 'SP' and cannot be bound to an existential type}}
  fAO(sp)
  fAOE(sp)
  fT(sp)

  fT(any)

  fAO(ao)
  fAOE(ao)
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
