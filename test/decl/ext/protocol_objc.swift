// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

@objc protocol OP1 {
  func reqOP1a() -> Bool // expected-note {{did you mean 'reqOP1a'?}}
}

extension OP1 {
  final func extOP1a() -> Bool { return !reqOP1a() }
}

class OC1 : OP1 {
  @objc func reqOP1a() -> Bool { return true }
}

func testOP1(_ oc1: OC1, ao: AnyObject) {
  _ = oc1.extOP1a()
  // expected-warning @+1 {{result of call is unused}}
  ao.reqOP1a!() // okay

  // Extension of @objc protocol does not have @objc members.
  ao.extOP1a!() // expected-error{{value of type 'AnyObject' has no member 'extOP1a'}}
}
