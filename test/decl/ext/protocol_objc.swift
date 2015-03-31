// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

@objc protocol OP1 {
  func reqOP1a() -> Bool
}

extension OP1 {
  func extOP1a() -> Bool { return !reqOP1a() }
}

class OC1 : OP1 {
  @objc func reqOP1a() -> Bool { return true }
}

func testOP1(oc1: OC1, ao: AnyObject) {
  oc1.extOP1a()
  ao.reqOP1a!() // okay

  // Extension of @objc protocol does not have @objc members.
  ao.extOP1a!() // expected-error{{'AnyObject' does not have a member named 'extOP1a()'}}
}
