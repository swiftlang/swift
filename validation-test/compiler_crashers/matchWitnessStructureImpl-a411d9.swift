// {"kind":"typecheck","signature":"matchWitnessStructureImpl(swift::ValueDecl*, swift::ValueDecl*, bool&, bool&)","signatureAssert":"Assertion failed: (!req->isInvalid() && \"Cannot have an invalid requirement here\"), function matchWitnessStructureImpl","signatureNext":"matchWitness"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objc protocol P {
  @objc optional func f() -> Undefined
}
class B: P {
}
class C: B {
  func f() {
  }
}
