// {"kind":"typecheck","original":"58607d1e","signature":"swift::constraints::OverloadChoice::getDecl(swift::Type, swift::ValueDecl*, swift::FunctionRefInfo)","signatureAssert":"Assertion failed: (!base->hasTypeParameter()), function getDecl","signatureNext":"ConstraintSystem::performMemberLookup"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  c {
  struct d {
  typealias e<b> = a @e var f = 2
  }
