// {"kind":"typecheck","original":"098f0ede","signature":"swift::constraints::ConstraintSystem::getMemberReferenceTypeFromOpenedType(swift::Type, swift::Type, swift::ValueDecl*, swift::constraints::ConstraintLocator*, bool, bool)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not %target-swift-frontend -typecheck %s
enum a<b> {
  c = d  macro d()
}
