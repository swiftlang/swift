// {"kind":"typecheck","original":"3a2652c9","signature":"(anonymous namespace)::OpaqueUnderlyingTypeChecker::check()","signatureAssert":"Assertion failed: (!UniqueUnderlyingType.has_value() && \"resetting underlying type?!\"), function setUniqueUnderlyingTypeSubstitutions"}
// RUN: not %target-swift-frontend -typecheck %s
var a: some Any = 0.0 {
  0
}
