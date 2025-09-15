// {"kind":"typecheck","original":"3a7e17d4","signature":"swift::TypeBase::computeInvertibleConformances()","signatureAssert":"Assertion failed: (!canType->hasUnboundGenericType()), function computeInvertibleConformances"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b
  extension a
...
