// {"kind":"typecheck","original":"17cf12b3","signature":"swift::TypeBase::computeInvertibleConformances()","signatureAssert":"Assertion failed: (!canType->hasTypeParameter()), function computeInvertibleConformances","signatureNext":"TypeBase::isNoncopyable"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
struct a<b> {
  var d: b {
    "\(#keyPath(d))"
    c
  }
}
