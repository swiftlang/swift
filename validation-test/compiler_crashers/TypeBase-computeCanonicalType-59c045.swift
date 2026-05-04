// {"kind":"typecheck","original":"30037d48","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"CheckRedeclarationRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self {
  struct c<each d: a> {
    var e: ()
  }
}
