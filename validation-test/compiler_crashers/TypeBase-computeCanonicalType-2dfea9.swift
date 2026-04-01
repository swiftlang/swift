// {"kind":"typecheck","original":"2c7b09da","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"TypeBase::getMinimalCanonicalType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  func c<each d: a>(repeat each d)
