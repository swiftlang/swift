// {"kind":"typecheck","original":"2c7b09da","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"TypeBase::getMinimalCanonicalType"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  func c<each d: a>(repeat each d)
