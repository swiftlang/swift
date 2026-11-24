// {"kind":"typecheck","original":"b96aeed6","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  extension {
    c<each d>(repeat (each d)) where repeat each d : a
