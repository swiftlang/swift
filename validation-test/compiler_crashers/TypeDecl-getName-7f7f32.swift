// {"kind":"typecheck","original":"b96aeed6","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  extension {
    c<each d>(repeat (each d)) where repeat each d : a
