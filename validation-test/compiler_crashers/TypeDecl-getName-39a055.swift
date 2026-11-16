// {"kind":"typecheck","original":"88a87e44","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  struct b < c {
    extension {
      enum d {
        e(c)
