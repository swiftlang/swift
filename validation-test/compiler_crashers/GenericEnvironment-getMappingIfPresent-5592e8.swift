// {"kind":"typecheck","signature":"swift::GenericEnvironment::getMappingIfPresent(swift::CanType) const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  struct b < each c {
    extension a {
      struct d {
        e(repeat(each c))
