// {"kind":"typecheck","original":"302e8309","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a < e
  struct b < each c {
    @globalActor actor d {
      static shared = @d
      extension a
