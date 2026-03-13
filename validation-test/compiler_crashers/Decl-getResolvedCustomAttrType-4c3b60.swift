// {"kind":"typecheck","original":"000f0a50","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (!isSpecial() && \"Cannot retrieve identifier from special names\"), function getIdentifier"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(peer prefixed(a)) macro b
struct c {
  @d config = @b
  init
