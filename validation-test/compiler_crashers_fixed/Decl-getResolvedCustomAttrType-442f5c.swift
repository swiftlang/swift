// {"kind":"typecheck","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const"}
// RUN: not %target-swift-frontend -typecheck %s
struct a {
  @b c, () {
