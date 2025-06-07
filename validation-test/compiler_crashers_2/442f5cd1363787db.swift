// {"signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
struct a {
  @b c, () {
