// {"kind":"typecheck","original":"db20da13","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (!hasAccess() && \"access already set\"), function setAccess"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  @b <#declaration#>c
}
extension a where b : <#type#> {
  struct b {
  }
}
