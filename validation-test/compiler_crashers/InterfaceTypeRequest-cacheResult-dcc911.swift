// {"frontendArgs":["-typecheck","-enable-experimental-feature","KeyPathWithMethodMembers"],"kind":"custom","signature":"swift::InterfaceTypeRequest::cacheResult(swift::Type) const","signatureAssert":"Assertion failed: (!type->hasTypeVariable() && \"Type variable in interface type\"), function cacheResult","signatureNext":"ValueDecl::setInterfaceType"}
// RUN: not --crash %target-swift-frontend -typecheck -enable-experimental-feature KeyPathWithMethodMembers %s
// REQUIRES: swift_feature_KeyPathWithMethodMembers
struct a < b {
  c(Int) -> b
  let : a< Int > -> Int = \.c(1
