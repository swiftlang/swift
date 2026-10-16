// {"frontendArgs":["-typecheck","-enable-experimental-feature","KeyPathWithMethodMembers"],"kind":"custom","signature":"swift::Expr::setType(swift::Type)","signatureAssert":"Assertion failed: (isa<TypeJoinExpr>(this) || !T->hasTypeVariableOrHole()), function setType","signatureNext":"ExprRewriter::visitKeyPathExpr"}
// RUN: not --crash %target-swift-frontend -typecheck -enable-experimental-feature KeyPathWithMethodMembers %s
// REQUIRES: swift_feature_KeyPathWithMethodMembers
struct a < b {
  c(Int) -> b
  let : a< Int > -> Int = \.c(1
