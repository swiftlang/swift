// {"frontendArgs":["-typecheck","-enable-experimental-feature","KeyPathWithMethodMembers"],"kind":"custom","signature":"swift::Expr::setType(swift::Type)","signatureAssert":"Assertion failed: (isa<TypeJoinExpr>(this) || !T->hasTypeVariableOrHole()), function setType","signatureNext":"ExprRewriter::visitKeyPathExpr"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck -enable-experimental-feature KeyPathWithMethodMembers %s
// REQUIRES: swift_feature_KeyPathWithMethodMembers
struct a < b {
  c(Int) -> b
  let : a< Int > -> Int = \.c(1
