// {"frontendArgs":["-typecheck","-enable-experimental-feature","KeyPathWithMethodMembers"],"kind":"custom","signature":"(anonymous namespace)::ExprRewriter::visitKeyPathExpr(swift::KeyPathExpr*)","signatureAssert":"Assertion failed: (objectTy), function visitKeyPathExpr","signatureNext":"ExprWalker::walkToExprPost"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck -enable-experimental-feature KeyPathWithMethodMembers %s
// REQUIRES: swift_feature_KeyPathWithMethodMembers
struct a<b {
  c(Int) -> b
  let d = \a<Int?>.c(1)?
