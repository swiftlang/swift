// {"kind":"typecheck","signature":"swift::ExprPattern::updateMatchExpr(swift::Expr*) const::FindMatchOperatorDeclRef::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (fnTy->getParams().size() == 2), function walkToExprPre","signatureNext":"Traversal::doIt"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func ~= (a) if case ~= b
