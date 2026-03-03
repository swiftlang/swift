// {"kind":"typecheck","signature":"swift::ExprPattern::updateMatchExpr(swift::Expr*) const::FindMatchOperatorDeclRef::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (fnTy->getParams().size() == 2), function walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func ~= (a) if case ~= b
