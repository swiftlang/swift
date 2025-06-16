// {"signature":"swift::ExprPattern::updateMatchExpr(swift::Expr*) const::FindMatchOperatorDeclRef::walkToExprPre(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func ~= (a) if case ~= b
