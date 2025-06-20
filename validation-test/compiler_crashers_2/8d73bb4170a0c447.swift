// {"signature":"swift::ParamDecl::setTypeCheckedDefaultExpr(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(b= a(
