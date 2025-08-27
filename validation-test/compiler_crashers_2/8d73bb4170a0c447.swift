// {"kind":"typecheck","signature":"swift::ParamDecl::setTypeCheckedDefaultExpr(swift::Expr*)","signatureAssert":"Assertion failed: (E || getDefaultArgumentKind() == DefaultArgumentKind::Inherited), function setTypeCheckedDefaultExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(b= a(
