// {"kind":"typecheck","original":"f7872afc","signature":"swift::ParamDecl::setTypeCheckedDefaultExpr(swift::Expr*)","signatureAssert":"Assertion failed: (E || getDefaultArgumentKind() == DefaultArgumentKind::Inherited), function setTypeCheckedDefaultExpr"}
// RUN: not %target-swift-frontend -typecheck %s
struct a {
  init<b>(b  = a(
