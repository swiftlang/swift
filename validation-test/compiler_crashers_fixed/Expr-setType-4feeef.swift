// {"kind":"typecheck","signature":"swift::Expr::setType(swift::Type)","signatureAssert":"Assertion failed: (!T->hasPlaceholder()), function setType","signatureNext":"ResolvePattern::visitUnresolvedDotExpr"}
// RUN: not %target-swift-frontend -typecheck %s
enum a<b {
  case c
}
do
catch a<_>.c
