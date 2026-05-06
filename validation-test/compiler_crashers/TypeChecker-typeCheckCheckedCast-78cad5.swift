// {"kind":"typecheck","original":"800deb95","signature":"swift::TypeChecker::typeCheckCheckedCast(swift::Type, swift::Type, swift::CheckedCastContextKind, swift::DeclContext*)","signatureAssert":"Assertion failed: (!toType->isAny() && \"casts to 'Any' should've been handled above\"), function typeCheckCheckedCast","signatureNext":"ExprRewriter::handleConditionalCheckedCastExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b>: ~Escapable {
  var c: b
}
func d() {
  a(c: d) as? Any
}
