// {"kind":"typecheck","signature":"swift::TypeChecker::typeCheckCheckedCast(swift::Type, swift::Type, swift::CheckedCastContextKind, swift::DeclContext*)","signatureAssert":"Assertion failed: (!toType->isAny() && \"casts to 'Any' should've been handled above\"), function typeCheckCheckedCast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
init(a : () -> ()) {
  [.init ?? a]
}
