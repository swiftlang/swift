// {"kind":"typecheck","original":"fe9674ea","signature":"swift::TypeChecker::typeCheckCheckedCast(swift::Type, swift::Type, swift::CheckedCastContextKind, swift::DeclContext*)","signatureAssert":"Assertion failed: (!toType->isAny() && \"casts to 'Any' should've been handled above\"), function typeCheckCheckedCast","signatureNext":"TypeChecker::coercePatternToType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: ~Escapable {
  var b: a {
    switch b {
    case Any:
    }
  }
}
