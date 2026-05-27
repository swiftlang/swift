// {"kind":"typecheck","original":"a037820a","signature":"swift::TypeChecker::typeCheckCheckedCast(swift::Type, swift::Type, swift::CheckedCastContextKind, swift::DeclContext*)","signatureAssert":"Assertion failed: (!toType->isAny() && \"casts to 'Any' should've been handled above\"), function typeCheckCheckedCast","signatureNext":"maybeWarnAboutExtraneousCast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
var <#pattern#>: (() -> Any) -> Any = { a in
  a as! Any
}
