// {"kind":"typecheck","original":"f3540b78","signature":"swift::constraints::CheckedCastToUnrelatedFailure::CheckedCastToUnrelatedFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::CheckedCastKind, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"AllowCheckedCastToUnrelated::diagnose"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a
  if
  let b
  as
  a = c as Any.Type
