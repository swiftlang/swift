// {"kind":"typecheck","signature":"(anonymous namespace)::TypeResolver::resolveImplicitlyUnwrappedOptionalType(swift::ImplicitlyUnwrappedOptionalTypeRepr*, swift::TypeResolutionOptions, bool)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  enum a: b! {
  }
  guard a else {
  }
}
