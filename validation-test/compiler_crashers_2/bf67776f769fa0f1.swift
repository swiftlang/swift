// {"kind":"typecheck","signature":"(anonymous namespace)::TypeResolver::resolveType(swift::TypeRepr*, swift::TypeResolutionOptions)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc b : _
