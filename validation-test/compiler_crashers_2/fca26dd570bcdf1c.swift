// {"kind":"typecheck","signature":"(anonymous namespace)::TypeResolver::resolveVarargType(swift::VarargTypeRepr*, swift::TypeResolutionOptions)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc b : Int...
