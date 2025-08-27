// {"kind":"typecheck","signature":"isParamListRepresentableInLanguage(swift::AbstractFunctionDecl const*, swift::ParameterList const*, swift::ObjCReason)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc b(_)
