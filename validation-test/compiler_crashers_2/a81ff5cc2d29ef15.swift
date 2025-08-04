// {"kind":"typecheck","signature":"swift::IsStaticRequest::evaluate(swift::Evaluator&, swift::FuncDecl*) const","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objcMembers class a open extension a {
  ...
