// {"kind":"typecheck","signature":"swift::PatternTypeRequest::evaluate(swift::Evaluator&, swift::ContextualPattern) const","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objcMembers class a open extension a {
  var b
