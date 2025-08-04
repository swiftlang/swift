// {"kind":"typecheck","original":"36a5bbda","signature":"swift::SuperclassDeclRequest::diagnoseCycle(swift::DiagnosticEngine&) const","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  class a: a {
  }
  if a {
  }
}
