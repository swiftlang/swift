// {"kind":"typecheck","signature":"swift::Evaluator::diagnoseCycle(swift::ActiveRequest const&)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a
  let _ b = a
  class b : b.c
