// {"kind":"typecheck","signature":"swift::ProtocolRequiresClassRequest::diagnoseCycle(swift::DiagnosticEngine&) const","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct b let b c a = b protocol a : a
