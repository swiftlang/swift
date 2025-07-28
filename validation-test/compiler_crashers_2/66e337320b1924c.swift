// {"kind":"typecheck","signature":"swift::ProtocolRequiresClassRequest::diagnoseCycle(swift::DiagnosticEngine&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct b let b c a = b protocol a : a
