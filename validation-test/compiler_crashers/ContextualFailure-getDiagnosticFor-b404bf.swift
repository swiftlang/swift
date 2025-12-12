// {"kind":"typecheck","signature":"swift::constraints::ContextualFailure::getDiagnosticFor(swift::ContextualTypePurpose, swift::Type)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{ struct a func b< c >->c->() var : (inout a)->() = b(
