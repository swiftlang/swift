// {"signature":"swift::constraints::ContextualFailure::getDiagnosticFor(swift::ContextualTypePurpose, swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{ struct a func b< c >->c->() var : (inout a)->() = b(
