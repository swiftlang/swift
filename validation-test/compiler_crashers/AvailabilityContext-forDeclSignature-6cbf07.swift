// {"kind":"typecheck","signature":"swift::AvailabilityContext::forDeclSignature(swift::Decl const*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@_typeEraser(a) protocol b dynamic func c->some b {
  d
