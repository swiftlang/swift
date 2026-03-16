// {"kind":"typecheck","signature":"swift::Parser::parseTypeSimple(swift::Diag<>, swift::Parser::ParseTypeReason)","signatureAssert":"Assertion failed: (getPtrOrNull() && \"not checked for nullptr\"), function get","signatureNext":"Parser::parseTypeSimpleOrComposition"}
// RUN: not --crash %target-swift-frontend -typecheck %s
subscript(a: ~b <
