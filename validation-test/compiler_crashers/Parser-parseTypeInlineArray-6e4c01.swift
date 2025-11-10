// {"kind":"typecheck","original":"58cef9f3","signature":"swift::Parser::parseTypeInlineArray(swift::SourceLoc)","signatureAssert":"Assertion failed: (getPtrOrNull() && \"not checked for nullptr\"), function get"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[[3] of
