// {"kind":"typecheck","original":"a275ad03","signature":"swift::Parser::parseTypeInlineArray(swift::SourceLoc)","signatureAssert":"Assertion failed: (Tok.is(K) && \"Consuming wrong token kind\"), function consumeToken"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[a & 3 of
