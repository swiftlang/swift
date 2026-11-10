// {"kind":"typecheck","original":"a275ad03","signature":"swift::Parser::parseTypeInlineArray(swift::SourceLoc)","signatureAssert":"Assertion failed: (Tok.is(K) && \"Consuming wrong token kind\"), function consumeToken","signatureNext":"Parser::parseExprCollection"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
[a & 3 of
