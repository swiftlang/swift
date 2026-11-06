// {"kind":"typecheck","original":"134d8fc1","signature":"swift::Parser::parseCustomAttribute(swift::SourceLoc)","signatureAssert":"Assertion failed: (Tok.is(tok::identifier)), function parseCustomAttribute","signatureNext":"Parser::parseDeclAttribute"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@ @ ::
