// {"kind":"typecheck","signature":"swift::Parser::consumeTokenWithoutFeedingReceiver()","signatureAssert":"Assertion failed: (Tok.isNot(tok::eof) && \"Lexing past eof!\"), function discardToken"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@_extern(a
