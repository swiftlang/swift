// {"kind":"typecheck","languageMode":6,"original":"35c1301e","signature":"swift::Parser::consumeTokenWithoutFeedingReceiver()","signatureAssert":"Assertion failed: (Tok.isNot(tok::eof) && \"Lexing past eof!\"), function discardToken","signatureNext":"Parser::parseStringSegments"}
// RUN: not --crash %target-swift-frontend -typecheck -swift-version 6 %s
"ch\(/ ch)"
