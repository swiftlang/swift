// {"kind":"typecheck","original":"e0ee2f06","signature":"swift::Parser::consumeTokenWithoutFeedingReceiver()","signatureAssert":"Assertion failed: (Tok.isNot(tok::eof) && \"Lexing past eof!\"), function discardToken","signatureNext":"Parser::isNextStartOfSwiftDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  private(
