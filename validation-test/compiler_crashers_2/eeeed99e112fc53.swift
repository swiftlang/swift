// {"kind":"typecheck","signature":"swift::Parser::parseTypeAttribute(llvm::PointerUnion<swift::CustomAttr*, swift::TypeAttribute*>&, swift::SourceLoc, swift::SourceLoc, swift::Parser::ParseTypeReason, bool)","signatureAssert":"Assertion failed: (Tok.is(K) && \"Consuming wrong token kind\"), function consumeToken"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@opened)
