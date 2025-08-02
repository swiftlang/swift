// {"kind":"typecheck","signature":"swift::Parser::parseList(swift::tok, swift::SourceLoc, swift::SourceLoc&, bool, swift::DiagRef, llvm::function_ref<swift::ParserStatus ()>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(conformances: ,
