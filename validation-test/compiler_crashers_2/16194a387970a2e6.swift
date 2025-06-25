// {"signature":"swift::TuplePattern::createSimple(swift::ASTContext&, swift::SourceLoc, llvm::ArrayRef<swift::TuplePatternElt>, swift::SourceLoc)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch { case (repeat a
