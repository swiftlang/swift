// {"signature":"swift::Parser::parseStmtConditionElement(llvm::SmallVectorImpl<swift::StmtConditionElement>&, swift::Diag<>, swift::StmtKind, llvm::StringRef&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
     func a: b[        if let
