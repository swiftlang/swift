// {"kind":"typecheck","signature":"swift::Parser::parseStmtConditionElement(llvm::SmallVectorImpl<swift::StmtConditionElement>&, swift::Diag<>, swift::StmtKind, llvm::StringRef&)","signatureAssert":"Assertion failed: (CurDeclContext->isLocalContext() && \"conditional binding in non-local context?!\"), function parseStmtConditionElement"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a: b[ if let
