// {"signature":"swift::findSyntacticErrorForConsume(swift::ModuleDecl*, swift::SourceLoc, swift::Expr*, llvm::function_ref<swift::Type (swift::Expr*)>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(consume : String) {
  consume consume consume
