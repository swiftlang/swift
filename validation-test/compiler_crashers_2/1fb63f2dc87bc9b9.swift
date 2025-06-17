// {"signature":"swift::Parser::parseDecl(bool, bool, llvm::function_ref<void (swift::Decl*)>, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a { class override ( override
