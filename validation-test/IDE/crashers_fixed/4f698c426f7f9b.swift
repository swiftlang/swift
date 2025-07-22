// {"kind":"complete","original":"03d6b22e","signature":"swift::TypeChecker::typeCheckForCodeCompletion(swift::constraints::SyntacticElementTarget&, bool, llvm::function_ref<void (swift::constraints::Solution const&)>)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a->Int { ((0 + 0 + 0 + 0 4) << 1)#^^# <<
