// {"kind":"complete","original":"6384b7fc","signature":"swift::TypeChecker::typeCheckForCodeCompletion(swift::constraints::SyntacticElementTarget&, bool, llvm::function_ref<void (swift::constraints::Solution const&)>)","signatureAssert":"Assertion failed: (fallback->E != expr), function typeCheckForCodeCompletion"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a -> b
a > (c > a #^^# )'
