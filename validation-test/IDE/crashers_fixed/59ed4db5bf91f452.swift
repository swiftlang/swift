// {"kind":"complete","original":"089853e1","signature":"swift::TypeChecker::typeCheckForCodeCompletion(swift::constraints::SyntacticElementTarget&, bool, llvm::function_ref<void (swift::constraints::Solution const&)>)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a {
b:
func c -> UInt32 {
0 ^ b#^^#
