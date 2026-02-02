// {"kind":"complete","original":"018f6774","signature":"swift::TypeChecker::typeCheckForCodeCompletion(swift::constraints::SyntacticElementTarget&, bool, llvm::function_ref<void (swift::constraints::Solution const&)>)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a {
arr: ] func b {
return c
{
before#^^#? arr : }
<
