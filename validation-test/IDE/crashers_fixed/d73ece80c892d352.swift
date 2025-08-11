// {"kind":"complete","original":"00db48f7","signature":"swift::TypeChecker::typeCheckForCodeCompletion(swift::constraints::SyntacticElementTarget&, bool, llvm::function_ref<void (swift::constraints::Solution const&)>)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a {
  arr: ]  func b() {
    return c {
      (#^^# ? arr : <#expression#>)
    }
      < <#expression#>
  }
}
