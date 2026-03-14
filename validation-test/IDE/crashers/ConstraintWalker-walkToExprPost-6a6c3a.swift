// {"kind":"complete","languageMode":6,"original":"18c8278f","signature":"(anonymous namespace)::ConstraintWalker::walkToExprPost(swift::Expr*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
func a
class b {
  c = {
    func async {
      for      in  #^^# {
        a
