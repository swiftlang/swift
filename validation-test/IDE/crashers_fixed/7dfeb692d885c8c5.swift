// {"kind":"complete","original":"0bd7af1f","signature":"swift::constraints::TypeVarRefCollector::walkToStmtPre(swift::Stmt*)","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  let a =
    if <#expression#> {
      return
    }
  return #^^#
}
