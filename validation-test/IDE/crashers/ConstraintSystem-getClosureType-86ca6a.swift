// {"kind":"complete","original":"8a598cc4","signature":"swift::constraints::ConstraintSystem::getClosureType(swift::ClosureExpr const*) const","signatureAssert":"Assertion failed: (result), function getClosureType","signatureNext":"TypeVarRefCollector::walkToStmtPre"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  do {
    a +=
      if <#expression#> {
        return
      }
  } catch {
    #^^#
  }
}
