// {"kind":"complete","original":"a705fa0d","signature":"swift::constraints::Solution::simplifyType(swift::Type, bool) const","signatureAssert":"Assertion failed: (!resolvedType->getRecursiveProperties().isSolverAllocated()), function simplifyType"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  $1 {
  }(
    #^^#)
}
