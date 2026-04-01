// {"kind":"complete","original":"6bb90e32","signature":"swift::constraints::ConstraintSystem::getClosureType(swift::ClosureExpr const*) const","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  let a =
    if .b + .else {
      return
    }
  #^^#
}
