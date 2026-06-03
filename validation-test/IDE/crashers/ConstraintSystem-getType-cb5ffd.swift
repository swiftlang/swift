// {"kind":"complete","original":"15751a3c","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"repairViaOptionalUnwrap"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
var <#pattern#>: <#type#> {
  sequence(
    #^^# {
      a?
    })
}
