// {"kind":"complete","original":"21e62752","signature":"swift::InFlightSubstitution::projectLaneFromPackType(swift::Type, unsigned int)","signatureAssert":"Assertion failed: (substEltType->is<PackExpansionType>() && \"substituted shape mismatch: expected an expansion component\"), function projectLaneFromPackType","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<each b> {
  #^^#  typealias c<each d> = (repeat (each b) -> each d) where repeat c<each d> == <#type#>
  =
}
