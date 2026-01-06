// {"kind":"complete","original":"773d9dd1","signature":"swift::ProtocolConformance::getTypeWitnessAndDecl(swift::AssociatedTypeDecl*, swift::SubstOptions) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@_marker protocol a {
  b:
  #^^#  typealias  5
