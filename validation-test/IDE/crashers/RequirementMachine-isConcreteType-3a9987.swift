// {"kind":"complete","original":"d4d90479","signature":"swift::rewriting::RequirementMachine::isConcreteType(swift::Type, swift::ProtocolDecl const*) const","signatureNext":"CompletionOverrideLookup::getOpaqueResultType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
}
protocol c {
  associatedtype d
  func e() -> d.b where d: a
  struct f: c {
    #^^#
  }
}
