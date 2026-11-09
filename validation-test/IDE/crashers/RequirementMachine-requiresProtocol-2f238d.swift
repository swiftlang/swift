// {"kind":"complete","original":"9d67d338","signature":"swift::rewriting::RequirementMachine::requiresProtocol(swift::Type, swift::ProtocolDecl const*) const","signatureNext":"Mangle::ASTMangler::gatherGenericSignatureParts"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol b {
  associatedtype c
}
protocol d<a> {
  associatedtype a: b
  associatedtype e: d where e.a == a.c
  extension d {
    struct e<h> {
      g { #^^#
          }
    }
  }
