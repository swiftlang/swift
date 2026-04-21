// {"kind":"complete","original":"d321f640","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const","signatureNext":"RequirementMachine::requiresProtocol"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<b, c: ~Copyable
  ~
  extension a: Copyable {
    protocol d {
      e
    }
    struct f: d {
      #^^#
