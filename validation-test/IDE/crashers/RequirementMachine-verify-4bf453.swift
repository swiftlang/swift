// {"kind":"complete","original":"634f36d5","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const","signatureNext":"RequirementMachine::isConcreteType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b
  extension a: c  [ {
    var d: some BidirectionalCollection<b> {
      #^^#
