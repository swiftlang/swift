// {"kind":"complete","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a<b{ c { struct d<f where f.e#^COMPLETE^#
