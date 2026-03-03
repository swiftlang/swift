// {"kind":"complete","original":"b8da5db5","signature":"swift::rewriting::RequirementMachine::requiresClass(swift::Type) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension Slice {
a {
  #^^#
