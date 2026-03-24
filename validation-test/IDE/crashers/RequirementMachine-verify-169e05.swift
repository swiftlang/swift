// {"kind":"complete","original":"b227805b","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const","signatureNext":"RequirementMachine::requiresClass"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
  c
  d
  e: b
}
func f<
  j,
  g
{
  struct h: a {
    typealias b = g
    i {
    h#^^#
