// {"kind":"complete","original":"ac690622","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
associatedtype b
func c -> b
struct d<e: Collection, f
>: a
where e.Element ==
{
c -> some a {
self#^^#
