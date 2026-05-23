// {"kind":"complete","original":"ad1d0fca","signature":"swift::constraints::ConstraintSystem::addConstraint(swift::Requirement, swift::constraints::ConstraintLocatorBuilder, bool, bool, swift::constraints::PreparedOverloadBuilder*)","stackOverflow":true}
// This test crashes by overflowing the stack, set a suitable timeout to ensure it doesn't take too long.
// RUN: not %{python} %swift_src_root/test/Inputs/timeout.py 60 \
// RUN:             %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s || \
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a { associatedtype b func c -> b }
struct d<e extension d: a { typealias b = d<d> }
extension d: Equatable where e: a, e.b: Equatable { == (f: d) { f#^^#== g
