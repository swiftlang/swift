// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::addConstraintImpl(swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocatorBuilder, bool)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a {
wrappedValue: Bool init(wrappedValue: Bool
var projectedValue init(projectedValue: ) func b(@a ) {
b#^^#
