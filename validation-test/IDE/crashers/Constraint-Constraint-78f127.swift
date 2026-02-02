// {"kind":"complete","signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::SmallPtrSetImpl<swift::TypeVariableType*>&)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a {
wrappedValue: Bool init(wrappedValue: Bool
var projectedValue init(projectedValue: ) func b(@a ) {
b#^^#
