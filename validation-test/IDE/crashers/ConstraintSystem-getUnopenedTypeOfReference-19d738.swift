// {"kind":"complete","original":"70ffd3a1","signature":"swift::constraints::ConstraintSystem::getUnopenedTypeOfReference(swift::VarDecl*, swift::Type, swift::DeclContext*, swift::constraints::ConstraintLocator*, bool)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a<b>(c: b) = {
macro d =
c#^^#
