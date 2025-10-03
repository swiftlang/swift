// {"kind":"complete","original":"098f0ede","signature":"swift::constraints::ConstraintSystem::getMemberReferenceTypeFromOpenedType(swift::Type, swift::Type, swift::ValueDecl*, swift::constraints::ConstraintLocator*, bool, bool)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<b> {
  c=d#^^#  macro d()
}
