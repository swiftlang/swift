// {"kind":"complete","original":"4bb98688","signature":"swift::constraints::ConstraintSystem::getTypeOfMemberReferencePost(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocator*, swift::Type, swift::Type)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::resolveOverload"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a { #if{
b {}#^^#
}
func b<c
func b<c
