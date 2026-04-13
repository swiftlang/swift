// {"kind":"complete","original":"72909145","signature":"swift::constraints::ConstraintSystem::resolveOverload(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocator*, swift::Type, swift::constraints::PreparedOverload*)","signatureAssert":"Assertion failed: (!declRefType.referenceType->hasTypeParameter() && \"Cannot have a dependent type here\"), function resolveOverload","signatureNext":"ConstraintSystem::simplifyConstraint"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b> {
  c { struct d { typealias e = a typealias e = e func f { e #^^#
  }
