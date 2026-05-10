// {"kind":"complete","original":"69939d71","signature":"swift::constraints::ConstraintSystem::lookupDependentMember(swift::Type, swift::AssociatedTypeDecl*, bool, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::simplifyForEachElementConstraint"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b: SIMD
}
protocol c: Sequence {
  associatedtype d: a where d.b == SIMD2<Element>
}
func e(f: c) {
  for body  g  f {
    #^^#
