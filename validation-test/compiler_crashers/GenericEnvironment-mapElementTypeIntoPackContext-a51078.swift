// {"kind":"typecheck","original":"09e5bde6","signature":"swift::GenericEnvironment::mapElementTypeIntoPackContext(swift::Type) const","signatureNext":"ConstraintSystem::simplifyPackElementOfConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c: repeat Sequence<each b>) {
  repeat (c.prefix(1))
}
