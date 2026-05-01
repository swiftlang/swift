// {"kind":"typecheck","original":"9ab0a972","signature":"swift::constraints::ConstraintSystem::openOpaqueType(swift::OpaqueTypeArchetypeType*, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
typealias c<d: a> = (d, d.b)
func e() -> c<some a> {
  f
}
