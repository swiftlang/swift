// {"kind":"typecheck","signature":"swift::GenericEnvironment::getMappingIfPresent(swift::CanType) const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"GenericEnvironment::getOrCreateArchetypeFromInterfaceType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b
  typealias c<b, d> = a<d>
  extension c<Int, Float> {  e: c
