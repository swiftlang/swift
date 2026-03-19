// {"kind":"typecheck","original":"1b1342ff","signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"TypeBase::getContextSubstitutionMap"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b {
  wrappedValue: b
  projectedValue: a
  init(projectedValue:
}
struct c<b, d {
    e(@a b  = e($f: g
