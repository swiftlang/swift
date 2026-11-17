// {"kind":"typecheck","signature":"swift::TypeBase::getContextSubstitutionMap()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypeAliasType::getSubstitutionMap"}
// RUN: not %target-swift-frontend -typecheck %s
struct a {
  struct b<c> {
    typealias d<e> = a
  }
  extension b.d<Int> {
  }
}
