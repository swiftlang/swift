// {"kind":"typecheck","signature":"swift::TypeBase::getContextSubstitutionMap()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  typealias c<d> = b where b == Int
}
func e(): a.c<Int>
