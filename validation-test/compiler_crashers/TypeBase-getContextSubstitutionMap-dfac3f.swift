// {"kind":"typecheck","original":"666d963b","signature":"swift::TypeBase::getContextSubstitutionMap()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConcreteContraction::substTypeParameterRec","useGuardMalloc":true}
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: no_asan
// REQUIRES: target-same-as-host
protocol a {
  associatedtype b
}
struct c < d > {
  typealias e = <#type#>
}
struct f {
  typealias g = c
  struct h < i : a where i.b == f > {
j where i.b.g.e ==
  }
}
